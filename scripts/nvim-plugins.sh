#!/usr/bin/env elixir

defmodule NvimPlugins do
  @moduledoc """
  Adds plugin information to my Neovim README.
  """

  @xdg_config_home System.get_env("XDG_CONFIG_HOME")
  @nvim_config_path "#{@xdg_config_home}/nvim"
  @readme_path "#{@nvim_config_path}/README.md"
  @specs_path "#{@nvim_config_path}/lua/ngs/specs"
  @metadata_line_count 7
  @marker_start "<!-- nvim-plugins:start -->"
  @marker_end "<!-- nvim-plugins:end -->"

  def run do
    file_paths = get_files_recursively(@specs_path)

    for path <- file_paths do
      path
      |> File.read!()
      |> String.split("\n")
      |> extract_metadata([])
    end
    |> List.flatten()
    |> write_to_readme!()
  end

  defp get_files_recursively(dir) do
    dir
    |> Path.expand()
    |> get_files([])
  end

  defp get_files(path, acc) do
    case File.ls(path) do
      {:ok, entries} ->
        Enum.reduce(entries, acc, fn entry, acc ->
          full_path = Path.join(path, entry)

          case File.stat(full_path) do
            {:ok, %File.Stat{type: :directory}} ->
              get_files(full_path, acc)

            {:ok, %File.Stat{type: :regular}} ->
              [full_path | acc]

            _ ->
              acc
          end
        end)

      _ ->
        acc
    end
  end

  defp extract_metadata([], metadata) do
    metadata
  end

  defp extract_metadata([line | lines], metadata) do
    {lines, metadata} =
      if Regex.match?(~r/^\s{0,}-- ==/, line) do
        {[name, _, url, info, tags, _], lines} = Enum.split(lines, @metadata_line_count - 1)

        plugin_metadata = %{
          name: parse_metadata_line(name),
          url: parse_metadata_line(url),
          info: parse_metadata_line(info),
          tags: tags |> parse_metadata_line |> String.split(", ")
        }

        {lines, [plugin_metadata | metadata]}
      else
        {lines, metadata}
      end

    extract_metadata(lines, metadata)
  end

  defp parse_metadata_line(line) do
    <<_::bytes-size(7)>> <> str = String.trim_leading(line)
    str
  end

  defp write_to_readme!(metadata) do
    readme_contents = File.read!(@readme_path)
    [pre_content, rest_content] = String.split(readme_contents, "\n" <> @marker_start)

    post_content =
      case String.split(rest_content, @marker_end <> "\n") do
        [_, content] -> content
        _ -> ""
      end

    generated_content =
      metadata
      |> Enum.group_by(&List.first(&1.tags))
      |> Enum.into([], fn {category, plugins} ->
        lines =
          plugins
          |> Enum.map(&"- [#{&1.name}](#{&1.url}): #{&1.info}")
          |> Enum.sort()
          |> Kernel.++([""])

        ["### #{humanize(category)}\n" | lines]
      end)
      |> List.flatten()
      |> Enum.join("\n")

    contents =
      """
      #{pre_content}
      #{@marker_start}

      #{generated_content}
      #{@marker_end}
      #{post_content}
      """
      |> String.trim_trailing("\n")

    File.write!(@readme_path, contents)
  end

  defp humanize("misc"), do: "Miscellaneous"
  defp humanize("tree-sitter"), do: "Tree-Sitter"
  defp humanize("ui"), do: "User Interface"

  defp humanize(str) do
    str
    |> String.split("-")
    |> Enum.map(&capitalize/1)
    |> Enum.join(" ")
  end

  defp capitalize("ai"), do: "AI"
  defp capitalize(str), do: String.capitalize(str)
end

NvimPlugins.run()

# vi:ft=elixir
