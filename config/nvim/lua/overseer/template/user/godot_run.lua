return {
  name = "Run Godot Project",
  builder = function()
    return {
      cmd = {
        "/Applications/Godot.app/Contents/MacOS/Godot",
        "--path",
        "/Users/nick/Projects/AdventureGame",
        "game.tscn",
      },
      components = { "default" },
    }
  end,
  condition = {
    filetype = { "gdscript" },
  },
}
