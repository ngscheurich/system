(local feline  (require "feline"))
(local vi-mode (require "feline.providers.vi_mode"))

(set vim.o.termguicolors true)

(local colors _G.theme.statusbar)

(local mode-hl (fn [] {:name (vi-mode.get_mode_highlight_name)
                       :fg colors.bg
                       :bg (vi-mode.get_mode_color)
                       :style :bold}))

(local mode-hl-rev (fn [] {:name (vi-mode.get_mode_highlight_name)
                           :fg (vi-mode.get_mode_color)
                           :bg colors.bg}))

(local left [{:provider :vi_mode
              :short_provider " "
              :icon ""
              :hl mode-hl
              :left_sep {:str " " :hl mode-hl}
              :right_sep {:str " " :hl mode-hl}}

             {:provider :file_info
              :type :short-path
              :file_readonly_icon " "
              :hl {:bg colors.bg_light}
              :left_sep {:str " " :hl {:bg colors.bg_light}}
              :right_sep {:str " " :hl {:bg colors.bg_light}}}

             {:provider " "}

             {:provider :git_branch
              :truncate_hide true
              :priority 2
              :hl {:fg colors.fg_dark}
              :right_sep " "}

             {:provider :git_diff_added
              :truncate_hide true
              :icon "+"
              :hl {:fg colors.green}
              :right_sep " "}

             {:provider :git_diff_changed
              :truncate_hide true
              :icon "~"
              :hl {:fg colors.blue}
              :right_sep " "}

             {:provider :git_diff_removed
              :truncate_hide true
              :icon "-"
              :hl {:fg colors.red}
              :right_sep " "}])

(local right [{:provider :diagnostic_errors
               :truncate_hide true
               :priority 1
               :icon " "
               :hl {:fg colors.red}
               :right_sep " "}

              {:provider :diagnostic_warnings
               :truncate_hide true
               :priority 1
               :icon " "
               :hl {:fg colors.yellow}
               :right_sep " "}

              {:provider :diagnostic_hints
               :truncate_hide true
               :priority 1
               :icon " "
               :hl {:fg colors.blue}
               :right_sep " "}

              {:provider :diagnostic_info
               :truncate_hide true
               :priority 1
               :icon " "
               :hl {:fg colors.blue}
               :right_sep " "}

              {:provider :lsp_client_names
               :truncate_hide true
               :priority 1
               :hl {:fg colors.fg_dark}
               :right_sep " "}

              {:provider (fn [] (.. " " (vim.api.nvim_buf_get_option 0 :filetype)))
               :hl {:fg colors.base04 :bg colors.bg_light}
               :left_sep {:str " " :hl {:fg colors.bg_light}}
               :right_sep {:str " " :hl {:bg colors.bg_light}}}

              {:provider :line_percentage
               :hl {:fg colors.bg_light :bg colors.fg_dark}
               :left_sep {:str " " :hl {:bg colors.fg_dark}}
               :right_sep {:str "   " :hl {:fg colors.bg_light :bg colors.fg_dark}}}

              {:provider (fn [] (let [(row) (unpack (vim.api.nvim_win_get_cursor 0))]
                                  (string.format "%3d" row)))
               :hl {:fg colors.bg_light :bg colors.fg_dark}}

              {:provider :: :hl {:fg colors.bg_light :bg colors.fg_dark}}

              {:provider (fn [] (let [(_ col) (unpack (vim.api.nvim_win_get_cursor 0))]
                                  (string.format "%-3d" (+ col 1))))
               :hl {:fg colors.bg_light :bg colors.fg_dark}}])

(local active [left right])

(local inactive [[{:provider :file_info} :hl {:fg colors.base04}]])

(local components {:active active :inactive inactive})

(local mode-colors {:NORMAL    colors.blue
                    :OP        colors.blue
                    :INSERT    colors.green
                    :VISUAL    colors.yellow
                    :LINES     colors.yellow
                    :BLOCK     colors.yellow
                    :REPLACE   colors.red
                    :V-REPLACE colors.red
                    :ENTER     colors.fg_dark
                    :MORE      colors.fg_dark
                    :SELECT    colors.fg_dark
                    :COMMAND   colors.fg_dark
                    :SHELL     colors.fg_dark
                    :TERM      colors.fg_dark
                    :NONE      colors.fg_dark})

(feline.setup {:components components
               :vi_mode_colors mode-colors
               :theme {:fg colors.fg
                       :bg colors.bg}})