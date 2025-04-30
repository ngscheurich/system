(local color-overrides {:mocha {:rosewater "#ba9e5b"
                                :flamingo "#f15f22"
                                :pink "#ae4035"
                                :mauve "#ba9e5b"
                                :red "#d74200"
                                :maroon "#ae4035"
                                :peach "#e99f10"
                                :yellow "#cfc041"
                                :green "#489f4e"
                                :teal "#47afca"
                                :sky "#3ddccb"
                                :sapphire "#f15f22"
                                :blue "#0096ff"
                                :lavender "#77bfcf"
                                :text "#ffffff"
                                :subtext1 "#e7eaeb"
                                :subtext0 "#b7c1c2"
                                :overlay2 "#9fadae"
                                :overlay1 "#87989a"
                                :overlay0 "#6e8385"
                                :surface2 "#566f71"
                                :surface1 "#3e5a5d"
                                :surface0 "#264648"
                                :base "#0e3134"
                                :mantle "#0A1D1E"
                                :crust "#041211"}})

{:apply (fn [cb]
          (cb {:color_overrides color-overrides
               :integrations {:aerial true :blink_cmp true}})
          (vim.cmd.colorscheme :catppuccin)
          (vim.cmd "hi FloatBorder guifg=#b7c1c2")
          (vim.cmd "hi link SnacksPickerListCursorLine CursorLine"))}
