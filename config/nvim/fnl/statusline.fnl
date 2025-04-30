(local {: get-in : merge : reduce : some} (require :nfnl.core))
(local {: get_palette} (require :catppuccin.palettes))
(local heirline (require :heirline))
(local conds (require :heirline.conditions))
(local utils (require :heirline.utils))

(fn catppuccin-color [name dim]
  "Get a named color from the Catppuccin mocha palette. Use `dim` to blend the
  color with black."
  (let [color (. (get_palette :mocha) name)]
    (Snacks.util.blend "#000000" color (or dim 0))))

(fn get-hl-attr [hl-group attr]
  "Get the `attr` of the given Neovim highlight group."
  (let [hl-id (vim.fn.hlID hl-group)]
    (vim.fn.synIDattr hl-id attr)))

(fn hl [fg bg]
  "Get a component `hl` table with the given `fg` and `bg` Catppuccin colors."
  {:fg (catppuccin-color (or fg :text)) :bg (catppuccin-color (or bg :mantle))})

(fn get-diagnostic-sign [severity]
  "Gets the configured diagnostic sign for the given `SEVERITY`."
  (get-in (vim.diagnostic.config) [:signs :text severity]))

(fn get-diagnostic-count [severity]
  (length (vim.diagnostic.get 0 {: severity})))

(fn get-mode-opts [mode]
  "Get a table of options related to the given `mode`."
  (let [list [{:modes [:n :niI :niR :niV :nt :nT]
               :name :NORMAL
               :color :blue
               :icon " "}
              {:modes [:no :nov :noV "no\022"]
               :name :NORMAL
               :color :pink
               :icon "󱦟 "}
              {:modes [:v :vs :V :Vs] :name :VISUAL :color :pink :icon "󰒅 "}
              {:modes ["\022" "\022s"]
               :name :VISUAL
               :color :pink
               :icon "󰩬 "}
              {:modes [:s :S "\019"]
               :name :SELECT
               :color :rosewater
               :icon "󰫙 "}
              {:modes [:i :ic :ix] :name :INSERT :color :green :icon " "}
              {:modes [:R :Rc :Rx :Rv :Rvc :Rvx]
               :name :REPLACE
               :color :red
               :icon " "}
              {:modes [:c] :name :COMMAND :color :flamingo :icon " "}
              {:modes [:cv] :name :EX :color :flamingo :icon " "}
              {:modes [:r :rm :r? "!"]
               :name "..."
               :color :lavender
               :icon "󰆅 "}
              {:modes [:t] :name :TERMINAL :color :sapphire :icon " "}]]
    (var mode-opts nil)
    (each [_ t (ipairs list) &until (not= mode-opts nil)]
      (when (or (some #(= $ mode) t.modes) false)
        (set mode-opts t)))
    mode-opts))

(local mode-init #(tset $1 :mode (vim.fn.mode 1)))
(local mode-update
       {1 :ModeChanged
        :pattern "*:*"
        :callback (vim.schedule_wrap #(vim.cmd :redrawstatus))})

(local mode-bar {:init mode-init
                 :update mode-update
                 1 {:provider #(string.format " %s "
                                              (. (get-mode-opts $1.mode) :icon))
                    :hl (fn [self]
                          {:fg (catppuccin-color :base)
                           :bg (let [{: color} (get-mode-opts self.mode)]
                                 (catppuccin-color color 0.2))})}
                 2 {:provider #(string.format " %s "
                                              (. (get-mode-opts $1.mode) :name))
                    :hl (fn [self]
                          {:fg (catppuccin-color :base)
                           :bg (let [{: color} (get-mode-opts self.mode)]
                                 (catppuccin-color color))
                           :bold true})}})

(local mode-tag {:init mode-init
                 :update mode-update
                 :provider (fn [] " ")
                 :hl (fn [self]
                       {:bg (let [{: color} (get-mode-opts self.mode)]
                              (catppuccin-color color 0.2))})})

(fn split-path [name]
  (let [last-slash (string.match name ".*/()")]
    (if last-slash
        (values (string.sub name 1 (- last-slash 1))
                (string.sub name last-slash))
        (values "" name))))

(fn file-path-provider [{: filename}]
  (let [name (vim.fn.fnamemodify filename ":.")
        path (split-path name)]
    (if (not (conds.width_percent_below (length path) 0.2))
        (vim.fn.pathshorten path)
        path)))

(fn file-name-provider [{: filename}]
  (case (vim.fn.fnamemodify filename ":t")
    "" "[No Name]"
    name name))

(local file {:init #(tset $1 :filename (vim.api.nvim_buf_get_name 0))
             1 {:provider file-path-provider :hl (hl :subtext0)}
             2 {:provider file-name-provider
                :hl #(when vim.bo.modified (hl :yellow))}
             3 {:condition (fn [] vim.bo.modified)
                :provider ""
                :hl (hl :peach)}
             4 {:condition #(or (not vim.bo.modifiable) vim.bo.readonly)
                :provider " "
                :hl (hl :flamingo)}})

(fn git-diff-provider [self type sym]
  (let [count (or (. self.status type) 0)]
    (when (> count 0) (.. sym count))))

(local git
       {:condition conds.is_git_repo
        :init (fn [self]
                (tset self :status vim.b.gitsigns_status_dict))
        :hl (hl :pink)
        1 {:provider #(string.format " %s " $1.status.head)}
        2 {:provider #(git-diff-provider $1 :added "+") :hl (hl :green)}
        3 {:provider #(git-diff-provider $1 :removed "-") :hl (hl :red)}
        4 {:provider #(git-diff-provider $1 :changed "~") :hl (hl :yellow)}})

(fn lsp-provider [self]
  (let [names []]
    (each [_ {: name} (ipairs (vim.lsp.get_clients {:bufnr 0}))]
      (when (not (some #(= $1 name) self.hidden))
        (table.insert names name)))
    (when (> (length names) 0) (.. "  " (table.concat names " | ")))))

(local lsp {:condition conds.lsp_attached
            :update [:LspAttach :LspDetach]
            :static {:hidden ["GitHub Copilot"]}
            :provider lsp-provider
            :hl (hl :blue)})

(local diagnostics {:condition conds.has_diagnostics
                    :static {:icons {:error (get-diagnostic-sign vim.diagnostic.severity.ERROR)
                                     :warn (get-diagnostic-sign vim.diagnostic.severity.WARN)
                                     :info (get-diagnostic-sign vim.diagnostic.severity.INFO)
                                     :hint (get-diagnostic-sign vim.diagnostic.severity.HINT)}}
                    :init (fn [self]
                            (tset self :errors (get-diagnostic-count :ERROR))
                            (tset self :warns (get-diagnostic-count :WARN))
                            (tset self :infos (get-diagnostic-count :INFO))
                            (tset self :hints (get-diagnostic-count :HINT)))
                    :update [:DiagnosticChanged :BufEnter]
                    1 {:provider #(and (> $1.errors 0)
                                       (.. " " $1.icons.error " " $1.errors))
                       :hl (hl :red)}
                    2 {:provider #(and (> $1.warns 0)
                                       (.. " " $1.icons.warn " " $1.warns))
                       :hl (hl :yellow)}
                    3 {:provider #(and (> $1.infos 0)
                                       (.. " " $1.icons.info " " $1.infos))
                       :hl (hl :teal)}
                    4 {:provider #(and (> $1.hints 0)
                                       (.. " " $1.icons.hint " " $1.hints))
                       :hl (hl :sapphire)}})

(local filetype
       [{:provider #(let [(icon) (MiniIcons.get :filetype vim.bo.filetype)]
                      icon)
         :hl (fn []
               (let [(_ hl-group) (MiniIcons.get :filetype vim.bo.filetype)]
                 {:fg (get-hl-attr hl-group :fg)
                  :bg (catppuccin-color :mantle)}))}
        {:provider #(.. " " vim.bo.filetype) :hl (hl :subtext1)}])

(local ruler {:provider " %7(%l/%3L%):%2c %P "
              :hl {:bg (catppuccin-color :surface1)}})

(fn gap [n]
  {:provider (fn []
               (var g "")
               (for [_ 1 (or n 1)] (set g (.. g " ")))
               g)})

{:setup (fn []
          (heirline.setup {:statusline [mode-bar
                                        (gap 2)
                                        file
                                        (gap 2)
                                        git
                                        {:provider "%="}
                                        diagnostics
                                        (gap 2)
                                        lsp
                                        (gap 2)
                                        filetype
                                        (gap 2)
                                        ruler
                                        mode-tag]}))}
