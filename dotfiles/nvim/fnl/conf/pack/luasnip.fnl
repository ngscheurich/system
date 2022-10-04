(local ls (require :luasnip))
(local s ls.snippet)
(local t ls.text_node)
(local i ls.insert_node)
(local fmt (. (require :luasnip.extras.fmt) :fmt))
(local rep (. (require :luasnip.extras) :rep))

;; Config
(ls.config.set_config {:history true
                       :updateevents "TextChanged,TextChangedI"}) 
                       
;; Mappings
(vim.keymap.set [:i :s] :<C-o> (fn [] (when (ls.expand_or_jumpable)
                                       (ls.expand_or_jump))))
(vim.keymap.set [:i :s] :<C-i> (fn [] (when (ls.expand_or_jumpable -1)
                                       (ls.expand_or_jump -1))))
;; TODO: Why only insert mode here?
(vim.keymap.set [:i] :<C-p> (fn [] (when (ls.choice_active)
                                    (ls.change_choice 1))))

;; Elixir
(local testhtml (fmt "File.write(\"{}\", {})\nSystem.cmd(\"open\", [\"{}\"])"
                     [(i 1 "/tmp/testout.html") (i 2) (rep 1)]))
(ls.add_snippets :elixir [(s :pry [(t ["require IEx" "IEx.pry()"])])
                          (s :ioi (fmt "IO.inspect({}, label: \"{}\")" [(i 1) (rep 1)]))
                          (s :testhtml testhtml)]) 

;; Fennel
(ls.add_snippets :fennel [(s :req (fmt "(local {} (require :{}))"
                                       [(i 1 :module) (rep 1)]))])

;; TypeScript
(ls.add_snippets :typescript [(s :cmpi (fmt "export {{{} as default}} from \"./{}\""
                                            [(i 1) (rep 1)]))])
(ls.add_snippets :typescript [(s :log (fmt "console.log(\"{}\", {})"
                                           [(i 1) (rep 1)]))])
