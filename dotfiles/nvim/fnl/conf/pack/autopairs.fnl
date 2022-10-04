(local npairs (require "nvim-autopairs"))

(npairs.setup {:disable_filetype ["TelescopePrompt"
                                  "clojure"
                                  "scheme"
                                  "lisp"
                                  "fennel"]})
