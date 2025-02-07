(local {: autoload} (require :nfnl.module))
(local core (autoload :nfnl.core))

(var tbl {:x 1})
(set tbl (core.merge tbl {:y 2}))

;; (core.run! (fn [x] (vim.print x)) (pairs tbl))

;; (each [k v (pairs tbl)]
;;  (print k "is" v))

(core.run! (fn [x] (vim.print x)) (pairs {:x 1 :y 2}))

