(local zen-mode (require :zen-mode))

(zen-mode.setup {:window {:background 1.0
                          :width 80
                          :options {:cursorline false
                                    :number false
                                    :relativenumber false
                                    :signcolumn "no"}}})
