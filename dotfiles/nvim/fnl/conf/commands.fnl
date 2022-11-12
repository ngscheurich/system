(local {:nvim_create_user_command command
        :nvim_create_buf create-buf} vim.api)

(local create-scratch-buffer
        (fn [] (let [buf (create-buf false true)]
                    (vim.cmd (.. "split " buf))
                    (tset vim.opt_local :filetype "fennel"))))

(command "Scratch" create-scratch-buffer {}) 
