;; (dap-register-debug-template "Python :: Uaprom Docker"
;;                              (list :type "python"
;;                                    :port 9977
;;                                    :request "attach"
;;                                    :host "localhost"
;;                                    ;; :pathMappings '((ht ("localRoot" "${workspaceFolder}") ("remoteRoot" ".")))
;;                                    :args ""
;;                                    :cwd nil
;;                                    :module nil
;;                                    :program nil
;;                                    :name "Python :: Uaprom Docker"))

(dap-register-debug-template "Python :: Uaprom Docker3"
                             (list :type "python-remote-debugging"
                                   :port 9977
                                   :request "attach"
                                   :host "localhost"
                                   :pathMappings '((ht ("localRoot" "${workspaceFolder}") ("remoteRoot" ".")))
                                   :args ""
                                   :cwd nil
                                   :module nil
                                   :program nil
                                   :name "Python :: Uaprom Docker3"))
