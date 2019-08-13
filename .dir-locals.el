((lsp-mode .
           ((lsp-pyls-plugins-pylint-args . ["--rcfile=./.pylintrc"])))
 (python-mode .
              ((fill-column . 80)))
 (web-mode .
           ((eval . (web-mode-set-engine "django")))))
