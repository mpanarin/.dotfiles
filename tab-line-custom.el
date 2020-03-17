;; TODO: commit a8f2ee424ce895caff15f1ff973e241b8a946aba in master broke the shit. Check whatsup
(load "~/projects/personal/elisp/tab-line/tab-line-orig.el")
(with-eval-after-load 'company-box
  (defun company-box--edges nil
    "Override of original to fix for company-box to properly include the height of tab-line"
    ;; FIXME: This is a harsh fix, As the original package is poorly maintained, I will probably fork it and use my own version with a few patches.
    (or company-box--edges
        (let* ((edges (window-edges nil t nil t))
               (value (replace edges (list (+ (nth 1 edges) (window-tab-line-height))) :start1 1)))
          (setq company-box--edges value)))))


;; TODO: create a layer out of this
;; TODO: tabs should never jump. I should probably cache their position.
(global-tab-line-mode t)
(setq tab-line-tabs-buffer-list-function (lambda () (current-buffer)))

;; `tab-line-tabs-function' should return projectile buffers `projectile-project-buffers' except some regexes
(setq custom-tab-line--exclude-buffer-show-regexp '("magit" "helm" "^\*")) ;; which buffers to never show in the tab-line.
(defun custom-tab-line--exclude-buffer-show-f ()
  (let* ((buffers (projectile-project-buffers)))
    (-filter
     (lambda (buffer)
       (not (-first
             (lambda (regex) (string-match-p regex (buffer-name buffer)))
             custom-tab-line--exclude-buffer-show-regexp)))
     buffers)
    ))
(setq tab-line-tabs-function 'custom-tab-line--exclude-buffer-show-f)

;; `tab-line-exclude-modes' should exclude helm, help, etc.
(setq custom-tab-line-exclude-buffer-regexp '("^magit"  ; regex for modes in which tab-line should not be showed.
                                              "^COMMIT"
                                              "^\*"
                                              "^\ \*"))
(mapc (lambda (mode) (push mode tab-line-exclude-modes)) '(helm-mode help-mode magit-mode vterm-mode))
(defun tab-line-mode--turn-on ()
  "Turn on `tab-line-mode'.
redefined to introduce regexps"
  (unless (or (minibufferp)
              (string-match-p "\\` " (buffer-name))
              (not (projectile-project-p))
              (-filter (lambda (name) (string-match-p name (buffer-name))) custom-tab-line-exclude-buffer-regexp)
              (memq major-mode tab-line-exclude-modes)
              (get major-mode 'tab-line-exclude)
              (buffer-local-value 'tab-line-exclude (current-buffer)))
    (tab-line-mode 1)))

;; additional functions
(defun custom-tab-line--switch (DIRECTION)
  "Switch tab in a specific DIRECTION.
DIRECTION can be 'left 'right"
  (let* ((tabs (funcall tab-line-tabs-function))
         (buffer (current-buffer))
         (index (-elem-index buffer tabs))
         (switch-to (pcase DIRECTION
                      ('left (nth (- index 1) tabs))
                      ('right (nth (+ index 1) tabs)))))
    (if switch-to
        (switch-to-buffer switch-to t t))))

(defun custom-tab-line--switch-left ()
  "Switch tab to the one in the left"
  (interactive)
  (custom-tab-line--switch 'left))

(defun custom-tab-line--switch-right ()
  "Switch tab to the one in the right"
  (interactive)
  (custom-tab-line--switch 'right))

(defun custom-tab-line--select-by-num (INDEX)
  "select tab by index.
if INDEX out of range - do nothing."
  (let* ((tabs (funcall tab-line-tabs-function))
         (switch-to (nth (- INDEX 1) tabs)))
    (if switch-to
        (switch-to-buffer switch-to t t))))

(defun custom-tab-line--select-1 ()
  "Select tab by index 1 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 1))

(defun custom-tab-line--select-2 ()
  "Select tab by index 2 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 2))

(defun custom-tab-line--select-3 ()
  "Select tab by index 3 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 3))

(defun custom-tab-line--select-4 ()
  "Select tab by index 4 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 4))

(defun custom-tab-line--select-5 ()
  "Select tab by index 5 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 5))

(defun custom-tab-line--select-6 ()
  "Select tab by index 6 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 6))

(defun custom-tab-line--select-7 ()
  "Select tab by index 7 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 7))

(defun custom-tab-line--select-8 ()
  "Select tab by index 8 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 8))

(defun custom-tab-line--select-9 ()
  "Select tab by index 9 with `custom-tab-line--select-by-num'"
  (interactive)
  (custom-tab-line--select-by-num 9))

(defun custom-tab-line--close-other ()
  "Close all other tabs."
  ;; TODO: Allow passing optional buffer to keep.
  (interactive)
  (let ((buffers (funcall tab-line-tabs-function))
        (current-window (get-buffer-window (current-buffer))))
    (mapc
     (lambda (buffer) (unless (equal buffer (current-buffer))
                        (kill-buffer buffer)))
     buffers)
    (mapc
     (lambda (window) (unless (equal current-window window) (delete-window window)))
     (get-buffer-window-list (current-buffer))))
  )

(defun custom-tab-line--close-non-visible ()
  "Close all other tabs."
  (interactive)
  (let ((buffers (funcall tab-line-tabs-function)))
    (mapc
     (lambda (buffer) (unless (or (equal buffer (current-buffer)) (get-buffer-window buffer))
                             (kill-buffer buffer)))
     buffers))
  )

(defun custom-tab-line--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "10" str) "➓")
   (t (format "(%s)" str))))

(defun custom-tab-line--numbered-names (buffer buffers)
  (let* ((index (+ (-elem-index buffer buffers) 1))
         (name (buffer-name buffer))
         (str_repr (custom-tab-line--unicode-number (int-to-string index))))
    (format " %s %s " str_repr name)))

;; binds
;; move window dedication to T
(define-key evil-normal-state-local-map (kbd "SPC w T") 'spacemacs/toggle-current-window-dedication)
(define-key evil-normal-state-local-map (kbd "SPC w t") nil)
;; define prefix and keys
(spacemacs/declare-prefix "wt" "tabs")
(evil-leader/set-key
  "wtD" 'custom-tab-line--close-other
  "wtV" 'custom-tab-line--close-non-visible
  "wth" 'custom-tab-line--switch-left
  "wtl" 'custom-tab-line--switch-right
  "wt1" 'custom-tab-line--select-1
  "wt2" 'custom-tab-line--select-2
  "wt3" 'custom-tab-line--select-3
  "wt4" 'custom-tab-line--select-4
  "wt5" 'custom-tab-line--select-5
  "wt6" 'custom-tab-line--select-6
  "wt7" 'custom-tab-line--select-7
  "wt8" 'custom-tab-line--select-8
  "wt9" 'custom-tab-line--select-9)
;; rename first binding for tabs in whichkey
(push '(("\\(.*\\)1" . "custom-tab-line--select-1") .
        ("\\11..9" . "select tab 1..9"))
      which-key-replacement-alist)
;; hide other tab select bindings
(push '((nil . "custom-tab-line--select-[2-9]") . t)
      which-key-replacement-alist)


;; normal customizations
(setq tab-line-auto-hscroll nil)
;; Don't show close button
(setq tab-line-close-button-show nil)
;; don't show new tab button
(setq tab-line-new-tab-choice nil)
;; change naming function
(setq tab-line-tab-name-function 'custom-tab-line--numbered-names)
;; don't show left and right buttons
(setq tab-line-left-button nil)
(setq tab-line-right-button nil)

;; faces
(custom-set-faces
 '(tab-line ((t (:underline "#83898d" :height 1.0))))
 '(tab-line-tab ((t (:inherit tab-line :box (:line-width 1 :color "#282725") :underline "#83898d"))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :box (:line-width 1 :color "#83898d") :underline "#282725"))))
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab :overline "#282725")))))
