;; Seed the random-number generator
(random t)

;; Enter passwords in minibuffer
(setq epa-pinentry-mode 'loopback)

;; Whitespace-style
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100)

;; Add Urban Dictionary to webjump (C-x g)
(eval-after-load "webjump"
  '(add-to-list 'webjump-sites '("Urban Dictionary" .
                             [simple-query
                              "www.urbandictionary.com"
                              "http://www.urbandictionary.com/define.php?term="
                              ""])))

;; Fix whitespace on save, but only if the file was clean
(global-whitespace-cleanup-mode)

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (λ (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (λ (insert "©")))
(global-set-key (kbd "C-x 8 - >") (λ (insert "→")))
(global-set-key (kbd "C-x 8 8") (λ (insert "∞")))
(global-set-key (kbd "C-x 8 ( c )") (λ (insert "©")))
(global-set-key (kbd "C-x 8 v") (λ (insert "✓")))

;; Add JSP expansions to html-mode
(eval-after-load "sgml-mode" '(require 'jsp-expansions))

;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)

;; Some stupid multiple cursors things
(defun mc--cursor-region-contents ()
  (let (entries)
    (mc/for-each-cursor-ordered
     (setq entries (cons (buffer-substring-no-properties (overlay-get cursor 'point)
                                                         (overlay-get cursor 'mark))
                         entries)))
    (reverse entries)))

(defun mc/eval-with-cursor-regions ()
  (interactive)
  (eval-expression
   (read--expression "Eval: "
                     (concat "(" (s-join " " (mc--cursor-region-contents)) ")"))))

(define-key mc/keymap (kbd "H-:") 'mc/eval-with-cursor-regions)

(provide 'my-misc)
