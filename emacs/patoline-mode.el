;; emacs mode for patoline

(provide 'patoline-mode)
(require 'patoline-input)

(add-to-list 'auto-mode-alist '("\\.txp\\'" . patoline-mode))

(defconst patoline-font-lock-keywords
  (list (cons "\\<[\\][a-zA-Z][a-zA-Z0-9]*\\>"
         'font-lock-keyword-face))
  "Minimal highlighting expressions for Patoline mode.")

(autoload 'patoline-mode "patoline-mode" "Major mode for editing Patoline documents." t)

(add-hook 'patoline-mode-hook 'flyspell-mode)

(define-derived-mode patoline-mode fundamental-mode "WPDL"
  "Major mode for editing Patoline documents."
  (interactive)
  (kill-all-local-variables)
 ; (use-local-map patoline-mode-map)
 ; (set-syntax-table patoline-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(patoline-font-lock-keywords))
  ;; Register our indentation function
;  (set (make-local-variable 'indent-line-function) 'patoline-indent-line)  
  (set (make-local-variable 'pop-up-windows) nil)  
;  (set (make-local-variable 'pop-up-frames) t)  
  (set-input-method "Patoline")
  (make-local-variable 'paren-mode)
  (if (featurep 'xemacs)
      (progn (require 'paren)
	     (paren-set-mode 'blink-paren t))
    (show-paren-mode 'mixed))
  (setq case-fold-search nil)
  (setq major-mode 'patoline-mode)
  (line-number-mode t)
  (column-number-mode t)
  (setq mode-name "Patoline")
;  (patoline-init-mode)
  (run-hooks 'patoline-mode-hook)
)


