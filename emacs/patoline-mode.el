;; emacs mode for patoline

(provide 'patoline-mode)
(require 'patoline-input)
(require 'comint)

(add-to-list 'auto-mode-alist '("\\.txp\\'" . patoline-mode))


(defconst patoline-font-lock-keywords
  (list (cons "\\\\[a-zA-Z][a-zA-Z0-9]*\\>" 'font-lock-keyword-face)
        (cons "^[ \t]*===*\\([^=]+=\\)*[^=]+===*" 'font-lock-doc-face)
        (cons "^[ \t]*---*\\(-[^-]+\\)*[^-]+---*" 'font-lock-doc-face)
        (cons "^[ \t]*===========*" 'font-lock-doc-face)
        (cons "^[ \t]*-----------*" 'font-lock-doc-face)
        (cons "^[ \t]*[=-]>[^\n]*\n[\r \t]*\n" 'font-lock-doc-face)
        (cons "^[ \t]*[=-]<\\([ \t]*[=-]<\\)*" 'font-lock-doc-face)
  )
  "Minimal highlighting expressions for Patoline mode.")

(defvar patoline-mode-hook nil)
(autoload 'patoline-mode "patoline-mode" "Major mode for editing Patoline documents." t)

(defvar patoline-program-buffer
  nil)
(defvar patoline-view-buffer
  nil)

(defun select-patoline-program-buffer ()
  (if (and patoline-program-buffer (buffer-live-p patoline-program-buffer))
      (set-buffer patoline-program-buffer)
    (progn
      (setq patoline-program-buffer (get-buffer-create "*patoline-interaction*"))
      (set-buffer patoline-program-buffer)
      (comint-mode)
      (make-local-variable 'comint-output-filter-functions)
      (make-local-variable 'comint-exec-hook)
;      (setq comint-output-filter-functions
;	    '(comint-postoutput-scroll-to-bottom patoline-filter-comint-output))
      (setq comint-exec-hook nil))))

(defun patoline-process-sentinel (p m)
  (if (not (string-match "finished\\.*" m))
      (display-buffer patoline-program-buffer t 'visible)) 
  (message m))

(defvar patoline-compile-format
  "patoline \"%s\""
  "What to do to compile patoline document. Examples [patoline \"%s\"], [make]")

(defun patoline-compile ()
  "compile the current buffer with patoline"
  (interactive)
  (save-buffer)
  (save-excursion
    (let ((cmd-format (read-from-minibuffer "compile: " patoline-compile-format)))
      (setq patoline-compile-format cmd-format)
      (let ((cmd (split-string-and-unquote (format cmd-format buffer-file-name))))
	(select-patoline-program-buffer)
	(erase-buffer)
	(comint-exec patoline-program-buffer "patoline-process" (car cmd) nil (cdr cmd))
	(set-process-sentinel (get-process "patoline-process") 'patoline-process-sentinel)))))

(defvar patoline-view-format
  "embedded"
  "What to do to view patoline document. Examples [embedded], [xpdf \"%s\"]")

(defun patoline-view ()
  "view the pdf corresponding to the current buffer"
  (interactive)
  (let ((file-name 
	 (concat (file-name-sans-extension (buffer-file-name (current-buffer))) ".pdf"))
	(cmd-format (read-from-minibuffer "view: " patoline-view-format)))
    (setq patoline-view-format cmd-format)
    (if (string-equal cmd-format "embedded")
	(progn
	  (find-file-other-window file-name)
	  (save-excursion
	    (set-buffer (find-buffer-visiting file-name))
	    (auto-revert-mode)))
      (let ((cmd (split-string-and-unquote (format cmd-format file-name))))
	(save-excursion
	  (select-patoline-program-buffer)
	  (erase-buffer)
	  (apply 'start-process "patoline-view" patoline-view-buffer (car cmd) (cdr cmd))
	  (set-process-sentinel (get-process "patoline-process") 'patoline-process-sentinel))))))

(defvar patoline-mode-map
  (let ((patoline-mode-map (make-keymap)))
    (progn
      (define-key patoline-mode-map (kbd "C-c C-c") 'patoline-compile)
      (define-key patoline-mode-map (kbd "C-c C-e") 'patoline-make)
      (define-key patoline-mode-map (kbd "C-c C-v") 'patoline-view)
    patoline-mode-map))
  "Keymap for PATOLINE major mode")

(add-hook 'patoline-mode-hook (lambda () (flyspell-mode t)))

(defvar patoline-mode-syntax-table
  (let ((patoline-mode-syntax-table (make-syntax-table)))
	
    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?_ "w" patoline-mode-syntax-table)
	(modify-syntax-entry ?' "w" patoline-mode-syntax-table)
	(modify-syntax-entry ?\$ "_" patoline-mode-syntax-table)
	
	; Comment styles are same as C++
	(modify-syntax-entry ?\) ")(4" patoline-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" patoline-mode-syntax-table)
	(modify-syntax-entry ?\( "()1" patoline-mode-syntax-table)
;	(modify-syntax-entry ?\n "> b" patoline-mode-syntax-table)
	patoline-mode-syntax-table)
  "Syntax table for patoline-mode")

(define-derived-mode patoline-mode fundamental-mode "WPDL"
  "Major mode for editing Patoline documents."
  (interactive)
  (kill-all-local-variables)
  (use-local-map patoline-mode-map)
  (set-syntax-table patoline-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(patoline-font-lock-keywords))
  ;; Register our indentation function
;  (set (make-local-variable 'indent-line-function) 'patoline-indent-line)  
  (set-input-method "Patoline")
  (make-local-variable 'paren-mode)
  (make-local-variable 'patoline-compile-format)
  (make-local-variable 'patoline-view-format)
  (make-local-variable 'patoline-view-buffer)
  (if (featurep 'xemacs)
      (progn (require 'paren)
	     (paren-set-mode 'blink-paren t))
    (show-paren-mode 'mixed))
  (setq case-fold-search nil)
  (setq major-mode 'patoline-mode)
  (line-number-mode t)
  (column-number-mode t)
  (setq mode-name "Patoline")
  (run-hooks 'patoline-mode-hook)
)

(require 'mmm-mode nil t)
(require 'tuareg nil t)

(if (and (featurep 'mmm-mode) (featurep 'tuareg))
    (progn 
      (setq mmm-global-mode 'maybe)
      (mmm-add-mode-ext-class nil "\\.txp" 'patoline-tuareg)
      (mmm-add-classes
       '((patoline-tuareg
	  :submode tuareg-mode
	  :front "\\\\[Cc]aml("
	  :back "^)"
	  :insert ((?c tuareg-mode nil @ "\\Caml(\n"  @ " " _ " " @ "\n)" @)))
	 ))))




