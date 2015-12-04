;; emacs mode for patoline

(provide 'patoline-mode)
(require 'patoline-input)

(add-to-list 'auto-mode-alist '("\\.txp\\'" . patoline-mode))

(defface font-patoline-math-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:family "Asana Math" :foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:family "Asana Math" :foreground "LightGray" ,@font))
      (((class color) (background light))
       (:family "Asana Math" :foreground "medium blue"))
      (((class color) (background dark))
       (:family "Asana Math" :foreground "burlywood"))
      (t (:family "Asana Math" ,@font))))
  "Face used to highlight math in Patoline.")

(defconst font-patoline-math-face 'font-patoline-math-face)

(defconst patoline-font-lock-keywords
  (list
   (cons "[$][$]?[^$]+[$][$]?" 'font-patoline-math-face)
   (cons "\\\\[a-zA-Z][a-zA-Z0-9]*\\>" 'font-lock-keyword-face)
   (cons "^[ \t]*===?=?=?=?=?=?\\([^=]+=\\)*[^=]+===*" 'font-lock-doc-face)
   (cons "^[ \t]*---?-?-?-?-?-?\\(-[^-]+\\)*[^-]+---*" 'font-lock-doc-face)
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

(defvar patoline-error-regexp
    '(("^[^\"]+\"\\([^\"]*\\)\", line \\([0-9]+\\), character \\([0-9]+\\):$" 1 2 3 1)
      ("^[^\"]+\"\\([^\"]+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):$" 1 2 (3 . 4) 1)))

(defun select-patoline-program-buffer ()
  (if (and patoline-program-buffer (buffer-live-p patoline-program-buffer))
      (set-buffer patoline-program-buffer)
    (progn
      (setq patoline-program-buffer (get-buffer-create "*patoline-interaction*"))
      (set-buffer patoline-program-buffer)
      )))

(defun select-patoline-view-buffer (buffer-name)
  (if (and patoline-view-buffer (buffer-live-p patoline-view-buffer))
      (set-buffer patoline-view-buffer)
    (progn
      (setq patoline-view-buffer (get-buffer-create (concat "*patoline-view-" buffer-name "*")))
      (set-buffer patoline-view-buffer)
      )))

(defun patoline-process-sentinel (p m)
  (if (not (string-match "finished\\.*" m))
      (display-buffer patoline-program-buffer t 'visible)
    (select-patoline-program-buffer)
    (if patoline-view-process
	(signal-process patoline-view-process 1)))
  (message m))

(defun display-program-buffer ()
  "display patoline compilation buffer"
  (interactive)
  (display-buffer patoline-program-buffer t 'visible))

(defun patoline-compile-format ()
  (let
      ((el (if patoline-compile-edit-link "--edit-link " ""))
       (dr (if (string= patoline-compile-driver "-c") "" "--driver "))
       (dy (if (and (string= patoline-compile-driver "Bin") patoline-compile-dynlink) "--dynlink " "")))
    (format "patoline %s%s%s %s \"%%s\"" el dy dr patoline-compile-driver)))

(defvar patoline-compile-edit-link
  t
  "If t patoline should produce link for edition")

(defvar patoline-compile-dynlink
  nil
  "If t patoline should produce link for edition")

(defvar patoline-compile-driver
  "Bin"
  "Patoline driver to use when compiling (Pdf, Bin, ...)")

(defvar patoline-drivers-list
  '("Pdf" "Bin" "SVG" "Html" "DriverCairo" "Image" "-c")
  "List of patoline supported drivers")

(defvar patoline-options-list
  '(("Edtion link" . 'patoline-compile-edit-link)
    ("Dynamik linking" . 'patoline-compile-dynlink))
  "List of supported toggle options")

(defun patoline-set-driver (driver)
  (setq patoline-compile-driver driver)
  (patoline-update-driver-menu))

(defun patoline-toggle-option (opt)
  (set opt (not (symbol-value opt)))
  (patoline-update-option-menu))

(defun patoline-update-driver-menu ()
  (easy-menu-change
   '("Patoline") "Driver"
   (mapcar (lambda (driver)
	     (vector driver
		     (list 'patoline-set-driver driver)
		     ':style 'toggle
		     ':selected (equal patoline-compile-driver driver)
		     ':active t))
	   patoline-drivers-list)))

(defun patoline-update-option-menu ()
  (easy-menu-change
   '("Patoline") "Options"
   (mapcar (lambda (option)
;;	     (message "toggle %S %S" (cdr option) (eval (cdr option)))
	     (vector (car option)
		     (list 'patoline-toggle-option (cdr option))
		     ':style 'toggle
		     ':selected (nth 1 (cdr option))
		     ':active t))
	   patoline-options-list)))

(defun patoline-compile ()
  "compile the current buffer with patoline"
  (interactive)
  (save-buffer)
  (save-excursion
    (let ((cmd-format (read-from-minibuffer "compile: " (patoline-compile-format))))
      (setq patoline-compile-format cmd-format)
      (let ((cmd (format cmd-format (file-name-nondirectory buffer-file-name)))
	    (dir-name (file-name-directory buffer-file-name)))
	(select-patoline-program-buffer)
	(erase-buffer)
	(cd dir-name)
	(compilation-start cmd t (lambda (name) "*patoline-interaction*") t)
	(set-process-sentinel (get-buffer-process patoline-program-buffer) 'patoline-process-sentinel)))))

(defvar patoline-default-env
  "center"
  "Patoline default environment")

(defun patoline-env ()
  "insert environment"
  (interactive)
  (let ((env (read-from-minibuffer "insert environment: " patoline-default-env)))
    (setq patoline-default-env env)
    (insert "\\begin{" env "}\n")
    (save-excursion (insert "\n\\end{" env "}"))))

(defvar patoline-view-format
  "embedded"
  "What to do to view patoline document. Examples [embedded], [xpdf \"%s\"]")

(defvar patoline-view-process
  nil)

(defun patoline-view ()
  "view the pdf corresponding to the current buffer"
  (interactive)
  (let ((file-name
	 (concat (file-name-sans-extension (buffer-file-name (current-buffer))) ".pdf"))
	(buffer-name
	 (file-name-sans-extension (buffer-file-name (current-buffer))))
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
	  (select-patoline-view-buffer buffer-name)
	  (cd (file-name-directory file-name))
	  (setq patoline-view-process
		(apply 'start-process (concat "patoline-view-" buffer-name) nil (car cmd) (cdr cmd))))))))

(defun patoline-glview ()
  "view the binary output corresponding to the current buffer"
  (interactive)
  (let ((file-name
	 (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
	(dir-name (file-name-directory buffer-file-name)))
    (let ((cmd
	   (if patoline-compile-dynlink
	       (split-string-and-unquote (format "\"%s.tmx\" --driver DriverGL --in \"%s.bin\"" file-name file-name))
	     (split-string-and-unquote (format "patoline --driver DriverGL \"%s.bin\"" file-name)))
	   ))
      (save-excursion
	(select-patoline-view-buffer file-name)
	(cd dir-name)
	(setq patoline-view-process
	      (get-buffer-process
	       (apply 'make-comint (concat "patoline-view-" file-name) (car cmd) nil (cdr cmd))))))))

(defun patoline-goto (file line col)
  (find-file file)
  (goto-line line)
  (let ((bol (position-bytes (point))))
    (let ((dest (+ bol col)))
      (while (< (position-bytes (point)) dest)
	(forward-char)))))

(defun patoline-fontify-and-mmm-parse-buffer ()
  "Font lock fontify and MMM parse buffer"
  (interactive)
  (progn (font-lock-fontify-buffer) (mmm-parse-buffer)))

(defvar patoline-mode-map
  (let ((patoline-mode-map (make-sparse-keymap)))
    (define-key patoline-mode-map "\C-c\C-c" 'patoline-compile)
;    (define-key patoline-mode-map "\C-c\C-e" 'patoline-make)
    (define-key patoline-mode-map "\C-c\C-a" 'patoline-env)
    (define-key patoline-mode-map "\C-c\C-v" 'patoline-glview)
    (define-key patoline-mode-map "\C-c\C-p" 'patoline-view)
    (define-key patoline-mode-map "\C-c\C-s" 'patoline-forward-search)
    (define-key patoline-mode-map "\C-c\C-l" 'display-program-buffer)
    (define-key patoline-mode-map "\C-c\C-f" 'patoline-fontify-and-mmm-parse-buffer)
    patoline-mode-map)
  "Keymap for PATOLINE major mode")

(add-hook 'patoline-mode-hook (lambda () (progn
                                           (flyspell-mode t)
                                           (set (make-local-variable 'comment-start) "(*")
                                           (set (make-local-variable 'comment-end) "*)"))))


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

(defun patoline-forward-search ()
  (interactive)
  (let ((line (line-number-at-pos))
	(col (- (position-bytes (point)) (position-bytes (line-beginning-position))))
	(buffer-name
	 (file-name-sans-extension (buffer-file-name (current-buffer)))))
;;    (message  (format "e %d %d\n" line col))
    (save-excursion
      (select-patoline-view-buffer buffer-name)
      (if patoline-view-process
	  (process-send-string patoline-view-process (format "e %d %d\n" line col))))))

(define-derived-mode patoline-mode fundamental-mode "Patoline"
  "Major mode for editing Patoline documents."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'patoline-mode)
  (line-number-mode t)
  (column-number-mode t)
  (setq mode-name "Patoline")
  (make-local-variable 'compilation-error-regexp-alist)
  (setq-default compilation-error-regexp-alist patoline-error-regexp)
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
  (make-local-variable 'patoline-view-process)
  (make-local-variable 'patoline-default-env)
  (if (featurep 'xemacs)
      (progn (require 'paren)
	     (paren-set-mode 'blink-paren t))
    (show-paren-mode 'mixed))
  (setq case-fold-search nil)
  (setq mmm-primary-mode-entry-hook
	(list (lambda () (set-input-method "Patoline"))))
  (patoline-build-menu)
  (run-hooks 'patoline-mode-hook)
)

(require 'mmm-mode nil t)
(require 'tuareg nil t)

(define-derived-mode tuareg-patoline-mode tuareg-mode "ocaml-patoline"
  "Major mode for editing OCaml inside patoline."
    (define-key tuareg-patoline-mode-map "\C-c\C-c" 'patoline-compile)
;    (define-key tuareg-patoline-mode-map "\C-c\C-e" 'patoline-make)
    (define-key tuareg-patoline-mode-map "\C-c\C-a" 'patoline-env)
    (define-key tuareg-patoline-mode-map "\C-c\C-v" 'patoline-glview)
    (define-key tuareg-patoline-mode-map "\C-c\C-p" 'patoline-view)
    (define-key tuareg-patoline-mode-map "\C-c\C-s" 'patoline-forward-search)
    (define-key tuareg-patoline-mode-map "\C-c\C-l" 'display-program-buffer)


  )

(defvar mmm-tuareg-patoline-mode-submode-hook
  (list (lambda () (set-input-method "ucs"))
	(lambda () (overlay-put mmm-current-overlay 'entry-hook
				(list (lambda () (set-input-method "ucs")))))))

(defvar mmm-patoline-mode-submode-hook
  (list (lambda () (set-input-method "Patoline"))
	(lambda () (overlay-put mmm-current-overlay 'entry-hook
				(list (lambda () (set-input-method "Patoline")))))))

(defvar mmm-patoline-subregions nil)

(defun mmm-patoline-back (limit mode)
  (setq mmm-patoline-subregions nil)
  (setq mmm-patoline-parsing t)
  (let ((depth 1)
	(subs nil)
	(ret nil))
    (while (> depth 0)
      (let ((regexp (case mode
		      ('caml "[]})({[\"]\\|<[$<]")
		      ('string "\\\\?\"")
		      ('text "\\\\\\(\\([Cc]aml\\)\\|\\(diagram\\)(\\)\\|>>")
		      ('math "\\\\\\(\\([Cc]aml\\)\\|\\(diagram\\)(\\)\\|\\$>"))))
;	(message "back: %S %S %S %S %S"  mode depth (point) subs mmm-patoline-subregions )
	(setq ret (search-forward-regexp regexp limit))
	(let ((m (match-string 0))
	      (pos (cons (match-beginning 0) (match-end 0))))
;	  (message m)
	  (cond
	    ((equal m "\\Caml(") (setq depth (+ depth 1) subs (cons (cons mode pos) subs) mode 'caml))
	    ((equal m "\\caml(") (setq depth (+ depth 1) subs (cons (cons mode pos) subs) mode 'caml))
	    ((equal m "\\diagram(") (setq depth (+ depth 1) subs (cons (cons mode pos) subs) mode 'caml))
	    ((equal m "(") (setq depth (+ depth 1) subs (cons nil subs)))
	    ((equal m "{") (setq depth (+ depth 1)))
	    ((equal m "[") (setq depth (+ depth 1)))
	    ((equal m ")")
	      (if (and (> depth 1) (car subs))
		  (progn
		    (setq mmm-patoline-subregions
			  (cons (list 'patoline-tuareg (cdr (car subs)) pos)
				mmm-patoline-subregions))
		    (setq mode (car (car subs)))))
	      (setq depth (- depth 1) subs (cdr subs)))
	    ((equal m "}") (setq depth (- depth 1)))
	    ((equal m "]") (setq depth (- depth 1)))
	    ((equal m "<<") (setq depth (+ depth 1) subs (cons pos subs) mode 'text))
	    ((equal m "<$") (setq depth (+ depth 1) subs (cons pos subs) mode 'math))
	    ((equal m ">>")
	      (if (> depth 1)
		  (setq mmm-patoline-subregions
			(cons (list 'patoline-quote-text-args (car subs) pos)
			      mmm-patoline-subregions)))
	      (setq depth (- depth 1) subs (cdr subs) mode 'caml))

	    ((equal m "$>")
	     (if (> depth 1)
		 (setq mmm-patoline-subregions
		       (cons (list 'patoline-quote-math-args (car subs) pos)
			     mmm-patoline-subregions)))
	     (setq depth (- depth 1) subs (cdr subs) mode 'caml))
	    ((equal m "\"")
	     (if (equal mode 'string)
		 (setq depth (- depth 1) mode 'caml)
	       (setq depth (+ depth 1) mode 'string)))))))
    ret))

(defun mmm-make-region-by-class (class beg end)
  (let ((class (mmm-get-class-spec class))
	(front (car beg))
	(back (cdr end))
	(beg (cdr beg))
	(end (car end))
	match-face match-name face front-form back-form)
;;    (message "make-region %S %S %S" (plist-get class :submode) beg end)
    (setq front-form (regexp-quote (buffer-substring front beg)))
    (setq back-form (regexp-quote (buffer-substring end back)))
    (setq match-face (plist-get class :match-face))
    (setq match-name (plist-get class :match-name))
    (setq face
	  (cond ((functionp match-face)
		 (mmm-save-all
		  (funcall match-face front-form)))
		(match-face
		 (cdr (assoc front-form match-face)))
		(t
		 (plist-get class :face))))
    (setq name
	  (cond ((plist-get class :skel-name)
		 ;; Optimize the name to the user-supplied str
		 ;; if we are so instructed.
		 str)
		;; Call it if it is a function
		((functionp match-name)
		 (mmm-save-all (funcall match-name front-form)))
		;; Now we know it's a string, does it need to
		;; be formatted?
		((plist-get class :save-name)
		 ;; Yes.  Haven't done a match before, so
		 ;; match the front regexp against the given
		 ;; form to format the string
		 (string-match (plist-get class :front)
			       front-form)
		 (mmm-format-matches match-name front-form))
		(t
		 ;; No, just use it as-is
		 match-name)))
    (mmm-make-region
     (plist-get class :submode) beg end
					;   :face face
					;   :name name
     :front front
     :back back
     :match-front front-form
     :match-back  back-form
     :evaporation 'front
     :beg-sticky t :end-sticky t
     :creation-hook (plist-get class :creation-hook))))

(defvar mmm-patoline-lock nil)

(defun mmm-patoline-enable-sub-regions ()
  (if (not mmm-patoline-lock)
      (progn
;;	(message "enable: %S" mmm-patoline-subregions)
	(setq mmm-patoline-lock t)
	(dolist (args mmm-patoline-subregions)
;;	  (message "apply: %S" args)
	  (ignore-errors (apply 'mmm-make-region-by-class args)))
	(setq mmm-patoline-lock nil)
	(setq mmm-patoline-subregions nil))))

(defun mmm-patoline-caml-back (limit)
  (mmm-patoline-back limit 'caml))

(defun mmm-patoline-math-back (limit)
  (mmm-patoline-back limit 'math))

(defun mmm-patoline-text-back (limit)
  (mmm-patoline-back limit 'text))

(if (and (featurep 'mmm-mode) (featurep 'tuareg))
    (progn
      (setq mmm-global-mode 'maybe)
      (setq mmm-patoline-lock nil)
      (setq mmm-patoline-subregions nil)
      (mmm-add-mode-ext-class nil "\\.txp" 'patoline-tuareg)
      (mmm-add-mode-ext-class nil "\\.txp" 'patoline-verb-tuareg)
      (mmm-add-mode-ext-class nil "\\.txp" 'patoline-verb-sml)
      (mmm-add-mode-ext-class nil "\\.txp" 'patoline-quote-math-args)
      (mmm-add-mode-ext-class nil "\\.txp" 'patoline-quote-text-args)
      (mmm-add-classes
       '((patoline-tuareg
	  :submode tuareg-patoline-mode
	  :front "\\\\\\([Cc]aml\\|diagram\\)("
	  :back (lambda (limit) (mmm-patoline-back limit 'caml))
	  :creation-hook (lambda () (mmm-patoline-enable-sub-regions))
	  :insert ((?C tuareg-patoline-mode nil @ "\\Caml(\n"  @ " " _ " " @ "\n)" @)
		   (?c tuareg-patoline-mode nil @ "\\caml(\n"  @ " " _ " " @ "\n)" @)
		   (?d tuareg-patoline-mode nil @ "\\diagram(\n"  @ " " _ " " @ "\n)" @)))
       (patoline-verb-tuareg
	  :submode tuareg-patoline-mode
	  :front "^###[ \t]*OCaml"
	  :back "^###")
       (patoline-verb-sml
	  :submode sml-mode
	  :front "^###[ \t]*SML"
	  :back "^###")
       (patoline-quote-text-args
	  :submode patoline-mode
	  :front (lambda (limit) nil)
	  :back (lambda (limit) nil)
	  :insert ((?m patoline-mode nil @ "<$"  @ " " _ " " @ "$>" @)
		   (?t patoline-mode nil @ "<<"  @ " " _ " " @ ">>" @)))
       (patoline-quote-math-args
	  :submode patoline-mode
	  :front (lambda (limit) nil)
	  :back (lambda (limit) nil)
	  :insert ((?m patoline-mode nil @ "<$"  @ " " _ " " @ "$>" @)
		   (?t patoline-mode nil @ "<<"  @ " " _ " " @ ">>" @)))
	 ))))


(defun patoline-build-menu ()
  (easy-menu-define
   patoline-mode-menu (list patoline-mode-map)
   "Patoline Mode Menu."
   '("Patoline"
     ["Compile..." patoline-compile t]
     ["View Pdf..." patoline-view t]
     ["View Gl..." patoline-glview t]
     ("Driver" ["Dummy" nil t])
     ("Options" ["Dummy" nil t])))
  (easy-menu-add patoline-mode-menu)
  (patoline-update-driver-menu)
  (patoline-update-option-menu))
