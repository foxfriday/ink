;;; ink.el --- Insert images in a document using inkscape -*- lexical-binding: t; -*-

;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, tex, convenience
;; URL: https://github.com/foxfriday/ink

;;; Commentary:
;; You can insert a new figure at point using `ink-make-figure', edit an
;; existing figure with `ink-edit-figure', or start a new figure from an
;; existing one with `ink-duplicate-figure'.  This assumes you have
;; inkscape installed.

;;; Code:

(defgroup ink nil
  "Insert images in a document using inkscape."
  :group 'multimedia
  :prefix "ink-")

(defcustom ink-fig-dir "figures"
  "Directory where the images are saved, relative to the document."
  :type 'string)

(defcustom ink-flags-latex (list "--export-area-drawing"
                                 "--export-dpi 300"
                                 "--export-type=pdf"
                                 "--export-latex"
                                 "--export-overwrite")
  "List of flags to produce a LaTeX file with inkscape."
  :type '(repeat string))

(defcustom ink-flags-png (list "--export-area-drawing"
                               "--export-dpi 100"
                               "--export-type=png"
                               "--export-overwrite")
  "List of flags to produce a png file with inkscape."
  :type '(repeat string))

(defcustom ink-flags ink-flags-latex
  "Default list of flags for inkscape."
  :type '(repeat string))

(defcustom ink-flags-options
  (list (cons 'latex-mode ink-flags-latex)
        (cons 'org-mode ink-flags-png)
        (cons 'markdown-mode ink-flags-png))
  "The command line flags used with each major and derived modes.

If the current mode is not found, `ink-flags' is used. Note that
the flags and the insert template should match: if your template
inserts a LaTeX fragment, you should use flags that produce an
appropriate LaTeX figure, and if your template expects a png file, the
flags for that mode should produce a png file."
  :type '(alist :key-type symbol :value-type (repeat string)))

(defcustom ink-process-cmnd 'ink-process-cmnd-default
  "Function to make the shell command from the svg file and the flags."
  :type 'function)

(define-obsolete-variable-alias
  'ink-latex
  'ink-insert-latex
  "2021-11-02")

(defcustom ink-insert-latex "\n\\begin{figure}
    \\centering
    \\def\\svgwidth{\\columnwidth}
    \\subimport{%s}{%s.pdf_tex}
    \\caption{}
    \\label{fig:%s}
\\end{figure}\n"
  "LaTeX insert template."
  :type 'string)

(defcustom ink-insert-org "#+NAME: fig:%3$s\n[[file:%1$s/%2$s.png]]\n"
  "Org mode insert template.

`%1$s' is replaced with the figure's folder.
`%2$s' is replaced with the figure's file name.
`%3$s' is replaced with the figure's file name without the extension.

Note that if your template expects a png file, the corresponding
flags should produce a png file."
  :type 'string)

(defcustom ink-insert-md "![%3$s](%1$s/%2$s.png)"
  "Markdown mode insert template.

`%1$s' is replaced with the figure's folder.
`%2$s' is replaced with the figure's file name.
`%3$s' is replaced with the figure's file name without the extension.

Note that if your template expects a png file, the corresponding
flags should produce a png file."
  :type 'string)

(defcustom ink-insert ink-insert-latex
  "Default insert template."
  :type 'string)

(defcustom ink-insert-options
  (list (cons 'latex-mode ink-insert-latex)
        (cons 'org-mode ink-insert-org)
        (cons 'markdown-mode ink-insert-md))
  "The insert template used with each major and derived modes.

If the current mode is not found, `ink-insert' is used.  If you change this
variable, please make sure you also read the documentation for
`ink-flags-options' as you may need to change that variable too."
  :type '(alist :key-type symbol :value-type string))

(defcustom ink-temp-dir "temp"
  "Name of the temporary directory used while editing a figure."
  :type 'string)

(defvar ink-log-buffer "*inky-log*"
  "Name of the buffer receiving inkscape's output.")

(defvar ink-default-file
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<svg
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   width=\"210mm\"
   height=\"297mm\"
   viewBox=\"0 0 210 297\"
   version=\"1.1\"
   id=\"svg8\"
   inkscape:version=\"1.0.2 (e86c870879, 2021-01-15)\"
   sodipodi:docname=\"default.svg\">
  <defs
     id=\"defs2\" />
  <sodipodi:namedview
     id=\"base\"
     pagecolor=\"#ffffff\"
     bordercolor=\"#666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"0.0\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"0.35\"
     inkscape:cx=\"400\"
     inkscape:cy=\"560\"
     inkscape:document-units=\"mm\"
     inkscape:current-layer=\"layer1\"
     inkscape:document-rotation=\"0\"
     showgrid=\"false\"
     inkscape:window-width=\"1920\"
     inkscape:window-height=\"1068\"
     inkscape:window-x=\"1920\"
     inkscape:window-y=\"1068\"
     inkscape:window-maximized=\"0\" />
  <metadata
     id=\"metadata5\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
        <dc:title></dc:title>
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label=\"Layer 1\"
     inkscape:groupmode=\"layer\"
     id=\"layer1\" />
</svg>"
  "Default file template.")

(defun ink--mode-value (alist default)
  "Return the value in ALIST for the current major mode, else DEFAULT.

ALIST maps major modes to values.  Modes derived from a mode in
ALIST also match."
  (let ((mode (apply #'derived-mode-p (mapcar #'car alist))))
    (if mode (cdr (assq mode alist)) default)))

(defun ink--figure-file (name)
  "Return the path of the svg figure NAME in the figure directory."
  (expand-file-name (concat name ".svg") (expand-file-name ink-fig-dir)))

(defun ink--temp-file (name)
  "Return the temporary svg path for figure NAME, creating the directory."
  (let ((tdir (expand-file-name ink-temp-dir)))
    (make-directory tdir t)
    (expand-file-name (concat name ".svg") tdir)))

(defun ink-process-cmnd-default (file flags)
  "Make the shell command to export FILE using the string FLAGS."
  (concat "inkscape " (shell-quote-argument file) " " flags))

(defun ink--post-process (file)
  "Move the files exported from FILE to the image directory.

Removes the temporary directory when it is left empty."
  (let* ((tdir (file-name-directory file))
         (name (file-name-sans-extension (file-name-nondirectory file)))
         (idir (file-name-as-directory (expand-file-name ink-fig-dir)))
         (outputs (directory-files tdir t
                                   (concat "\\`" (regexp-quote name) "\\."))))
    (make-directory idir t)
    (dolist (output outputs)
      (rename-file output (concat idir (file-name-nondirectory output)) t))
    (unless (directory-files tdir nil directory-files-no-dot-files-regexp t)
      (delete-directory tdir))
    (message "ink: exported %s to %s" name idir)))

(defun ink--insert-snippet (file frmt marker)
  "Insert the include snippet for FILE at MARKER using the template FRMT."
  (let ((buffer (marker-buffer marker))
        (name (file-name-sans-extension (file-name-nondirectory file))))
    (if (not (buffer-live-p buffer))
        (message "ink: buffer for figure %s is gone, snippet not inserted" name)
      (with-current-buffer buffer
        (save-excursion
          (goto-char marker)
          (insert (format frmt ink-fig-dir name (downcase name)))))
      (set-marker marker nil))))

(defun ink--process (file dir flags frmt marker)
  "Export FILE with inkscape and move the results to the image directory.

DIR is the directory of the document the figure belongs to.
FLAGS is the string of command line flags passed to inkscape.
When MARKER is non-nil, insert the template FRMT at MARKER."
  (let ((default-directory dir))
    (call-process-shell-command (funcall ink-process-cmnd file flags)
                                nil ink-log-buffer)
    (ink--post-process file)
    (when marker
      (ink--insert-snippet file frmt marker))))

(defun ink--launch (file &optional marker)
  "Open FILE in inkscape and export it when inkscape closes.

When MARKER is non-nil, insert the figure's include snippet at
MARKER after the export.  The export flags and insert template
are taken from the current buffer's major mode."
  (let ((dir default-directory)
        (flags (mapconcat #'identity
                          (ink--mode-value ink-flags-options ink-flags)
                          " "))
        (frmt (and marker (ink--mode-value ink-insert-options ink-insert))))
    (make-process :name "inkscape"
                  :buffer (get-buffer-create ink-log-buffer)
                  :command (list "inkscape" file)
                  :stderr (get-buffer-create ink-log-buffer)
                  :sentinel
                  (lambda (process _event)
                    (cond ((and (eq (process-status process) 'exit)
                                (zerop (process-exit-status process)))
                           (ink--process file dir flags frmt marker))
                          ((memq (process-status process) '(exit signal))
                           (message "ink: inkscape exited abnormally, see %s"
                                    ink-log-buffer)))))))

(defun ink-edit-svg (fsvg)
  "Edit an existing svg file named FSVG.

The file is copied to the temporary directory and copied back
over the original when inkscape closes."
  (let ((file (ink--temp-file (file-name-sans-extension
                               (file-name-nondirectory fsvg)))))
    (copy-file fsvg file t)
    (ink--launch file)))

;;; Commands
;;;###autoload
(defun ink-make-figure (fig)
  "Make a new figure named FIG and insert it at point.

If the figure already exists, offer to edit it instead."
  (interactive "sFigure name: ")
  (when (string= fig "")
    (user-error "The figure needs a name"))
  (if (file-exists-p (ink--figure-file fig))
      (if (y-or-n-p (format "Figure %s already exists.  Edit it instead? " fig))
          (ink-edit-svg (ink--figure-file fig))
        (user-error "Figure %s already exists" fig))
    (let ((file (ink--temp-file fig)))
      (write-region ink-default-file nil file)
      (ink--launch file (point-marker)))))

;;;###autoload
(defun ink-edit-figure ()
  "Edit an existing figure or a related tex file."
  (interactive)
  (let* ((fdir (file-name-as-directory (expand-file-name ink-fig-dir)))
         (file (expand-file-name (read-file-name "Edit file: " fdir) fdir)))
    (if (string= (file-name-extension file) "svg")
        (ink-edit-svg file)
      (find-file file))))

;;;###autoload
(defun ink-duplicate-figure (fsvg fig)
  "Make a new figure named FIG from the existing figure FSVG.

The new figure is opened in inkscape and inserted at point when
inkscape closes.  The original figure is left untouched."
  (interactive
   (list (read-file-name "Duplicate figure: "
                         (file-name-as-directory (expand-file-name ink-fig-dir))
                         nil t)
         (read-string "New figure name: ")))
  (when (string= fig "")
    (user-error "The figure needs a name"))
  (when (file-exists-p (ink--figure-file fig))
    (user-error "Figure %s already exists" fig))
  (let ((file (ink--temp-file fig)))
    (copy-file (expand-file-name fsvg) file t)
    (ink--launch file (point-marker))))

(provide 'ink)
;;; ink.el ends here
