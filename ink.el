;; ink.el
;; Insert images in a LaTeX document using inkscape. You can insert a new
;; figure at point using ink-make-figure or edit an existing figure with
;; ink-edit-figure. This is pretty rough so use at your own risk.
;; This assumes you have inkscape installed.

(defvar ink-fig-dir "figures"
  "Default image directory")

(defvar ink-flags (list "--export-area-drawing"
                        "--export-dpi 300"
                        "--export-type=pdf"
                        "--export-latex"
                        "--export-overwrite")
  "Default list of flags for inkscape.")

(defvar ink-latex "\n\\begin{figure}
    \\def\\svgwidth{\\linewidth}
    \\import{%s}{%s.pdf_tex}
    \\label{fig:%s}
    \\caption{}
\\end{figure}\n"
  "Default latex insert")

(defvar ink-temp-dir "temp"
  "Default name for the temporary directory")

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
     "Default file template")

(defun ink-post-process (tdir)
  "Move files to image directory and remove temp."
  (let* ((temp (expand-file-name ink-fig-dir default-directory))
         (idir (file-name-as-directory temp)))
    (copy-directory tdir idir nil t t)
    (delete-directory tdir t nil)
    (message "done")))

(defun ink-insert-tex (file)
  "Inserts tex string at point."
  (let* ((dname (file-name-directory file))
         (fname (file-name-nondirectory file))
         (name (file-name-sans-extension fname))
         (caption (downcase name))
         (ltex (format ink-latex dname name caption)))
    (message dname)
    (insert ltex)))

(defun ink-process ()
  "Use inkspace to create an image and tex."
  (let* ((tdir (expand-file-name ink-temp-dir default-directory))
         (files-all (directory-files tdir t "\\.svg$"))
         (flags (mapconcat 'identity ink-flags " ")))
    (dolist (file files-all (ink-post-process tdir))
      (shell-command (concat "inkscape " file " " flags))
      (ink-insert-tex file))))

(defun ink-sentinel (process event)
  "Wait for inkscape to close."
  (when (memq (process-status process) '(exit signal))
    (ink-process)))

(defun ink-make-figure (fig)
  "Make a new figure and insert it at point."
  (interactive "sFigure name: ")
  (let* ((log-buffer (get-buffer-create "*inky-log*"))
         (tdir (expand-file-name ink-temp-dir default-directory))
         (file (concat (file-name-as-directory tdir) fig ".svg")))
    (make-directory tdir t)
    (write-region ink-default-file nil file)
    (make-process :name "inksape"
                  :buffer log-buffer
                  :command (list "inkscape" file)
                  :stderr log-buffer
                  :sentinel 'ink-sentinel)))

(defun ink-edit-figure ()
  "Edit existing figure."
  (interactive)
  (let* ((fdir (expand-file-name ink-fig-dir default-directory))
         (fig (read-file-name "Figure: " fdir))
         (fname (file-name-nondirectory fig))
         (tdir (expand-file-name ink-temp-dir default-directory))
         (file (concat (file-name-as-directory tdir) fname))
         (log-buffer (get-buffer-create "*inky-log*")))
    (make-directory tdir t)
    (rename-file fig file)
    (make-process :name "inksape"
                  :buffer log-buffer
                  :command (list "inkscape" file)
                  :stderr log-buffer
                  :sentinel 'ink-sentinel)))

(provide 'ink)
