
# Ink.el

Insert images in a LaTeX document using Inkscape. You can insert a new figure at point using `ink-make-figure` or edit an existing figure or its associated TeX file with `ink-edit-figure`. The figure is added when you save your image and close Inkscape. By default the resulting images are saved in the figures subdirectory. So if your project is in `~/mytex`, the images are saved in `~/mytex/figures`. Of course, you can change that location, but please make sure that the path exists. This assumes you have Inkscape installed. And you should have `\usepackage{import}` in your preamble. This is pretty rough so use at your own risk.


## Troubleshooting

### Missing Text

If your text doesn't show in the final document, you probably inserted flowed text. To make plain text you need to add the text with a single click in Inkscape instead of making a text box.

### Unrecognized Flags

If you are working with an older version of Inkscape, you may need to make some changes to the flags used to convert the document, and the function used to make the command:

```
(setq ink-flags (list "--export-area-drawing"
                      "--export-dpi 300"
                      "--export-type=pdf"
                      "--export-latex"
                      "--export-overwrite"
                      "--export-pdf=%s"))

(defun ink-process-cmnd-092 (file flags)
  "Inkscape 0.92 export command"
  (let* ((pdf (concat (file-name-sans-extension file) ".pdf"))
         (nflags (format flags pdf)))
    (concat "inkscape" " " file " " nflags)))

(setq ink-process-cmnd 'ink-process-cmnd-092)
```
