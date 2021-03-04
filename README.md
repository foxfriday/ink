
# Ink.el

Insert images in a LaTeX document using Inkscape. You can insert a new figure at point using `ink-make-figure` or edit an existing figure or its associated TeX file with `ink-edit-figure`. The figure is added when you save your image and close Inkscape. By default the resulting images are saved in the figures subdirectory. So if your project is in `~/mytex`, the images are saved in `~/mytex/figures`. Of course, you can change that location, but please make sure that the path exists. This assumes you have Inkscape installed. And you should have `\usepackage{import}` in your preamble. This is pretty rough so use at your own risk.


## Troubleshooting

If your text doesn't show in the final document, you probably inserted flowed text. To make plain text you need to add the text with a single click instead of making a box. To convert flowing text to regular text go to the text menu and select convert to text.
