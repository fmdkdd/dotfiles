# Synctex allows one to jump to from the PDF in Zathura to the source in Emacs
# by Ctrl+click in the PDF.

# Tell latexmk to use Zathura as a previewer, and run emacsclient as the Synctex
# editor.
$pdflatex='pdflatex -synctex=1 %O %B';
$pdf_previewer = 'exec zathura -x \'emacsclient --no-wait +%{line} %{input}\' %O %S';
