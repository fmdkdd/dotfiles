# Synctex allows one to jump to from the PDF in Zathura to the source in Emacs
# by Ctrl+click in the PDF.

# Tell latexmk to use Zathura as a previewer, and run emacsclient as the Synctex
# editor.
$pdf_previewer = 'exec zathura --synctex -x \'emacsclient --no-wait +%{line} %{input}\' %O %S';
