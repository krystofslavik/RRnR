all: latex grayscale

latex:
	cd latex_src && pdflatex diplomka.tex && bibtex diplomka.aux && pdflatex diplomka.tex && pdflatex diplomka.tex && cp diplomka.pdf ..

grayscale:
	gs -sOutputFile=diplomka_grayscale.pdf -sDEVICE=pdfwrite -sColorConversionStrategy=Gray -dProcessColorModel=/DeviceGray -dCompatibilityLevel=1.4 -dAutoRotatePages=/None -dNOPAUSE -dBATCH diplomka.pdf
