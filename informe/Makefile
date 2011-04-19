SOURCES=informe.tex
OBJS=
PDFs=$(SOURCES:.tex=.pdf)


all: $(PDFs)

%.pdf: %.tex $(OBJS)
	pdflatex $^
	pdflatex $^
	rm -f *.aux *.idx *.log *.out *.toc
	clear; echo 'PDF COMPILADO!'
        
clean:
	rm -f $(PDFs)
	
new: clean all

