#	/home/astrand/Data/arabidopsis/paper1-analysis-figures/Makefile
#	$Modified:  Mon Aug 20 10:36:05 EDT 2012$

#	  /usr/local/skel/Makefile.tex
#	  $Modified: Tue Oct 27 10:41:38 1998 by astrand $


#	  targets

PAPERDOCUMENTS =		paper1-analysis-figures.tex
SLIDEDOCUMENTS =
DOCUMENTS =                     $(PAPERDOCUMENTS) $(SLIDEDOCUMENTS)
TEXTPAGES =		
FIGURES =		  
BIBFILES =		 

DVIFILES =		 $(DOCUMENTS:.tex=.dvi)
PSDOCS =		$(DOCUMENTS:.tex=.ps)
PDFDOCS =		  $(DOCUMENTS:.tex=.pdf)
PROOFS =		$(DOCUMENTS:.tex=.X)
PSPROOFS =		 $(DOCUMENTS:.tex=.Xps)
DRAFTS =		$(DOCUMENTS:.tex=.draft)
HTMLFILES =		$(DOCUMENTS:.tex=.html)
RCODENAME=		 $(PAPERDOCUMENTS:.tex=.R)
ODTNAME=		 $(DOCUMENTS:.tex=.odt)


#SLIDETEX=		$(DOCUMENTS:.tex=-slides.tex)
#SLIDEDVI=		$(DOCUMENTS:.tex=-slides.dvi)
#SLIDEPDF=		$(DOCUMENTS:.tex=-slides.pdf)
#SLIDEPS=		$(DOCUMENTS:.tex=-slides.ps)
#SLIDEBBL=		$(DOCUMENTS:.tex=-slides.bbl)
#SLIDEAUX=		$(DOCUMENTS:.tex=-slides.aux)


JUNK =		  *.aux *.[0-9]*.aux *.bbl *.[0-9]*.bbl *.blg \
			*.[0-9]*.blg *.dvi *.err *.glo *.idx *.lof \
			*.log *.lot *.toc *.tty *~ $(PSDOCS) body.tex\
			body-*.eps body-*.pdf *.tidx *.out

# uncomment  to reun the imagte conversion programs
# CONVERTIMAGE = YES

# add directories containing TeX input files
TEXINPUTS =		:.:

#add flags for Sweave and Stangle
SRCDEPOSIT =	    ../htdocs/source/

# add flags for latex2HTML
HTMLDIR =		  ../htdocs/latex2html
HTMLPRE =		  $(basename $(PSDOCS))
HTMLSPLIT =		5
HTMLFLAGS =		-info 0 -dir ${HTMLDIR} -prefix ${HTMLPRE} -split ${HTMLSPLIT}

include Makefile.knitr

# uncomment the following line to run BibTeX
paper1-analysis-figures.dvi: paper1-analysis-figures.tex paper1-analysis-figures.bbl
paper1-analysis-figures-slides.dvi: paper1-analysis-figures-slides.tex paper1-analysis-figures-slides.bbl





