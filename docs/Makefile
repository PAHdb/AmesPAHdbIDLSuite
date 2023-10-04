# Makefile to generate IDL-docs for the NASA Ames PAH IR Spectroscopic
# Database

IDLCMD = /Users/boersma/Applications/harris/idl89/bin/idl

docs:

#	wkhtmltopdf --enable-internal-links --enable-external-links --enable-toc-back-links --page-size Letter guide.html guide.pdf
	rm -rf html latex

	@$(IDLCMD) -e "idldoc,root='../',output='html', \
                       template_location='readthedocs', \
                       format_style='rst',markup_style='rst', \
                       index_level=1,user=1, \
                       title='AmesPAHdbIDLSuite Manual', \
                       comment_style='html', \
                       statistics=0, \
                       subtitle='NASA Ames PAH IR Spectroscopic Database'"
	/bin/cp -f -r static/resources/ html/idldoc-resources/
	/bin/cp -f -r static/html/ html/

#	@$(IDLCMD) -e "idldoc,root='../',output='latex', \
#                       format_style='rst',markup_style='rst', \
#                       index_level=1,user=0, \
#                       title='AmesPAHdbIDLSuite Manual', \
#                       comment_style='latex', \
#                       statistics=0, \
#                       subtitle='NASA Ames PAH IR Spectroscopic Database'"

#	cd latex; pdflatex index.tex; pdflatex index.tex; mv index.pdf ../api.pdf
