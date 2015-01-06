RSYNC=$(shell pwd)/sync.sh
SITEUSER=rjhala@login.eng.ucsd.edu
SITEPATH=/cse/htdocs/classes/wi15/cse230-a
SITE=$(SITEUSER):$(SITEPATH)
remoteuser=rjhala
remotedir=/cse/htdocs/classes/wi15/cse230-a
remotehost=login.eng.ucsd.edu

all: website
	./website rebuild

website: website.hs
	ghc --make website

clean:
	rm -rf *.hi *.o .*.swp .*.swo website _site/ _cache/

rsync:
	$(RSYNC) _site/ $(remoteuser) $(remotehost) $(remotedir)

update:
	cp css/syntax-rj.css _site/css/syntax.css
	cp lectures/*.lhs _site/lectures/
	cp -p -r slides _site/
	cp homeworks/*.lhs _site/homeworks/
  scp -r _site/* $(SITEUSER):$(SITEPATH)
	ssh $(SITEUSER) "chmod -R g+w $(SITEPATH) && chmod -R ugo+r $(SITEPATH)"

slides:
	cd slides && make && cd ..

