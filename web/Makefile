
SITEUSER=rjhala@csefast.ucsd.edu
SITEPATH=/net/cs/htdocs/classes/wi14/cse230-a/

all: hakyll
	./hakyll rebuild

hakyll:
	ghc --make hakyll

clean:
	rm -rf *.hi *.o .*.swp .*.swo hakyll _site/ _cache/

update:
	cp css/syntax-rj.css _site/css/syntax.css
	cp lectures/*.lhs _site/lectures/
	cp -p -r slides _site/
	cp homeworks/*.lhs _site/homeworks/
	scp -r _site/* $(SITEUSER):$(SITEPATH)
	ssh $(SITEUSER) "chmod -R g+w $(SITEPATH) && chmod -R ugo+r $(SITEPATH)"


slides:
	cd slides && make && cd ..
