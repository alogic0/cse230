all:
	./hakyll rebuild

hakyll:
	ghc --make hakyll

clean:
	rm -rf *.hi *.o .*.swp .*.swo hakyll _site/ _cache/

update:
	cp css/syntax-rj.css _site/css/syntax.css
	cp lectures/*.lhs _site/lectures/
	cp homeworks/*.lhs _site/homeworks/
	scp -r _site/* rjhala@csefast.ucsd.edu:/net/cs/htdocs/classes/wi12/cse230-a/
	ssh rjhala@csefast.ucsd.edu "chmod -R g+w /net/cs/htdocs/classes/wi12/cse230-a/ && chmod -R ugo+r /net/cs/htdocs/classes/wi12/cse230-a/"

