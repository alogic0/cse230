---
title: Installing SOE Graphics (MAC OS 10.6 (and above ?))
---

### SOE crashes `ghci` on MacOS 

But you can just build the single standalone applications and run them from
the terminal instead.


To install SOE on Ubuntu 12.04 simply do the following.

1. Install [Haskell Platform](http://www.haskell.org/platform/mac.html)

2. Install the Graphics Libraries

~~~~~
$ cabal install OpenGL
$ cabal install GLFW 
~~~~~

3. Grab [SOE](/static/SOE-CSE230-wi13.tgz)

~~~~~
$ wget http://cseweb.ucsd.edu/classes/wi13/cse230-a/static/SOE-CSE230-wi13.tgz
$ tar -zxvf SOE-CSE230-wi13.tgz
~~~~~

4. Grab and load [soe-test.hs](/static/soe-test.hs) 

5. To test your install, build the application with the extended *search path* 

~~~~~
$ ghc -i./SOE/src --make soe-test.hs
~~~~~

6. Finally, test the code with the following

~~~~~
$ ./soe-test 
~~~~~

You should see a window that looks like this

![bullseye](/static/SOE-test.png)

(Hit any key over the window to close it.)
