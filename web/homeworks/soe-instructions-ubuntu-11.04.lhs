---
title: Installing SOE Graphics (Ubuntu 11.04)
---

To install SOE on Ubuntu 11.04 simply do this:

Step 1: Install GTK 
-------------------

Download and install the relevant ghci etc.

	$ sudo apt-get install libghc6-soegtk-dev

Step 2: Install SOE
-------------------

    $ wget http://cseweb.ucsd.edu/classes/wi12/cse230-a/static/SOE-cse230-wi12.zip
	$ unzip SOE-cse230-wi12.zip


Step 3: Test
------------

Next, suppose you have a Haskell file called `soe-test.lhs` with the following
source. 

> import Graphics.SOE.Gtk
>
> makeCircle (x,y) r = ellipse (x-r, y-r) (x+r, y+r)
>
> circles = reverse $ zipWith withColor colors bwcircles
>   where bwcircles = [makeCircle center (25*i) | i <- [1..(length colors)]]
>         colors    = [Red, Blue, Green, Cyan, Magenta, Yellow]
>         center    = (350, 225)  
>
> test = runGraphics $ do 
>   w <- openWindow "Test" (300,300)
>   drawInWindow w $ text (10,10) "Hello World" 
>   sequence_ $ map (drawInWindow w) circles
>   k <- getKey w
>   closeWindow w

To test that your install is working, run the following, which adds the SOE
modules to the *search path* of `ghci`

	$ /usr/bin/ghci -i./SOE/src

You should see something like

	GHCi, version 6.12.3: http://www.haskell.org/ghc/  :? for help
	Loading package ghc-prim ... linking ... done.
	Loading package integer-gmp ... linking ... done.
	Loading package base ... linking ... done.

Next, load up the file `foo.hs`

	Prelude> :load soe-test.lhs 
	[1 of 1] Compiling Main             ( soe-test.lhs, interpreted )
	Ok, modules loaded: Main.

Finally, test the code with the following

	*Main> test 

You should see a window that looks like this

![bullseye](/static/SOE-test.png)

(Hit any key over the window to close it.)
