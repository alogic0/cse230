> import System

Swizzling One Character
-----------------------

First make a lookup table or *association list* of characters; each
element is a list is a pair of the char and its swizzled version.

> code :: [(Char, Char)]
> code = zip ['a' .. 'z'] "thequickbrownfxjmpsdvlazyg"
	  
> swizzleChar :: Char -> Char
> swizzleChar c = lookup c code of
>   Just c' -> c'
>   Nothing -> c



Can you think of a simple way to check that each (lower case) 
character is infact mapped to a distinct character, ie that there
are no collisions?

Swizzling One Line
------------------

To swizzle one line, we will swizzle each character on the line.

> swizzleLine :: String -> String
> swizzleLine []     = []
> swizzleLine (c:cs) = (swizzleChar c) : (swizzleLine cs)


Swizzling Many Lines
--------------------

To swizzle a file, we will first reverse the lines in the file and then 
swizzle each line of the file. 

> swizzleContent :: String -> String
> swizzleContent fileString = 
>   let fileLines        = lines fileString
>       fileLinesRev     = reverse fileLines
>       fileLinesRevSwiz = swizzleLines fileLinesRev
>       fileStringSwiz   = unlines fileLinesRevSwiz
>   in fileStringSwiz

where the auxiliary function `swizzleLines` 
simply swizzles the content of each line.

> swizzleLines :: [String] -> [String]
> swizzleLines []     = []
> swizzleLines (l:ls) = (swizzleLine l) : (swizzleLines ls)

Doing the IO
------------

Of course, its all very well to manipulate strings, but in the end,
the rubber must hit the road. The next function takes a filename as
input and returns an action that corresponds to the swizzling of the 
file.

> swizzleFile :: FilePath -> IO ()
> swizzleFile f = do d <- readFile f
>                    writeFile (f ++ ".swz") (swizzleContent d) 


Hmm, it would be nice to be able to swizzle many files at one shot.

> swizzleFiles :: [FilePath] -> IO ()
> swizzleFiles []     = return ()
> swizzleFiles (f:fs) = do swizzleFile f 
>                          swizzleFiles fs

Finally, we put it all together.

> main = do files <- getArgs 
>           swizzleFiles files

Note that except for the very end, the code is completely pure. 
Merely by inspecting the function's type we can know that it 
doesnt so much as breathe on the filesystem. However, as we will 
see, this code is full of superfluous recursion. In the [next
version](swizzle-v1.html) we will eliminate the recursion by
spotting and applying the right computation patterns.

