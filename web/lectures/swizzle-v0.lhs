> import System
> import System.Environment
> import Data.Maybe 

Swizzling One Character
-----------------------

First make a lookup table or *association list* of characters; each
element is a list is a pair of the char and its swizzled version.

That is, we want a list that looks like:

~~~~~{.haskell}
  [('a', 't')
  ,('b', 'h')
  ,('c', 'e')
  ,('d', 'q')
  ,('e', 'u')
  , ...
  ]
~~~~~






How do we create it?

> makePairs :: [a] -> [b] -> [(a, b)]
> makePairs [] []           = [] 
> makePairs (c:cs) (cc:ccs) = (c,cc) : makePairs cs ccs 




> code :: [(Char, Char)]
> code = makePairs ['a' .. 'z'] "thequickbrownfxjmpsdvlazyg"
	 

Now, we can use the `code` to translate a **single** character.


So that, 

~~~~~{.haskell}
swizzleChar  'a' code == 't'     -- 'a' encoded as 't'
swizzleChar  'b' code == 'h'     -- 'b' encoded as 'h'
swizzleChar  'λ' code == 'λ'     -- non-lower case encoded as itself.
~~~~~

> swizzleChar c code = fromMaybe c (lookup c code)

> foo = bar 
> -- data Maybe a = Nothing | Just a








Representing Failure 
--------------------

It is common for functions to be *partial* by 

+ failing 
+ being undefined 

on some inputs. 

Instead of throwing an **exception** the more Haskelly solution is:

~~~~~{.haskell}
data Maybe a = Nothing    -- failure, no value  
             | Just a     -- success, with a value
~~~~~

Lets rewrite the above recursive `swizzleChar` to use `Maybe`

> findInList x kvs = undefined

**QUIZ** What is the type of `findInList` ?

Now lets rewrite `swizzleChar` with `findInList`

> swizzleChar' c code = findInList c code 

**QUIZ**: Will the above work?


**QUIZ**: Can you think of a simple way to check that each (lower case) 
character is in fact mapped to a distinct character, ie that there
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

> (|>) x f  = f x


> swizzleFile :: FilePath -> IO ()
> swizzleFile f = do d <- readFile f
>                    writeFile (f ++ ".swz") (swizzleContent d) 


swizMany :: [FilePath] -> IO ()


> swizMany files = map swizzleFile files



> smashActions        :: [IO ()] -> IO ()
> smashActions []     = return ()
> smashActions (a:as) = do a
>                          smashActions as
> 
>

do act1 
   act2











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

