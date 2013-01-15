
> import System
> import Data.Maybe
> import Data.Char

> code = smalls ++ caps 
>   where smalls   = zip ['a' .. 'z'] shuffles 
>         caps     = zip ['A' .. 'Z'] (map toUpper shuffles) 
>         shuffles = "thequickbrownfxjmpsdvlazyg"

First, we will just make two operations for swizzling and unswizzling
characters.

> txChar kvs c  = fromMaybe c (lookup c kvs)
> swizzleChar   = txChar code 
> unswizzleChar = txChar [(y,x) | (x,y) <- code]

To (un/)swizzle one line, we will (un/)swizzle each character 
on the line. We simply make actual operation a parameter. 

> txLine op     = map op 

To swizzle a file, we will first reverse the lines in the file and 
then swizzle each line of the file. 

> txContent op  = unlines . map (txLine op) . reverse . lines

To actually transform a file

> txFile op f   = do d  <- readFile f
>                    writeFile (f ++ ".swz") (txContent op d) 

And to transform many files

> txFiles op = sequence_ . map (txFile op)

Finally, at the very top, the command line args tell us whether to
swizzle or unswizzle

 > main = do args <- getArgs 
 >           case args of 
 >             ("-s":files) -> txFiles swizzleChar files
 >             ("-u":files) -> txFiles unswizzleChar files
 >             (_)          -> putStrLn "usage: swizzle -s file1 file2 ... OR swizzle -u file1 file2 ..."



[1]: http://haskell.org/hoogle "Hoogle"
