--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Arrow ((>>>))
import           Hakyll
--------------------------------------------------------------------------------

copyDir = do route   idRoute
             compile copyFileCompiler

main :: IO ()
main = hakyll $ do
    match "static/*"     copyDir
    --match "static/pa1/*" copyDir
    --match "static/pa2/*" copyDir
    --match "static/pa3/*" copyDir
    --match "static/pa4/*" copyDir
    --match "static/pa5/*" copyDir
    --match "static/pa6/*" copyDir
    --match "static/pa7/*" copyDir

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*"   $ compile templateCompiler
    match "lectures/*"    $ myMakeHTML
    -- match "slides/*"      $ myMakeHTML
    match "homeworks/*"   $ myMakeHTML
    match "final/*.lhs"       $ myMakeHTML
    match (fromList tops) $ myMakeHTML

tops = [ "index.markdown"
       , "grades.markdown"
       , "lectures.markdown"
       , "links.markdown"
       , "assignments.markdown"]

myMakeHTML 
  = do route   $ setExtension "html"
       -- route   $ setExtension "lhs"
       compile $ pandocCompiler
         >>= loadAndApplyTemplate "templates/default.html" defaultContext
         >>= relativizeUrls
