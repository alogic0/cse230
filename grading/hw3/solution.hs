module Solution where

import qualified Hw3 as H
-- import Fal hiding (between, pball, walls, paddle, paddleball)
-- import Animation (picToGraphic)
-- import qualified SOE as G
-- import Picture
import Data.Map
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String

-- Pong
-- Two players, one on mouse, one on keyboard
-- Keeps score, first to five wins
-- Ball speed increases each round
-- Press key to start game

-- main = reactimate "paddleball" $ paddleball 0 0 2

-- maxscore = 5

-- paddleball p1score p2score vel =
--   if p1score == maxscore then
--     lift0 $ G.text (0, 0) "Player 1 wins!"
--   else if p2score == maxscore then
--     lift0 $ G.text (0, 0) "Player 2 wins!"
--   else
--   let (pb, p1scores, p2scores) = pball vel
--       vel'                     = vel * 1.05 in
--   lift0 (G.text (0, 0) $ show p1score ++ " vs. " ++ show p2score) `untilB` key ->>
--     (lift1 picToGraphic $ (walls `over` paddle1 `over` paddle2 `over` pb)) `switch`
--         ((p1scores ->> paddleball (p1score + 1) p2score vel') .|.
--          (p2scores ->> paddleball p1score (p2score + 1) vel'))

-- walls = let left  = paint blue (translate (-2.2,0) (rec 0.05 3.4))
--             right = paint blue (translate ( 2.2,0) (rec 0.05 3.4))
--         in left `over` right

-- paddle y color source = paint color (translate (source, y) (rec 0.5 0.05))

-- keyUpE k = Event (\(uas,_) -> Prelude.map getkey uas)
--   where getkey (Just (G.Key k' False)) | k' == k = Just ()
--         getkey _                               = Nothing
        
-- kbSpeed = 2.5

-- keyboardVel = lift0 0 `switch` key =>> \k ->
--   case k of
--     'a' -> lift0 (-kbSpeed) `untilB` (keyUpE 'a') ->> lift0 0
--     'd' -> lift0 kbSpeed `untilB` (keyUpE 'd') ->> lift0 0
--     _   -> lift0 0

-- keyboardPos = integral keyboardVel

-- -- keyboardPos = lift0 0 `switch` (key `snapshot` keyboardPos =>> \(c, p) ->
-- --                                  case c of 'a' -> lift0 $ p - 0.3
-- --                                            'd' -> lift0 $ p + 0.3
-- --                                            _   -> lift0 p)

-- p2input = fst mouse
-- p1input = keyboardPos

-- paddle1 = paddle (-1.7) red p1input

-- paddle2 = paddle 1.7 green p2input

-- pball vel =
--   let xvel    = vel `stepAccum` xbounce ->> negate
--       p1score = when $ ypos >* 2.5
--       p2score = when $ ypos <* -2.5
--       xpos    = integral xvel
--       xbounce = when (xpos >*  2 ||* xpos <* -2)
--       yvel    = vel `stepAccum` ybounce ->> negate
--       ypos    = integral yvel
--       ybounce = when ((ypos    `between` (-2.0,-1.5) &&*
--                        p1input `between` (xpos-0.25,xpos+0.25))
--                       ||* (ypos    `between` (1.5, 2.0) &&*
--                            p2input `between` (xpos-0.25,xpos+0.25)))
--   in (paint yellow (translate (xpos, ypos) (ell 0.2 0.2)), p1score, p2score)

-- x `between` (a,b) = x >* a &&* x <* b

-- While Language Evaluator

intOp :: (Int -> Int -> Int) -> H.Value -> H.Value -> H.Value
intOp op (H.IntVal x) (H.IntVal y) = H.IntVal $ x `op` y
intOp _  _            _              = H.IntVal 0

boolOp :: (Int -> Int -> Bool) -> H.Value -> H.Value -> H.Value
boolOp op (H.IntVal x) (H.IntVal y) = H.BoolVal $ x `op` y
boolOp _  _            _            = H.BoolVal False

-- Note: we don't have exceptions yet, so if a variable is not found,
-- simply return value 0. (Future homework: add this as a case where
-- exceptions would be thrown. Ditto type errors.)
evalE :: H.Expression -> State H.Store H.Value
evalE (H.Var x)      = get >>= return . findWithDefault (H.IntVal 0) x
evalE (H.Val v)      = return v
evalE (H.Op o e1 e2) = (return $ semantics o) `ap` evalE e1 `ap` evalE e2
   where semantics H.Plus   = intOp (+)
         semantics H.Minus  = intOp (-)
         semantics H.Times  = intOp (*)
         semantics H.Divide = intOp (div)
         semantics H.Gt     = boolOp (>)
         semantics H.Ge     = boolOp (>=)
         semantics H.Lt     = boolOp (<)
         semantics H.Le     = boolOp (<=)

evalS :: H.Statement -> State H.Store ()
evalS w@(H.While e s)    = evalS (H.If e (H.Sequence s w) H.Skip)
evalS H.Skip             = return ()
evalS (H.Sequence s1 s2) = evalS s1 >> evalS s2
evalS (H.Assign x e )    = do v <- evalE e
                              m <- get
                              put $ insert x v m
                              return ()
evalS (H.If e s1 s2) = do v <- evalE e
                          case v of H.BoolVal True  -> evalS s1
                                    H.BoolVal False -> evalS s2
                                    _               -> return ()

printStore :: H.Store -> IO ()
printStore e = do putStrLn "Environment:"
                  putStrLn $ show e

execS :: H.Statement -> H.Store -> H.Store
execS s = execState (evalS s)

-- Running the program should print the value of all variables
-- at the end of execution.
run :: H.Statement -> IO ()
run = printStore . flip execS empty

-- User-Facing While Parser and Evaluator

intP :: Parser H.Value
intP = many1 digit >>= return . H.IntVal . read

constP :: String -> a -> Parser a
constP s x = string s >> return x

boolP :: Parser H.Value
boolP = (constP "true" $ H.BoolVal True) <|> (constP "false" $ H.BoolVal True)

valueP :: Parser H.Value
valueP = intP <|> boolP

varP :: Parser H.Variable
varP = many1 upper

variableExpr :: Parser H.Expression
variableExpr = varP >>= return . H.Var

opP :: Parser H.Bop
opP =     constP "+"  H.Plus
      <|> constP "-"  H.Minus
      <|> constP "*"  H.Times
      <|> constP "/"  H.Divide
      <|> constP ">"  H.Gt
      <|> constP ">=" H.Ge
      <|> constP "<"  H.Lt
      <|> constP "<=" H.Le

valExpr :: Parser H.Expression
valExpr = valueP >>= return . H.Val

parenExpr :: Parser H.Expression
parenExpr = do string "("
               e <- exprP
               string ")"
               return e

baseExpr :: Parser H.Expression
baseExpr = valExpr <|> variableExpr <|> parenExpr

-- This is contorted to avoid left recursion.
-- One should be able to avoid this using:
--  - the Expr module in Parsec
--  - the chain* functions in Parsec
--  - the try combinator
exprOp :: H.Expression -> Parser H.Expression
exprOp e1 = do op <- opP
               spaces
               e2 <- exprP
               return $ H.Op op e1 e2

exprP :: Parser H.Expression
exprP = do e1 <- baseExpr
           spaces
           exprOp e1 <|> return e1

assignStmt :: Parser H.Statement
assignStmt = do v <- varP
                spaces; string ":="; spaces
                e <- exprP
                return $ H.Assign v e

ifStmt :: Parser H.Statement
ifStmt = do string "if"; spaces
            e <- exprP
            spaces; string "then"; spaces
            s1 <- statementP
            spaces; string "else"; spaces
            s2 <- statementP
            spaces; string "endif"
            return $ H.If e s1 s2

whileStmt :: Parser H.Statement
whileStmt = do string "while"; spaces
               e <- exprP
               spaces; string "do"; spaces
               s <- statementP
               spaces; string "endwhile"
               return $ H.While e s

skipStmt :: Parser H.Statement
skipStmt = string "skip" >> return H.Skip

baseStmt :: Parser H.Statement
baseStmt = assignStmt <|> ifStmt <|> whileStmt <|> skipStmt

seqStatement :: H.Statement -> Parser H.Statement
seqStatement s1 = do char ';'; spaces
                     s2 <- statementP
                     return $ H.Sequence s1 s2

statementP :: Parser H.Statement
statementP = do s1 <- baseStmt
                seqStatement s1 <|> return s1

runFile s = do p <- parseFromFile statementP s
               case p of
                 Left err   -> print err
                 Right stmt -> run stmt
