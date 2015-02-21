import Fal hiding (between, pball, walls, paddle, paddleball)
import Animation (picToGraphic)
import qualified SOE as G
import Picture
import Data.Map
import Control.Monad.State hiding (when)

-- Pong
-- Two players, one on mouse, one on keyboard
-- Keeps score, first to five wins
-- Ball speed increases each round
-- Press key to start game

main = reactimate "paddleball" $ paddleball 0 0 2

maxscore = 5

paddleball p1score p2score vel =
  if p1score == maxscore then
    lift0 $ G.text (0, 0) "Player 1 wins!"
  else if p2score == maxscore then
    lift0 $ G.text (0, 0) "Player 2 wins!"
  else
  let (pb, p1scores, p2scores) = pball vel
      vel'                     = vel * 1.05 in
  lift0 (G.text (0, 0) $ show p1score ++ " vs. " ++ show p2score) `untilB` key ->>
    (lift1 picToGraphic $ (walls `over` paddle1 `over` paddle2 `over` pb)) `switch`
        ((p1scores ->> paddleball (p1score + 1) p2score vel') .|.
         (p2scores ->> paddleball p1score (p2score + 1) vel'))

walls = let left  = paint blue (translate (-2.2,0) (rec 0.05 3.4))
            right = paint blue (translate ( 2.2,0) (rec 0.05 3.4))
        in left `over` right

paddle y color source = paint color (translate (source, y) (rec 0.5 0.05))

keyUpE k = Event (\(uas,_) -> Prelude.map getkey uas)
  where getkey (Just (G.Key k' False)) | k' == k = Just ()
        getkey _                               = Nothing
        
kbSpeed = 2.5

keyboardVel = lift0 0 `switch` key =>> \k ->
  case k of
    'a' -> lift0 (-kbSpeed) `untilB` (keyUpE 'a') ->> lift0 0
    'd' -> lift0 kbSpeed `untilB` (keyUpE 'd') ->> lift0 0
    _   -> lift0 0

keyboardPos = integral keyboardVel

-- keyboardPos = lift0 0 `switch` (key `snapshot` keyboardPos =>> \(c, p) ->
--                                  case c of 'a' -> lift0 $ p - 0.3
--                                            'd' -> lift0 $ p + 0.3
--                                            _   -> lift0 p)

p2input = fst mouse
p1input = keyboardPos

paddle1 = paddle (-1.7) red p1input

paddle2 = paddle 1.7 green p2input

pball vel =
  let xvel    = vel `stepAccum` xbounce ->> negate
      p1score = when $ ypos >* 2.5
      p2score = when $ ypos <* -2.5
      xpos    = integral xvel
      xbounce = when (xpos >*  2 ||* xpos <* -2)
      yvel    = vel `stepAccum` ybounce ->> negate
      ypos    = integral yvel
      ybounce = when ((ypos    `between` (-2.0,-1.5) &&*
                       p1input `between` (xpos-0.25,xpos+0.25))
                      ||* (ypos    `between` (1.5, 2.0) &&*
                           p2input `between` (xpos-0.25,xpos+0.25)))
  in (paint yellow (translate (xpos, ypos) (ell 0.2 0.2)), p1score, p2score)

x `between` (a,b) = x >* a &&* x <* b

