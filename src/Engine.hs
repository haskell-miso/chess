-----------------------------------------------------------------------------
-- | The engine: negamax alpha-beta with a capture-only quiescence search,
-- MVV-LVA move ordering, and a material + piece-square evaluation that
-- switches the king to an endgame table once the queens are gone.
--
-- Three strengths share one search; the casual level also picks at
-- random among its near-best moves so it stays beatable and varied.
-- Search randomness comes in as a supply of uniform doubles so every
-- decision is reproducible.
-----------------------------------------------------------------------------
module Engine
  ( Level(..), levelDepth, levelName, levelBlurb
  , Score(..), Search(..), search, evaluate, mateScore
  ) where
-----------------------------------------------------------------------------
import           Data.List (sortBy)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing, Down(..))
-----------------------------------------------------------------------------
import           Chess
-----------------------------------------------------------------------------
data Level = Casual | Club | Expert | Master
  deriving (Eq, Ord, Show, Enum, Bounded)
-----------------------------------------------------------------------------
levelDepth :: Level -> Int
levelDepth = \case
  Casual -> 1
  Club -> 2
  Expert -> 3
  Master -> 4
-----------------------------------------------------------------------------
levelName :: Level -> String
levelName = \case
  Casual -> "Casual"
  Club -> "Club"
  Expert -> "Expert"
  Master -> "Master"
-----------------------------------------------------------------------------
levelBlurb :: Level -> String
levelBlurb = \case
  Casual -> "Looks one move ahead and sometimes wanders. Good for learning."
  Club -> "Two moves ahead, and it always finishes an exchange."
  Expert -> "Three moves deep with every capture resolved."
  Master -> "Four moves deep. Bring a plan and keep your pieces defended."
-----------------------------------------------------------------------------
-- | A score from White's point of view.
data Score
  = Centipawns Int
  | MateIn Side Int   -- ^ who is mating, and in how many moves
  deriving (Eq, Show)
-----------------------------------------------------------------------------
data Search = Search
  { sMove :: Move
  , sScore :: Score
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
mateScore :: Int
mateScore = 100000
-----------------------------------------------------------------------------
infinity :: Int
infinity = 10 * mateScore
-----------------------------------------------------------------------------
-- | Pick a move for the side to move, or 'Nothing' if it has none.
search :: Level -> [Double] -> Position -> Maybe Search
search level supply pos = case moves of
  [] -> Nothing
  first : _
    | level == Casual -> casual first
    | otherwise -> alphaBetaRoot first
  where
    depth = levelDepth level
    side = posTurn pos
    -- root moves: captures first, quiet moves shuffled by the supply
    moves = map snd $ sortBy (comparing fst)
      [ ((Down (orderKey pos m), r), m) | (m, r) <- zip (legalMoves pos) (cycle' supply) ]
    cycle' xs = if null xs then repeat 0.5 else cycle xs
    -- a mate found with @d@ plies of depth left is @depth - d@ plies away
    toScore v
      | v >= mateScore = MateIn side (movesAway (v - mateScore))
      | v <= negate mateScore = MateIn (opponent side) (movesAway (negate v - mateScore))
      | otherwise = Centipawns (if side == White then v else negate v)
    movesAway d = max 1 ((depth - d + 1) `div` 2)
    alphaBetaRoot first =
      let go [] best = best
          go (m : ms) (bestV, bestM) =
            let v = negate (negamax True (applyMove pos m) (depth - 1) (negate infinity) (negate bestV))
            in if v > bestV then go ms (v, m) else go ms (bestV, bestM)
          (topV, topM) = go moves (negate infinity - 1, first)
      in Just (Search topM (toScore topV))
    casual first =
      let scored = [ (negate (negamax False (applyMove pos m) 0 (negate infinity) infinity), m) | m <- moves ]
          best = maximum (map fst scored)
          near = [ sm | sm@(v, _) <- scored, v >= best - 40 ]
          (pickV, pickM) = case (near, drop 1 supply) of
            (_ : _, r : _) -> near !! min (length near - 1) (floor (r * fromIntegral (length near)))
            (n : _, []) -> n
            ([], _) -> (best, first)
      in Just (Search pickM (toScore pickV))
-----------------------------------------------------------------------------
-- | Scores from the mover's point of view; mates found nearer the root
-- score higher so the engine finishes rather than dallies.
negamax :: Bool -> Position -> Int -> Int -> Int -> Int
negamax quiet pos depth alpha beta
  | depth <= 0 && inCheck && null moves = negate mateScore
  | depth <= 0 = if quiet then quiesce pos alpha beta 5 else evaluateFor pos
  | null moves = if inCheck then negate (mateScore + depth) else 0
  | otherwise = loop moves alpha
  where
    inCheck = isInCheck pos (posTurn pos)
    moves = ordered pos (legalMoves pos)
    loop [] a = a
    loop (m : ms) a =
      let v = negate (negamax quiet (applyMove pos m) (depth - 1) (negate beta) (negate a))
          a' = max a v
      in if a' >= beta then a' else loop ms a'
-----------------------------------------------------------------------------
-- | Resolve captures so the leaf evaluation is not mid-exchange.
quiesce :: Position -> Int -> Int -> Int -> Int
quiesce pos alpha beta budget
  | budget <= 0 = stand
  | stand >= beta = stand
  | otherwise = loop captures (max alpha stand)
  where
    stand = evaluateFor pos
    captures = ordered pos [ m | m <- legalMoves pos, isCapture pos m ]
    loop [] a = a
    loop (m : ms) a =
      let v = negate (quiesce (applyMove pos m) (negate beta) (negate a) (budget - 1))
          a' = max a v
      in if a' >= beta then a' else loop ms a'
-----------------------------------------------------------------------------
ordered :: Position -> [Move] -> [Move]
ordered pos = sortBy (comparing (Down . orderKey pos))
-----------------------------------------------------------------------------
-- | Most valuable victim, least valuable attacker; promotions on top.
orderKey :: Position -> Move -> Int
orderKey pos (Move from to promo) = victim + promoBonus
  where
    board = posBoard pos
    attacker = maybe 0 (value . pieceType) (Map.lookup from board)
    victim = case Map.lookup to board of
      Just p -> 10 * value (pieceType p) - attacker
      Nothing
        | Just to == posEP pos && attackerIsPawn -> 10 * value Pawn - attacker
        | otherwise -> 0
    attackerIsPawn = fmap pieceType (Map.lookup from board) == Just Pawn
    promoBonus = maybe 0 ((* 10) . value) promo
-----------------------------------------------------------------------------
value :: PieceType -> Int
value = \case
  Pawn -> 100; Knight -> 320; Bishop -> 330; Rook -> 500; Queen -> 900; King -> 20000
-----------------------------------------------------------------------------
evaluateFor :: Position -> Int
evaluateFor pos = if posTurn pos == White then e else negate e
  where
    e = evaluate (posBoard pos)
-----------------------------------------------------------------------------
-- | Static evaluation in centipawns, positive for White.
evaluate :: Board -> Int
evaluate board = sum (map term pieces) + pair White - pair Black
  where
    pieces = Map.toList board
    endgame = null [ () | (_, Piece _ Queen _) <- pieces ]
      || sum [ value t | (_, Piece _ t _) <- pieces, t /= Pawn, t /= King ] <= 2600
    term (sq, Piece c t _) =
      let s = value t + pst endgame t c sq
      in if c == White then s else negate s
    pair c = if length [ () | (_, Piece c' Bishop _) <- pieces, c' == c ] >= 2 then 30 else 0
-----------------------------------------------------------------------------
-- | Piece-square bonus.  Tables are written from White's side with rank 8
-- on the first row; Black reads the same table mirrored.
pst :: Bool -> PieceType -> Side -> Square -> Int
pst endgame t side (f, r) = (table !! row) !! f
  where
    row = if side == White then 7 - r else r
    table = case t of
      Pawn -> pawnTable
      Knight -> knightTable
      Bishop -> bishopTable
      Rook -> rookTable
      Queen -> queenTable
      King -> if endgame then kingEndTable else kingTable
-----------------------------------------------------------------------------
pawnTable, knightTable, bishopTable, rookTable, queenTable, kingTable, kingEndTable :: [[Int]]
pawnTable =
  [ [ 0,  0,  0,  0,  0,  0,  0,  0]
  , [50, 50, 50, 50, 50, 50, 50, 50]
  , [10, 10, 20, 30, 30, 20, 10, 10]
  , [ 5,  5, 10, 25, 25, 10,  5,  5]
  , [ 0,  0,  0, 20, 20,  0,  0,  0]
  , [ 5, -5,-10,  0,  0,-10, -5,  5]
  , [ 5, 10, 10,-20,-20, 10, 10,  5]
  , [ 0,  0,  0,  0,  0,  0,  0,  0] ]
knightTable =
  [ [-50,-40,-30,-30,-30,-30,-40,-50]
  , [-40,-20,  0,  0,  0,  0,-20,-40]
  , [-30,  0, 10, 15, 15, 10,  0,-30]
  , [-30,  5, 15, 20, 20, 15,  5,-30]
  , [-30,  0, 15, 20, 20, 15,  0,-30]
  , [-30,  5, 10, 15, 15, 10,  5,-30]
  , [-40,-20,  0,  5,  5,  0,-20,-40]
  , [-50,-40,-30,-30,-30,-30,-40,-50] ]
bishopTable =
  [ [-20,-10,-10,-10,-10,-10,-10,-20]
  , [-10,  0,  0,  0,  0,  0,  0,-10]
  , [-10,  0,  5, 10, 10,  5,  0,-10]
  , [-10,  5,  5, 10, 10,  5,  5,-10]
  , [-10,  0, 10, 10, 10, 10,  0,-10]
  , [-10, 10, 10, 10, 10, 10, 10,-10]
  , [-10,  5,  0,  0,  0,  0,  5,-10]
  , [-20,-10,-10,-10,-10,-10,-10,-20] ]
rookTable =
  [ [  0,  0,  0,  0,  0,  0,  0,  0]
  , [  5, 10, 10, 10, 10, 10, 10,  5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [  0,  0,  0,  5,  5,  0,  0,  0] ]
queenTable =
  [ [-20,-10,-10, -5, -5,-10,-10,-20]
  , [-10,  0,  0,  0,  0,  0,  0,-10]
  , [-10,  0,  5,  5,  5,  5,  0,-10]
  , [ -5,  0,  5,  5,  5,  5,  0, -5]
  , [  0,  0,  5,  5,  5,  5,  0, -5]
  , [-10,  5,  5,  5,  5,  5,  0,-10]
  , [-10,  0,  5,  0,  0,  0,  0,-10]
  , [-20,-10,-10, -5, -5,-10,-10,-20] ]
kingTable =
  [ [-30,-40,-40,-50,-50,-40,-40,-30]
  , [-30,-40,-40,-50,-50,-40,-40,-30]
  , [-30,-40,-40,-50,-50,-40,-40,-30]
  , [-30,-40,-40,-50,-50,-40,-40,-30]
  , [-20,-30,-30,-40,-40,-30,-30,-20]
  , [-10,-20,-20,-20,-20,-20,-20,-10]
  , [ 20, 20,  0,  0,  0,  0, 20, 20]
  , [ 20, 30, 10,  0,  0, 10, 30, 20] ]
kingEndTable =
  [ [-50,-40,-30,-20,-20,-30,-40,-50]
  , [-30,-20,-10,  0,  0,-10,-20,-30]
  , [-30,-10, 20, 30, 30, 20,-10,-30]
  , [-30,-10, 30, 40, 40, 30,-10,-30]
  , [-30,-10, 30, 40, 40, 30,-10,-30]
  , [-30,-10, 20, 30, 30, 20,-10,-30]
  , [-30,-30,  0,  0,  0,  0,-30,-30]
  , [-50,-30,-30,-30,-30,-30,-30,-50] ]
