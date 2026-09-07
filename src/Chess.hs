-----------------------------------------------------------------------------
-- | The rules of chess, pure and DOM-free: the board, move generation
-- (castling, en passant, promotion), legality via check detection, game
-- status including every draw rule, FEN in both directions, standard
-- algebraic notation, and perft for testing the generator.
--
-- Squares are @(file, rank)@ pairs with @(0,0)@ = a1 and @(7,7)@ = h8.
-- Every 'Piece' carries a stable id so the view can animate it between
-- squares; the id never affects the rules.
-----------------------------------------------------------------------------
module Chess
  ( -- * Types
    PieceType(..), Side(..), Piece(..), Square, Board
  , Castling(..), Move(..), Position(..)
  , Status(..), DrawReason(..)
    -- * Positions
  , startPosition, opponent, pawnDir, positionKey
  , fileChar, rankChar, squareName, parseSquare
    -- * Moves
  , legalMoves, movesFrom, applyMove, isCapture, isCastle
  , findKing, isInCheck, attacked
    -- * Status
  , status, insufficientMaterial
    -- * Notation
  , san, toFEN, fromFEN, perft
    -- * Material
  , pointValue, materialLeft
  ) where
-----------------------------------------------------------------------------
import           Data.Char (isDigit, ord, chr, toUpper)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe)
-----------------------------------------------------------------------------
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Show, Enum, Bounded)
-----------------------------------------------------------------------------
data Side = White | Black
  deriving (Eq, Ord, Show)
-----------------------------------------------------------------------------
data Piece = Piece
  { pieceColor :: Side
  , pieceType :: PieceType
  , pieceId :: Int    -- ^ identity for the view; ignored by the rules
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
type Square = (Int, Int)
-----------------------------------------------------------------------------
type Board = Map Square Piece
-----------------------------------------------------------------------------
data Castling = Castling
  { cWK :: Bool, cWQ :: Bool, cBK :: Bool, cBQ :: Bool
  } deriving (Eq, Ord, Show)
-----------------------------------------------------------------------------
data Move = Move
  { mvFrom :: Square
  , mvTo :: Square
  , mvPromo :: Maybe PieceType
  } deriving (Eq, Ord, Show)
-----------------------------------------------------------------------------
data Position = Position
  { posBoard :: Board
  , posTurn :: Side
  , posEP :: Maybe Square      -- ^ square behind a just-double-stepped pawn
  , posCastling :: Castling
  , posHalfmove :: Int         -- ^ plies since the last capture or pawn move
  , posFullmove :: Int
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
data DrawReason
  = Stalemate
  | InsufficientMaterial
  | FiftyMoves
  | Repetition
  deriving (Eq, Show)
-----------------------------------------------------------------------------
data Status
  = Ongoing
  | Check
  | Mate Side         -- ^ the winner
  | Draw DrawReason
  deriving (Eq, Show)
-----------------------------------------------------------------------------
opponent :: Side -> Side
opponent White = Black
opponent Black = White
-----------------------------------------------------------------------------
pawnDir :: Side -> Int
pawnDir White = 1
pawnDir Black = -1
-----------------------------------------------------------------------------
inBounds :: Square -> Bool
inBounds (f, r) = f >= 0 && f < 8 && r >= 0 && r < 8
-----------------------------------------------------------------------------
fileChar :: Int -> Char
fileChar f = chr (ord 'a' + f)
-----------------------------------------------------------------------------
rankChar :: Int -> Char
rankChar r = chr (ord '1' + r)
-----------------------------------------------------------------------------
squareName :: Square -> String
squareName (f, r) = [fileChar f, rankChar r]
-----------------------------------------------------------------------------
parseSquare :: String -> Maybe Square
parseSquare [f, r]
  | f >= 'a', f <= 'h', r >= '1', r <= '8' = Just (ord f - ord 'a', ord r - ord '1')
parseSquare _ = Nothing
-----------------------------------------------------------------------------
startPosition :: Position
startPosition = case fromFEN startFEN of
  Just p -> p
  Nothing -> error "startPosition: bad FEN"
  where
    startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
-----------------------------------------------------------------------------
-- | What matters for repetition: pieces, side to move, castling and en
-- passant rights (ids dropped).
positionKey :: Position -> (Map Square (Side, PieceType), Side, Castling, Maybe Square)
positionKey p =
  ( Map.map (\pc -> (pieceColor pc, pieceType pc)) (posBoard p)
  , posTurn p, posCastling p, posEP p )
-----------------------------------------------------------------------------
knightDeltas, kingDeltas, orthoDirs, diagDirs :: [(Int, Int)]
knightDeltas = [(1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)]
kingDeltas = [ (a, b) | a <- [-1 .. 1], b <- [-1 .. 1], (a, b) /= (0, 0) ]
orthoDirs = [(1,0),(-1,0),(0,1),(0,-1)]
diagDirs = [(1,1),(1,-1),(-1,1),(-1,-1)]
-----------------------------------------------------------------------------
step :: Square -> (Int, Int) -> Square
step (f, r) (df, dr) = (f + df, r + dr)
-----------------------------------------------------------------------------
-- | Is @sq@ attacked by any piece of side @by@?  Looks outward from the
-- square instead of scanning every enemy piece.
attacked :: Board -> Side -> Square -> Bool
attacked board by sq =
     any (holds Knight) (reach knightDeltas)
  || any (holds King) (reach kingDeltas)
  || any (holds Pawn) (reach [(-1, -d), (1, -d)])
  || any (ray [Rook, Queen]) orthoDirs
  || any (ray [Bishop, Queen]) diagDirs
  where
    d = pawnDir by
    reach ds = filter inBounds (map (step sq) ds)
    holds pt s = case Map.lookup s board of
      Just (Piece c t _) -> c == by && t == pt
      Nothing -> False
    ray pts dir = go (step sq dir)
      where
        go s
          | not (inBounds s) = False
          | otherwise = case Map.lookup s board of
              Nothing -> go (step s dir)
              Just (Piece c t _) -> c == by && t `elem` pts
-----------------------------------------------------------------------------
findKing :: Board -> Side -> Maybe Square
findKing board side =
  listToMaybe [ s | (s, Piece c King _) <- Map.toList board, c == side ]
-----------------------------------------------------------------------------
isInCheck :: Position -> Side -> Bool
isInCheck pos side = case findKing (posBoard pos) side of
  Nothing -> False
  Just k -> attacked (posBoard pos) (opponent side) k
-----------------------------------------------------------------------------
-- | Squares reachable along a ray: empties, then the first enemy.
slide :: Board -> Side -> Square -> (Int, Int) -> [Square]
slide board side from dir = go (step from dir)
  where
    go s
      | not (inBounds s) = []
      | otherwise = case Map.lookup s board of
          Nothing -> s : go (step s dir)
          Just (Piece c _ _) -> [ s | c /= side ]
-----------------------------------------------------------------------------
-- | Moves ignoring whether they leave the king in check.
pseudoMoves :: Position -> Square -> Piece -> [Move]
pseudoMoves pos from@(f, r) (Piece side pt _) = case pt of
  Pawn ->
    let d = pawnDir side
        startRank = if side == White then 1 else 6
        lastRank = if side == White then 7 else 0
        one = (f, r + d)
        two = (f, r + 2 * d)
        promos to
          | snd to == lastRank = [ Move from to (Just p) | p <- [Queen, Knight, Rook, Bishop] ]
          | otherwise = [ Move from to Nothing ]
        pushes =
          [ m | inBounds one, empty one, m <- promos one ]
          ++ [ Move from two Nothing | r == startRank, empty one, empty two ]
        captures =
          [ m | df <- [-1, 1], let to = (f + df, r + d), inBounds to
              , enemyAt to || Just to == posEP pos, m <- promos to ]
    in pushes ++ captures
  Knight -> [ Move from to Nothing | to <- jumps knightDeltas ]
  Bishop -> rays diagDirs
  Rook -> rays orthoDirs
  Queen -> rays (orthoDirs ++ diagDirs)
  King -> [ Move from to Nothing | to <- jumps kingDeltas ] ++ castles
  where
    board = posBoard pos
    empty s = Map.notMember s board
    enemyAt s = case Map.lookup s board of
      Just (Piece c _ _) -> c /= side
      Nothing -> False
    jumps ds = [ to | to <- filter inBounds (map (step from) ds), not (ownAt to) ]
    ownAt s = case Map.lookup s board of
      Just (Piece c _ _) -> c == side
      Nothing -> False
    rays dirs = [ Move from to Nothing | dir <- dirs, to <- slide board side from dir ]
    castles
      | from /= (4, home) = []
      | attacked board enemy from = []
      | otherwise =
          [ Move from (6, home) Nothing
          | kingSide, rookAt (7, home)
          , all empty [(5, home), (6, home)]
          , not (any (attacked board enemy) [(5, home), (6, home)]) ]
          ++
          [ Move from (2, home) Nothing
          | queenSide, rookAt (0, home)
          , all empty [(1, home), (2, home), (3, home)]
          , not (any (attacked board enemy) [(2, home), (3, home)]) ]
    home = if side == White then 0 else 7
    enemy = opponent side
    cr = posCastling pos
    kingSide = if side == White then cWK cr else cBK cr
    queenSide = if side == White then cWQ cr else cBQ cr
    rookAt s = case Map.lookup s board of
      Just (Piece c Rook _) -> c == side
      _ -> False
-----------------------------------------------------------------------------
-- | All legal moves for the side to move.  Only moves that could expose
-- the king (king moves, en passant, pieces on a line with the king, or
-- anything while in check) pay for a full check test.
legalMoves :: Position -> [Move]
legalMoves pos =
  [ m
  | (sq, pc) <- Map.toList board
  , pieceColor pc == side
  , m <- pseudoMoves pos sq pc
  , safe pc m
  ]
  where
    board = posBoard pos
    side = posTurn pos
    inCheck = isInCheck pos side
    kingSq = findKing board side
    aligned (f, r) = case kingSq of
      Nothing -> True
      Just (kf, kr) -> f == kf || r == kr || abs (f - kf) == abs (r - kr)
    safe pc m
      | inCheck || pieceType pc == King || aligned (mvFrom m) || isEnPassant pc m =
          not (isInCheck (applyMove pos m) side)
      | otherwise = True
    isEnPassant pc m = pieceType pc == Pawn && Just (mvTo m) == posEP pos
-----------------------------------------------------------------------------
movesFrom :: Position -> Square -> [Move]
movesFrom pos sq = filter ((== sq) . mvFrom) (legalMoves pos)
-----------------------------------------------------------------------------
isCapture :: Position -> Move -> Bool
isCapture pos m =
  Map.member (mvTo m) board
    || (fmap pieceType (Map.lookup (mvFrom m) board) == Just Pawn
          && Just (mvTo m) == posEP pos)
  where
    board = posBoard pos
-----------------------------------------------------------------------------
isCastle :: Position -> Move -> Bool
isCastle pos m =
  fmap pieceType (Map.lookup (mvFrom m) (posBoard pos)) == Just King
    && abs (fst (mvTo m) - fst (mvFrom m)) == 2
-----------------------------------------------------------------------------
-- | Play a move assumed legal.
applyMove :: Position -> Move -> Position
applyMove pos (Move from to promo) = Position
  { posBoard = board'
  , posTurn = opponent side
  , posEP = ep'
  , posCastling = castling'
  , posHalfmove = if pt == Pawn || captured then 0 else posHalfmove pos + 1
  , posFullmove = posFullmove pos + (if side == Black then 1 else 0)
  }
  where
    board = posBoard pos
    piece = case Map.lookup from board of
      Just p -> p
      Nothing -> error ("applyMove: empty square " ++ squareName from)
    side = pieceColor piece
    pt = pieceType piece
    captured = Map.member to board || enPassant
    enPassant = pt == Pawn && Just to == posEP pos
    moved = maybe piece (\p -> piece { pieceType = p }) promo
    placed = Map.insert to moved (Map.delete from board)
    afterEP
      | enPassant = Map.delete (fst to, snd from) placed
      | otherwise = placed
    board'
      | pt == King && abs (fst to - fst from) == 2 =
          let (rf, rt) = if fst to > fst from
                then ((7, snd from), (5, snd from))
                else ((0, snd from), (3, snd from))
          in case Map.lookup rf afterEP of
               Just rook -> Map.insert rt rook (Map.delete rf afterEP)
               Nothing -> afterEP
      | otherwise = afterEP
    ep'
      | pt == Pawn && abs (snd to - snd from) == 2 = Just (fst from, (snd from + snd to) `div` 2)
      | otherwise = Nothing
    cr = posCastling pos
    touched s = from == s || to == s
    castling' = Castling
      { cWK = cWK cr && not (touched (4, 0) || touched (7, 0))
      , cWQ = cWQ cr && not (touched (4, 0) || touched (0, 0))
      , cBK = cBK cr && not (touched (4, 7) || touched (7, 7))
      , cBQ = cBQ cr && not (touched (4, 7) || touched (0, 7))
      }
-----------------------------------------------------------------------------
-- | Neither side can ever mate: bare kings, a lone minor piece, or only
-- bishops that all stand on one square colour.
insufficientMaterial :: Board -> Bool
insufficientMaterial board =
  case rest of
    [] -> True
    [(_, Piece _ Knight _)] -> True
    _ | all isBishop rest -> length (foldr uniq [] (map shade rest)) == 1
    _ -> False
  where
    rest = [ (s, p) | (s, p) <- Map.toList board, pieceType p /= King ]
    isBishop (_, p) = pieceType p == Bishop
    shade ((f, r), _) = even (f + r)
    uniq x acc = if x `elem` acc then acc else x : acc
-----------------------------------------------------------------------------
-- | The status of a position given the positions that came before it.
status :: [Position] -> Position -> Status
status history pos
  | null (legalMoves pos) = if inCheck then Mate (opponent side) else Draw Stalemate
  | insufficientMaterial (posBoard pos) = Draw InsufficientMaterial
  | posHalfmove pos >= 100 = Draw FiftyMoves
  | seen >= 3 = Draw Repetition
  | inCheck = Check
  | otherwise = Ongoing
  where
    side = posTurn pos
    inCheck = isInCheck pos side
    key = positionKey pos
    seen = 1 + length (filter ((== key) . positionKey) history)
-----------------------------------------------------------------------------
-- | Standard algebraic notation for a legal move in this position.
san :: Position -> Move -> String
san pos m@(Move from to promo) = body ++ suffix
  where
    board = posBoard pos
    piece = case Map.lookup from board of
      Just p -> p
      Nothing -> error "san: empty square"
    pt = pieceType piece
    after = applyMove pos m
    suffix
      | not (isInCheck after (posTurn after)) = ""
      | null (legalMoves after) = "#"
      | otherwise = "+"
    capture = isCapture pos m
    promoText = maybe "" (\p -> '=' : [letter p]) promo
    body
      | isCastle pos m = if fst to > fst from then "O-O" else "O-O-O"
      | pt == Pawn =
          (if capture then [fileChar (fst from), 'x'] else "") ++ squareName to ++ promoText
      | otherwise =
          letter pt : disambiguation ++ (if capture then "x" else "") ++ squareName to
    rivals =
      [ mvFrom o
      | o <- legalMoves pos, mvTo o == to, mvFrom o /= from
      , fmap pieceType (Map.lookup (mvFrom o) board) == Just pt ]
    disambiguation
      | null rivals = ""
      | all ((/= fst from) . fst) rivals = [fileChar (fst from)]
      | all ((/= snd from) . snd) rivals = [rankChar (snd from)]
      | otherwise = squareName from
-----------------------------------------------------------------------------
letter :: PieceType -> Char
letter = \case
  Pawn -> 'P'; Knight -> 'N'; Bishop -> 'B'; Rook -> 'R'; Queen -> 'Q'; King -> 'K'
-----------------------------------------------------------------------------
fromLetter :: Char -> Maybe PieceType
fromLetter c = lookup (toUpper c) [ (letter p, p) | p <- [minBound .. maxBound] ]
-----------------------------------------------------------------------------
toFEN :: Position -> String
toFEN pos = unwords [ rows, turn, castles, ep, show (posHalfmove pos), show (posFullmove pos) ]
  where
    board = posBoard pos
    rows = foldr1 (\a b -> a ++ "/" ++ b) [ row r | r <- [7, 6 .. 0] ]
    row r = go 0 [ Map.lookup (f, r) board | f <- [0 .. 7] ]
    go n [] = digits n
    go n (Nothing : xs) = go (n + 1) xs
    go n (Just p : xs) = digits n ++ [glyph p] ++ go 0 xs
    digits 0 = ""
    digits n = show (n :: Int)
    glyph (Piece White t _) = letter t
    glyph (Piece Black t _) = toLowerC (letter t)
    toLowerC c = chr (ord c + 32)
    turn = if posTurn pos == White then "w" else "b"
    cr = posCastling pos
    castles = case [ c | (True, c) <- [ (cWK cr, 'K'), (cWQ cr, 'Q'), (cBK cr, 'k'), (cBQ cr, 'q') ] ] of
      [] -> "-"
      cs -> cs
    ep = maybe "-" squareName (posEP pos)
-----------------------------------------------------------------------------
fromFEN :: String -> Maybe Position
fromFEN input = case words input of
  (rows : turn : castles : ep : rest) -> do
    board <- parseRows rows
    side <- case turn of
      "w" -> Just White
      "b" -> Just Black
      _ -> Nothing
    epSq <- if ep == "-" then Just Nothing else Just <$> parseSquare ep
    let (half, full) = case rest of
          (h : f : _) | all isDigit h, all isDigit f -> (read h, read f)
          _ -> (0, 1)
    pure Position
      { posBoard = board
      , posTurn = side
      , posEP = epSq
      , posCastling = Castling
          { cWK = 'K' `elem` castles, cWQ = 'Q' `elem` castles
          , cBK = 'k' `elem` castles, cBQ = 'q' `elem` castles }
      , posHalfmove = half
      , posFullmove = full
      }
  _ -> Nothing
  where
    parseRows rows = do
      let ranks = splitOn '/' rows
      if length ranks /= 8 then Nothing else do
        placed <- sequence (zipWith parseRank [7, 6 .. 0] ranks)
        let pieces = zipWith (\i (s, c, t) -> (s, Piece c t i)) [0 ..] (concat placed)
        pure (Map.fromList pieces)
    parseRank r = go 0
      where
        go f [] = if f == 8 then Just [] else Nothing
        go f (c : cs)
          | isDigit c = go (f + (ord c - ord '0')) cs
          | f >= 8 = Nothing
          | otherwise = do
              t <- fromLetter c
              let side = if c >= 'a' then Black else White
              ((( f, r), side, t) :) <$> go (f + 1) cs
-----------------------------------------------------------------------------
splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
  (a, []) -> [a]
  (a, _ : b) -> a : splitOn sep b
-----------------------------------------------------------------------------
-- | Count leaf nodes at a depth: the standard move-generator test.
perft :: Int -> Position -> Int
perft 0 _ = 1
perft n pos = sum [ perft (n - 1) (applyMove pos m) | m <- legalMoves pos ]
-----------------------------------------------------------------------------
-- | The classic point values shown in the material tally.
pointValue :: PieceType -> Int
pointValue = \case
  Pawn -> 1; Knight -> 3; Bishop -> 3; Rook -> 5; Queen -> 9; King -> 0
-----------------------------------------------------------------------------
-- | How many of each piece a side still has on the board.
materialLeft :: Board -> Side -> Map PieceType Int
materialLeft board side = foldl' bump Map.empty (Map.elems board)
  where
    bump acc (Piece c t _)
      | c == side = Map.insertWith (+) t 1 acc
      | otherwise = acc
