-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Concurrent    (threadDelay)
import           Data.List             (sortBy, foldl', minimumBy)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe, listToMaybe, mapMaybe)
import           Data.Ord              (comparing, Down(..))
-----------------------------------------------------------------------------
import           Miso hiding ((!!))
import           Miso.FFI.QQ (js)
import qualified Miso.CSS              as CSS
import           Miso.CSS.Color
import           Miso.String           (MisoString, ms)
import qualified Miso.Html             as H
import qualified Miso.Html.Property    as HP
import qualified Miso.Svg              as SVG
import qualified Miso.Svg.Property     as SP
-----------------------------------------------------------------------------
-- CHESS TYPES
-----------------------------------------------------------------------------

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Show)

data Side = White | Black
  deriving (Eq, Ord, Show)

data Piece = Piece { pieceColor :: Side, pieceType :: PieceType }
  deriving (Eq, Show)

-- (file 0-7, rank 0-7); (0,0)=a1, (7,7)=h8
type Square = (Int, Int)
type Board  = Map Square Piece

data CastleRights = CastleRights
  { crWK :: Bool, crWQ :: Bool
  , crBK :: Bool, crBQ :: Bool
  } deriving (Eq, Show)

data GameStatus
  = Playing | InCheck
  | Checkmate Side   -- side that WON
  | Stalemate
  deriving (Eq, Show)

data ChessMove = ChessMove
  { cmFrom  :: Square
  , cmTo    :: Square
  , cmPromo :: Maybe PieceType
  } deriving (Eq, Show)

data GameState = GameState
  { gsBoard  :: Board
  , gsTurn   :: Side
  , gsEP     :: Maybe Square
  , gsCastle :: CastleRights
  } deriving (Eq, Show)

-----------------------------------------------------------------------------
-- MODEL / ACTION
-----------------------------------------------------------------------------

data Model = Model
  { mGameState  :: GameState
  , mSelected   :: Maybe Square
  , mLegalDests :: [Square]
  , mLastMove   :: Maybe (Square, Square)
  , mStatus     :: GameStatus
  , mWhiteCapt  :: [PieceType]
  , mBlackCapt  :: [PieceType]
  , mThinking   :: Bool
  } deriving (Eq, Show)

data Action
  = ClickSquare Square
  | ComputerMove
  | NewGame
  | FullScreen
  | NoOp
  deriving (Show, Eq)

-----------------------------------------------------------------------------
-- INITIAL POSITION
-----------------------------------------------------------------------------

backRank :: [PieceType]
backRank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

initialBoard :: Board
initialBoard = Map.fromList $
  [ ((f,0), Piece White pt) | (f,pt) <- zip [0..7] backRank ] ++
  [ ((f,1), Piece White Pawn) | f <- [0..7] ] ++
  [ ((f,6), Piece Black Pawn) | f <- [0..7] ] ++
  [ ((f,7), Piece Black pt) | (f,pt) <- zip [0..7] backRank ]

initialGS :: GameState
initialGS = GameState initialBoard White Nothing (CastleRights True True True True)

initModel :: Model
initModel = Model
  { mGameState  = initialGS
  , mSelected   = Nothing
  , mLegalDests = []
  , mLastMove   = Nothing
  , mStatus     = Playing
  , mWhiteCapt  = []
  , mBlackCapt  = []
  , mThinking   = False
  }

-----------------------------------------------------------------------------
-- CHESS LOGIC
-----------------------------------------------------------------------------

opponent :: Side -> Side
opponent White = Black
opponent Black = White

inBounds :: Square -> Bool
inBounds (f,r) = f >= 0 && f <= 7 && r >= 0 && r <= 7

slideSquares :: Board -> Square -> (Int,Int) -> [Square]
slideSquares board (f,r) (df,dr) = go (f+df, r+dr)
  where
    go pos
      | not (inBounds pos) = []
      | Map.member pos board = [pos]
      | otherwise = pos : go (let (pf,pr) = pos in (pf+df, pr+dr))

attackSquares :: Board -> Square -> Piece -> [Square]
attackSquares board (f,r) (Piece side pt) = case pt of
  Pawn ->
    let d = if side == White then 1 else -1
    in filter inBounds [(f-1,r+d),(f+1,r+d)]
  Knight ->
    filter inBounds [(f+a,r+b)|(a,b)<-[(1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)]]
  Bishop -> concatMap (slideSquares board (f,r)) [(-1,-1),(-1,1),(1,-1),(1,1)]
  Rook   -> concatMap (slideSquares board (f,r)) [(-1,0),(1,0),(0,-1),(0,1)]
  Queen  -> concatMap (slideSquares board (f,r))
              [(-1,-1),(-1,1),(1,-1),(1,1),(-1,0),(1,0),(0,-1),(0,1)]
  King   -> filter inBounds [(f+a,r+b)|a <- [-1..1],b <- [-1..1], (a,b)/=(0,0)]

isAttackedBy :: Board -> Side -> Square -> Bool
isAttackedBy board atSide sq =
  any (\(s,pc) -> pieceColor pc == atSide && sq `elem` attackSquares board s pc)
      (Map.toList board)

findKing :: Board -> Side -> Maybe Square
findKing board side =
  listToMaybe [sq |(sq, Piece s King)<-Map.toList board,s==side]

isInCheck :: GameState -> Side -> Bool
isInCheck gs side =
  case findKing (gsBoard gs) side of
    Nothing -> False
    Just kSq -> isAttackedBy (gsBoard gs) (opponent side) kSq

slideTargets :: Board -> Square -> Side -> (Int,Int) -> [Square]
slideTargets board (f,r) side (df,dr) = go (f+df, r+dr)
  where
    go pos
      | not (inBounds pos) = []
      | otherwise = case Map.lookup pos board of
          Nothing          -> pos : go (let (pf,pr) = pos in (pf+df,pr+dr))
          Just (Piece s _) -> if s == side then [] else [pos]

pseudoMoves :: GameState -> Square -> Piece -> [ChessMove]
pseudoMoves gs sq@(f,r) (Piece side pt) = case pt of
  Pawn ->
    let board     = gsBoard gs
        d         = if side == White then 1 else -1
        startRank = if side == White then 1 else 6
        promoRank = if side == White then 7 else 0
        mkMv to   = ChessMove sq to (if snd to == promoRank then Just Queen else Nothing)
        fwd       = (f, r+d)
        fwd2      = (f, r+2*d)
        step1 = [mkMv fwd  | inBounds fwd,  Map.notMember fwd  board]
        step2 = [mkMv fwd2 | r == startRank, Map.notMember fwd board
                            , inBounds fwd2, Map.notMember fwd2 board]
        caps  = [mkMv cap  | df <- [-1,1], let cap=(f+df,r+d)
                            , inBounds cap
                            , case Map.lookup cap board of
                                Just (Piece s _) -> s /= side
                                Nothing          -> False]
        epCap = [ChessMove sq epSq Nothing
                | Just epSq <- [gsEP gs]
                , snd epSq == r+d
                , abs (fst epSq - f) == 1]
    in step1 ++ step2 ++ caps ++ epCap

  Knight ->
    [ChessMove sq to Nothing
    |(a,b)<-[(1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)]
    , let to=(f+a,r+b), inBounds to
    , case Map.lookup to (gsBoard gs) of
        Just (Piece s _) -> s /= side; Nothing -> True]

  Bishop ->
    [ChessMove sq to Nothing|to<-concatMap (slideTargets (gsBoard gs) sq side)[(-1,-1),(-1,1),(1,-1),(1,1)]]
  Rook ->
    [ChessMove sq to Nothing|to<-concatMap (slideTargets (gsBoard gs) sq side)[(-1,0),(1,0),(0,-1),(0,1)]]
  Queen ->
    [ChessMove sq to Nothing|to<-concatMap (slideTargets (gsBoard gs) sq side)
      [(-1,-1),(-1,1),(1,-1),(1,1),(-1,0),(1,0),(0,-1),(0,1)]]

  King ->
    let board = gsBoard gs
        normal = [ChessMove sq to Nothing
                 |a<-[-1..1],b<-[-1..1],(a,b)/=(0,0)
                 ,let to=(f+a,r+b),inBounds to
                 ,case Map.lookup to board of
                    Just (Piece s _)->s/=side; Nothing->True]
        castles = castleMoves gs sq (Piece side King)
    in normal ++ castles

castleMoves :: GameState -> Square -> Piece -> [ChessMove]
castleMoves gs (f,r) (Piece side King)
  | isAttackedBy (gsBoard gs) (opponent side) (f,r) = []
  | otherwise =
      let board = gsBoard gs
          cr    = gsCastle gs
          opp   = opponent side
          kside = case side of
            White | crWK cr, f==4, r==0
                  , Map.notMember (5,0) board, Map.notMember (6,0) board
                  , not (isAttackedBy board opp (5,0))
                  , not (isAttackedBy board opp (6,0))
                  -> [ChessMove (4,0) (6,0) Nothing]
            Black | crBK cr, f==4, r==7
                  , Map.notMember (5,7) board, Map.notMember (6,7) board
                  , not (isAttackedBy board opp (5,7))
                  , not (isAttackedBy board opp (6,7))
                  -> [ChessMove (4,7) (6,7) Nothing]
            _ -> []
          qside = case side of
            White | crWQ cr, f==4, r==0
                  , Map.notMember (1,0) board, Map.notMember (2,0) board
                  , Map.notMember (3,0) board
                  , not (isAttackedBy board opp (2,0))
                  , not (isAttackedBy board opp (3,0))
                  -> [ChessMove (4,0) (2,0) Nothing]
            Black | crBQ cr, f==4, r==7
                  , Map.notMember (1,7) board, Map.notMember (2,7) board
                  , Map.notMember (3,7) board
                  , not (isAttackedBy board opp (2,7))
                  , not (isAttackedBy board opp (3,7))
                  -> [ChessMove (4,7) (2,7) Nothing]
            _ -> []
      in kside ++ qside
castleMoves _ _ _ = []

legalMoves :: GameState -> [ChessMove]
legalMoves gs = concatMap getMoves (Map.toList (gsBoard gs))
  where
    turn = gsTurn gs
    getMoves (sq, piece)
      | pieceColor piece /= turn = []
      | otherwise = filter legal (pseudoMoves gs sq piece)
    legal mv = not (isInCheck (applyMove gs mv) turn)

applyMove :: GameState -> ChessMove -> GameState
applyMove gs mv = GameState
  { gsBoard  = newBoard
  , gsTurn   = opponent (gsTurn gs)
  , gsEP     = newEP
  , gsCastle = newCastle
  }
  where
    board = gsBoard gs
    from  = cmFrom mv
    to    = cmTo   mv
    piece = fromMaybe (error "applyMove: no piece") (Map.lookup from board)
    side  = pieceColor piece
    pt    = pieceType  piece
    moved = maybe piece (\p -> Piece side p) (cmPromo mv)
    base  = Map.insert to moved (Map.delete from board)
    -- En passant removal
    base2 = if pt == Pawn && Just to == gsEP gs
              then Map.delete (fst to, snd from) base
              else base
    -- Rook movement during castling
    newBoard
      | pt == King, abs (fst to - fst from) == 2 =
          let (rf, rt) = if fst to > fst from
                         then ((7, snd from), (5, snd from))
                         else ((0, snd from), (3, snd from))
              rook = fromMaybe (error "castle: no rook") (Map.lookup rf base2)
          in Map.insert rt rook (Map.delete rf base2)
      | otherwise = base2
    newEP
      | pt == Pawn, abs (snd to - snd from) == 2 =
          Just (fst from, (snd from + snd to) `div` 2)
      | otherwise = Nothing
    cr = gsCastle gs
    newCastle = CastleRights
      { crWK = crWK cr && from/=(4,0) && from/=(7,0) && to/=(7,0)
      , crWQ = crWQ cr && from/=(4,0) && from/=(0,0) && to/=(0,0)
      , crBK = crBK cr && from/=(4,7) && from/=(7,7) && to/=(7,7)
      , crBQ = crBQ cr && from/=(4,7) && from/=(0,7) && to/=(0,7)
      }

detectStatus :: GameState -> GameStatus
detectStatus gs =
  let side  = gsTurn gs
      moves = legalMoves gs
      check = isInCheck gs side
  in if null moves
       then if check then Checkmate (opponent side) else Stalemate
       else if check then InCheck
       else Playing

-----------------------------------------------------------------------------
-- AI  (alpha-beta minimax, depth 3)
-----------------------------------------------------------------------------

pieceValue :: PieceType -> Int
pieceValue Pawn   = 100
pieceValue Knight = 320
pieceValue Bishop = 330
pieceValue Rook   = 500
pieceValue Queen  = 900
pieceValue King   = 20000

-- Piece-square tables (rank 0 = white back rank for White; mirrored for Black)
pstTable :: PieceType -> [[Int]]
pstTable Pawn =
  [ [ 0,  0,  0,  0,  0,  0,  0,  0]
  , [50, 50, 50, 50, 50, 50, 50, 50]
  , [10, 10, 20, 30, 30, 20, 10, 10]
  , [ 5,  5, 10, 25, 25, 10,  5,  5]
  , [ 0,  0,  0, 20, 20,  0,  0,  0]
  , [ 5, -5,-10,  0,  0,-10, -5,  5]
  , [ 5, 10, 10,-20,-20, 10, 10,  5]
  , [ 0,  0,  0,  0,  0,  0,  0,  0] ]
pstTable Knight =
  [ [-50,-40,-30,-30,-30,-30,-40,-50]
  , [-40,-20,  0,  0,  0,  0,-20,-40]
  , [-30,  0, 10, 15, 15, 10,  0,-30]
  , [-30,  5, 15, 20, 20, 15,  5,-30]
  , [-30,  0, 15, 20, 20, 15,  0,-30]
  , [-30,  5, 10, 15, 15, 10,  5,-30]
  , [-40,-20,  0,  5,  5,  0,-20,-40]
  , [-50,-40,-30,-30,-30,-30,-40,-50] ]
pstTable Bishop =
  [ [-20,-10,-10,-10,-10,-10,-10,-20]
  , [-10,  0,  0,  0,  0,  0,  0,-10]
  , [-10,  0,  5, 10, 10,  5,  0,-10]
  , [-10,  5,  5, 10, 10,  5,  5,-10]
  , [-10,  0, 10, 10, 10, 10,  0,-10]
  , [-10, 10, 10, 10, 10, 10, 10,-10]
  , [-10,  5,  0,  0,  0,  0,  5,-10]
  , [-20,-10,-10,-10,-10,-10,-10,-20] ]
pstTable Rook =
  [ [  0,  0,  0,  0,  0,  0,  0,  0]
  , [  5, 10, 10, 10, 10, 10, 10,  5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [ -5,  0,  0,  0,  0,  0,  0, -5]
  , [  0,  0,  0,  5,  5,  0,  0,  0] ]
pstTable Queen =
  [ [-20,-10,-10,-5,-5,-10,-10,-20]
  , [-10,  0,  0, 0, 0,  0,  0,-10]
  , [-10,  0,  5, 5, 5,  5,  0,-10]
  , [ -5,  0,  5, 5, 5,  5,  0, -5]
  , [  0,  0,  5, 5, 5,  5,  0, -5]
  , [-10,  5,  5, 5, 5,  5,  0,-10]
  , [-10,  0,  5, 0, 0,  0,  0,-10]
  , [-20,-10,-10,-5,-5,-10,-10,-20] ]
pstTable King =
  [ [ 20, 30, 10,  0,  0, 10, 30, 20]
  , [ 20, 20,  0,  0,  0,  0, 20, 20]
  , [-10,-20,-20,-20,-20,-20,-20,-10]
  , [-20,-30,-30,-40,-40,-30,-30,-20]
  , [-30,-40,-40,-50,-50,-40,-40,-30]
  , [-30,-40,-40,-50,-50,-40,-40,-30]
  , [-30,-40,-40,-50,-50,-40,-40,-30]
  , [-30,-40,-40,-50,-50,-40,-40,-30] ]

pstScore :: PieceType -> Square -> Side -> Int
pstScore pt (f,r) side =
  let rank = if side == White then r else 7 - r
      tbl  = pstTable pt
  in (tbl !! rank) !! f

evalBoard :: Board -> Int
evalBoard board = sum
  [ sign * (pieceValue (pieceType p) + pstScore (pieceType p) sq (pieceColor p))
  | (sq,p) <- Map.toList board
  , let sign = if pieceColor p == White then 1 else -1
  ]

bigNum :: Int
bigNum = 10000000

orderMoves :: Board -> [ChessMove] -> [ChessMove]
orderMoves board = sortBy (comparing (Down . captScore))
  where
    captScore mv = case Map.lookup (cmTo mv) board of
      Just p  -> pieceValue (pieceType p)
      Nothing -> 0

alphaBeta :: GameState -> Int -> Int -> Int -> Bool -> Int
alphaBeta gs depth alpha beta isMax
  | depth == 0 = evalBoard (gsBoard gs)
  | otherwise =
      let moves = legalMoves gs
      in if null moves
           then if isInCheck gs (gsTurn gs)
                  then if isMax then -(bigNum + depth) else (bigNum + depth)
                  else 0
           else
             let ordered = orderMoves (gsBoard gs) moves
             in if isMax then maximize ordered alpha beta
                         else minimize ordered alpha beta
  where
    maximize []     a _ = a
    maximize (m:ms) a b =
      let v  = alphaBeta (applyMove gs m) (depth-1) a b False
          a' = max a v
      in if a' >= b then a' else maximize ms a' b
    minimize []     _ b = b
    minimize (m:ms) a b =
      let v  = alphaBeta (applyMove gs m) (depth-1) a b True
          b' = min b v
      in if b' <= a then b' else minimize ms a b'

aiDepth :: Int
aiDepth = 3

bestBlackMove :: GameState -> Maybe ChessMove
bestBlackMove gs
  | null moves = Nothing
  | otherwise  =
      let scored = map (\m -> (alphaBeta (applyMove gs m) (aiDepth-1) (-bigNum) bigNum True, m))
                       (orderMoves (gsBoard gs) moves)
      in Just . snd . minimumBy (comparing fst) $ scored
  where moves = legalMoves gs

-----------------------------------------------------------------------------
-- UPDATE
-----------------------------------------------------------------------------

updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  NewGame -> modify (const initModel)

  FullScreen -> io_ [js| if (document.documentElement.requestFullscreen) { document.documentElement.requestFullscreen() } else { if (document.documentElement.webkitRequestFullscreen) document.documentElement.webkitRequestFullscreen() }|]

  NoOp -> pure ()

  ComputerMove -> do
    m <- get
    case mStatus m of
      s | s `elem` [Playing, InCheck], gsTurn (mGameState m) == Black ->
        case bestBlackMove (mGameState m) of
          Nothing -> modify $ \m' ->
            m' { mStatus = detectStatus (mGameState m'), mThinking = False }
          Just mv  -> modify $ doComputerMove mv
      _ -> modify $ \m' -> m' { mThinking = False }

  ClickSquare sq -> do
    m <- get
    case mStatus m of
      s | s `elem` [Playing, InCheck] , gsTurn (mGameState m) == White ->
        let gs = mGameState m
        in case mSelected m of
          Nothing ->
            case Map.lookup sq (gsBoard gs) of
              Just piece | pieceColor piece == White ->
                let dests = map cmTo
                          . filter (\mv -> cmFrom mv == sq)
                          $ legalMoves gs
                in modify $ \m' -> m' { mSelected = Just sq, mLegalDests = dests }
              _ -> pure ()
          Just sel ->
            if sq `elem` mLegalDests m
            then do
              let mv  = findMove sel sq (legalMoves gs)
                  gs' = applyMove gs mv
                  st  = detectStatus gs'
                  capt = case Map.lookup sq (gsBoard gs) of
                           Just (Piece _ pt) -> pt : mBlackCapt m
                           Nothing -> mBlackCapt m
                  epCapt = case Map.lookup (fst sq, snd sel) (gsBoard gs) of
                             Just (Piece _ pt)
                               | pieceType (fromMaybe (Piece White Pawn) (Map.lookup sel (gsBoard gs))) == Pawn
                               , Just sq == gsEP gs
                               -> pt : mBlackCapt m
                             _ -> capt
              modify $ \m' -> m'
                { mGameState  = gs'
                , mSelected   = Nothing
                , mLegalDests = []
                , mLastMove   = Just (sel, sq)
                , mStatus     = st
                , mBlackCapt  = epCapt
                , mThinking   = st == Playing || st == InCheck
                }
              m2 <- get
              if mThinking m2
                then withSink $ \sink -> threadDelay 350000 >> sink ComputerMove
                else pure ()
            else
              case Map.lookup sq (gsBoard gs) of
                Just piece | pieceColor piece == White ->
                  let dests = map cmTo
                            . filter (\mv -> cmFrom mv == sq)
                            $ legalMoves gs
                  in modify $ \m' -> m' { mSelected = Just sq, mLegalDests = dests }
                _ -> modify $ \m' -> m' { mSelected = Nothing, mLegalDests = [] }
      _ -> pure ()

findMove :: Square -> Square -> [ChessMove] -> ChessMove
findMove from to mvs =
  case filter (\mv -> cmFrom mv == from && cmTo mv == to) mvs of
    (m:_) -> m
    []    -> ChessMove from to Nothing  -- fallback

doComputerMove :: ChessMove -> Model -> Model
doComputerMove mv m =
  let gs  = mGameState m
      gs' = applyMove gs mv
      st  = detectStatus gs'
      capt = case Map.lookup (cmTo mv) (gsBoard gs) of
               Just (Piece _ pt) -> pt : mWhiteCapt m
               Nothing -> mWhiteCapt m
  in m { mGameState  = gs'
       , mSelected   = Nothing
       , mLegalDests = []
       , mLastMove   = Just (cmFrom mv, cmTo mv)
       , mStatus     = st
       , mWhiteCapt  = capt
       , mThinking   = False
       }

-----------------------------------------------------------------------------
-- MAIN
-----------------------------------------------------------------------------

main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else
main = startApp defaultEvents app
#endif

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

app :: App Model Action
app = component initModel updateModel viewGame

-----------------------------------------------------------------------------
-- VIEW
-----------------------------------------------------------------------------

sqSize :: Int
sqSize = 70

boardPx :: Int
boardPx = sqSize * 8  -- 560

-- SVG x/y for a given (file, rank) -- white at bottom
sqX :: Int -> Int
sqX f = f * sqSize

sqY :: Int -> Int
sqY r = (7 - r) * sqSize

-- Center of square
sqCX, sqCY :: Int -> Int
sqCX f = sqX f + sqSize `div` 2
sqCY r = sqY r + sqSize `div` 2

lightSq, darkSq :: MisoString
lightSq = "#F0D9B5"
darkSq  = "#B58863"

squareColor :: Int -> Int -> MisoString
squareColor f r = if even (f + r) then darkSq else lightSq

pieceChar :: Piece -> MisoString
pieceChar (Piece _ King  ) = "♔"
pieceChar (Piece _ Queen ) = "♕"
pieceChar (Piece _ Rook  ) = "♖"
pieceChar (Piece _ Bishop) = "♗"
pieceChar (Piece _ Knight) = "♘"
pieceChar (Piece _ Pawn  ) = "♙"

pieceCharSmall :: PieceType -> MisoString
pieceCharSmall Pawn   = "♟"
pieceCharSmall Knight = "♞"
pieceCharSmall Bishop = "♝"
pieceCharSmall Rook   = "♜"
pieceCharSmall Queen  = "♛"
pieceCharSmall King   = "♚"

fileLabel :: Int -> MisoString
fileLabel 0 = "a"; fileLabel 1 = "b"; fileLabel 2 = "c"; fileLabel 3 = "d"
fileLabel 4 = "e"; fileLabel 5 = "f"; fileLabel 6 = "g"; fileLabel _ = "h"

-----------------------------------------------------------------------------
-- MAIN VIEW
-----------------------------------------------------------------------------

viewGame :: props -> Model -> View Model Action
viewGame _ m =
  H.div_
    [ CSS.style_
      [ CSS.margin "0"
      , CSS.padding "0"
      , CSS.position "fixed"
      , "top" =: "0"
      , "left" =: "0"
      , CSS.width "100%"
      , "height" =: "100%"
      , "overflow-y" =: "auto"
      , "overscroll-behavior" =: "none"
      , "-webkit-overflow-scrolling" =: "touch"
      , CSS.backgroundColor (RGB 49 46 43)
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.alignItems "center"
      , CSS.justifyContent "flex-start"
      , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
      , CSS.boxSizing "border-box"
      , CSS.paddingTop "16px"
      , CSS.paddingBottom "16px"
      , CSS.paddingLeft "12px"
      , CSS.paddingRight "12px"
      ]
    ]
    [ -- Title
      H.div_
        [ CSS.style_
          [ CSS.color (RGB 240 217 181)
          , CSS.fontSize "clamp(18px, 5vw, 28px)"
          , CSS.fontWeight "bold"
          , CSS.letterSpacing "4px"
          , CSS.marginBottom "16px"
          , CSS.textAlign "center"
          , CSS.textShadow "0 2px 8px rgba(0,0,0,0.6)"
          ]
        ]
        [ text "♟ MISO CHESS" ]
    , -- Main content row
      H.div_
        [ CSS.style_
          [ CSS.display "flex"
          , CSS.gap "16px"
          , CSS.alignItems "flex-start"
          , CSS.flexWrap "wrap"
          , CSS.justifyContent "center"
          , CSS.width "100%"
          , CSS.maxWidth (ms (boardPx + 28 + 16 + 280) <> "px")
          ]
        ]
        [ viewBoardPanel m
        , viewSidebar m
        ]
    ]

-----------------------------------------------------------------------------
-- BOARD PANEL
-----------------------------------------------------------------------------

viewBoardPanel :: Model -> View Model Action
viewBoardPanel m =
  H.div_
    [ CSS.style_
      [ CSS.position "relative"
      , CSS.boxShadow "0 8px 32px rgba(0,0,0,0.7)"
      , CSS.borderRadius "4px"
      , CSS.overflow "hidden"
      , CSS.border "3px solid"
      , CSS.borderColor (RGB 30 25 20)
      , CSS.width "100%"
      , CSS.maxWidth (ms (boardPx + 28) <> "px")
      , CSS.flex "0 1 auto"
      ]
    ]
    [ viewSVGBoard m ]

viewSVGBoard :: Model -> View Model Action
viewSVGBoard m =
  SVG.svg_
    [ SP.viewBox_ ("0 0 " <> ms (boardPx + 28) <> " " <> ms (boardPx + 28))
    , HP.width_  "100%"
    , CSS.style_ [CSS.display "block", "aspect-ratio" =: "1"]
    ]
    ( [ -- Dark background
        SVG.rect_
          [ SP.x_ "0", SP.y_ "0"
          , HP.width_  (ms (boardPx + 28))
          , HP.height_ (ms (boardPx + 28))
          , SP.fill_ "#212121"
          ]
      ] ++
      -- Board positioned at (28, 0): rank/file labels on left + bottom
      map (renderSquareBg m) allSquares ++
      renderHighlights m ++
      renderLegalDots m ++
      map (renderPiece m) (Map.toList (gsBoard (mGameState m))) ++
      renderClickTargets m ++
      renderLabels
    )

allSquares :: [Square]
allSquares = [(f,r) | r <- [0..7], f <- [0..7]]

-- Translate to board offset (28px on left, 0 on top)
boardX :: Int -> Int
boardX f = 28 + sqX f

boardY :: Int -> Int
boardY r = sqY r

renderSquareBg :: Model -> Square -> View Model Action
renderSquareBg _ (f,r) =
  SVG.rect_
    [ SP.x_      (ms (boardX f))
    , SP.y_      (ms (boardY r))
    , HP.width_  (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_   (squareColor f r)
    ]

renderHighlights :: Model -> [View Model Action]
renderHighlights m =
  let gs = mGameState m

      -- Last move highlight
      lastMoveRects = case mLastMove m of
        Nothing -> []
        Just (from, to) ->
          [ overlayRect f r "rgba(205,210,106,0.55)"
          | (f,r) <- [from, to]
          ]

      -- Selected square (orange when pinned/no moves, green otherwise)
      selRect = case mSelected m of
        Nothing -> []
        Just (f,r) ->
          let color = if null (mLegalDests m)
                        then "rgba(220,120,20,0.6)"
                        else "rgba(20,200,50,0.5)"
          in [overlayRect f r color]

      -- Check highlight
      checkRect = case mStatus m of
        InCheck ->
          case findKing (gsBoard gs) (gsTurn gs) of
            Just (f,r) -> [overlayRect f r "rgba(232,60,50,0.6)"]
            Nothing    -> []
        Checkmate loser ->
          case findKing (gsBoard gs) loser of
            Just (f,r) -> [overlayRect f r "rgba(232,60,50,0.7)"]
            Nothing    -> []
        _ -> []

  in lastMoveRects ++ selRect ++ checkRect

overlayRect :: Int -> Int -> MisoString -> View Model Action
overlayRect f r fill =
  SVG.rect_
    [ SP.x_      (ms (boardX f))
    , SP.y_      (ms (boardY r))
    , HP.width_  (ms sqSize)
    , HP.height_ (ms sqSize)
    , SP.fill_   fill
    ]

renderLegalDots :: Model -> [View Model Action]
renderLegalDots m =
  [ renderDot f r (Map.member (f,r) (gsBoard (mGameState m)))
  | (f,r) <- mLegalDests m
  ]

renderDot :: Int -> Int -> Bool -> View Model Action
renderDot f r isCapture
  | isCapture =
      -- Ring indicator for captures
      SVG.circle_
        [ SP.cx_           (ms (boardX f + sqSize `div` 2))
        , SP.cy_           (ms (boardY r + sqSize `div` 2))
        , SP.r_            (ms (sqSize `div` 2 - 4))
        , SP.fill_         "none"
        , SP.stroke_       "rgba(20,190,40,0.65)"
        , SP.strokeWidth_  "5"
        ]
  | otherwise =
      SVG.circle_
        [ SP.cx_   (ms (boardX f + sqSize `div` 2))
        , SP.cy_   (ms (boardY r + sqSize `div` 2))
        , SP.r_    (ms (sqSize `div` 4))
        , SP.fill_ "rgba(20,190,40,0.55)"
        ]

renderPiece :: Model -> (Square, Piece) -> View Model Action
renderPiece m ((f,r), piece) =
  let cx  = boardX f + sqSize `div` 2
      cy  = boardY r + sqSize `div` 2
      isSel = mSelected m == Just (f,r)
      isDark = pieceColor piece == Black
  in SVG.g_
       [ CSS.style_
         [ CSS.transition "opacity 0.15s ease"
         , CSS.cursor "pointer"
         ]
       ]
       [ -- Shadow text (offset)
         SVG.text_
           [ SP.x_              (ms (cx + 2))
           , SP.y_              (ms (cy + 2))
           , SP.textAnchor_     "middle"
           , SP.dominantBaseline_ "central"
           , SP.fontSize_       (ms (sqSize - 10))
           , SP.fill_           "rgba(0,0,0,0.35)"
           , SP.fontFamily_     "Arial Unicode MS, Segoe UI Symbol, serif"
           ]
           [ text (pieceChar piece) ]
       , -- Main piece text
         SVG.text_
           [ SP.x_              (ms cx)
           , SP.y_              (ms cy)
           , SP.textAnchor_     "middle"
           , SP.dominantBaseline_ "central"
           , SP.fontSize_       (ms (sqSize - 10))
           , SP.fill_           (if isDark then "#1a1a1a" else "#ffffff")
           , SP.stroke_         (if isDark then "#e8d5b0" else "#2a2a2a")
           , SP.strokeWidth_    "0.8"
           , SP.fontFamily_     "Arial Unicode MS, Segoe UI Symbol, serif"
           , CSS.style_
             [ CSS.transition "transform 0.12s ease"
             , "transform-origin" =: (ms cx <> "px " <> ms cy <> "px")
             , CSS.transform (if isSel then "scale(1.15)" else "scale(1.0)")
             ]
           ]
           [ text (pieceChar piece) ]
       ]

-- Transparent click-target rects on top of everything
renderClickTargets :: Model -> [View Model Action]
renderClickTargets m =
  [ SVG.rect_
      [ SP.x_      (ms (boardX f))
      , SP.y_      (ms (boardY r))
      , HP.width_  (ms sqSize)
      , HP.height_ (ms sqSize)
      , SP.fill_   "transparent"
      , CSS.style_ [CSS.cursor "pointer", "touch-action" =: "manipulation"]
      , SVG.onClick (ClickSquare (f,r))
      ]
  | (f,r) <- allSquares
  ]

renderLabels :: [View Model Action]
renderLabels =
  -- Rank labels (left side, 1-8 from bottom)
  [ SVG.text_
      [ SP.x_              "14"
      , SP.y_              (ms (boardY r + sqSize `div` 2))
      , SP.textAnchor_     "middle"
      , SP.dominantBaseline_ "central"
      , SP.fontSize_       "13"
      , SP.fill_           "#b0a090"
      , SP.fontFamily_     "Arial, sans-serif"
      , SP.fontWeight_     "bold"
      ]
      [ text (ms (r + 1)) ]
  | r <- [0..7]
  ] ++
  -- File labels (bottom)
  [ SVG.text_
      [ SP.x_              (ms (boardX f + sqSize `div` 2))
      , SP.y_              (ms (boardPx + 20))
      , SP.textAnchor_     "middle"
      , SP.dominantBaseline_ "central"
      , SP.fontSize_       "13"
      , SP.fill_           "#b0a090"
      , SP.fontFamily_     "Arial, sans-serif"
      , SP.fontWeight_     "bold"
      ]
      [ text (fileLabel f) ]
  | f <- [0..7]
  ]

-----------------------------------------------------------------------------
-- SIDEBAR
-----------------------------------------------------------------------------

viewSidebar :: Model -> View Model Action
viewSidebar m =
  H.div_
    [ CSS.style_
      [ CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.gap "14px"
      , CSS.flex "1 1 200px"
      , CSS.minWidth "200px"
      , CSS.maxWidth "280px"
      ]
    ]
    [ viewStatus m
    , viewPlayers m
    , viewCaptured m
    , viewControls m
    ]

-- Status box
viewStatus :: Model -> View Model Action
viewStatus m =
  let (bgColor, fgColor, msg) = case mStatus m of
        Playing    ->
          if gsTurn (mGameState m) == White
            then (RGB 40 90 60, RGB 180 255 180, "Your turn ♙")
            else (RGB 40 50 90, RGB 160 200 255, "Computer thinking…")
        InCheck    ->
          if gsTurn (mGameState m) == White
            then (RGB 160 40 40, RGB 255 200 200, "⚠ You are in check!")
            else (RGB 160 40 40, RGB 255 200 200, "⚠ Computer in check!")
        Checkmate w ->
          if w == White
            then (RGB 50 130 50,  RGB 220 255 220, "🏆 You win! Checkmate!")
            else (RGB 120 30 30,  RGB 255 220 220, "☠ Checkmate! Computer wins.")
        Stalemate  ->
          (RGB 80 70 40, RGB 240 230 180, "½ Stalemate — Draw!")
      pinnedHint = case (mSelected m, mLegalDests m, mStatus m) of
        (Just _, [], Playing) -> Just "Piece is pinned — cannot move"
        (Just _, [], InCheck) -> Just "Cannot resolve check with this piece"
        _                     -> Nothing
  in H.div_
       [ CSS.style_
         [ CSS.backgroundColor bgColor
         , CSS.color fgColor
         , CSS.borderRadius "8px"
         , CSS.padding "14px 16px"
         , CSS.fontSize "14px"
         , CSS.fontWeight "bold"
         , CSS.textAlign "center"
         , CSS.boxShadow "0 2px 8px rgba(0,0,0,0.4)"
         , CSS.transition "background-color 0.4s ease"
         ]
       ]
       [ text msg
       , case pinnedHint of
           Just hint ->
             H.div_
               [ CSS.style_
                 [ CSS.marginTop "8px"
                 , CSS.fontSize "11px"
                 , CSS.opacity "0.85"
                 , CSS.fontWeight "normal"
                 ]
               ]
               [ text hint ]
           Nothing -> H.div_ [] []
       , if mThinking m
           then H.div_
                  [ CSS.style_
                    [ CSS.marginTop "8px"
                    , CSS.fontSize "11px"
                    , CSS.opacity "0.8"
                    , CSS.fontWeight "normal"
                    ]
                  ]
                  [ text "⟳ Analysing position…" ]
           else H.div_ [] []
       ]

-- Player info
viewPlayers :: Model -> View Model Action
viewPlayers m =
  H.div_
    [ CSS.style_
      [ CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.gap "6px"
      ]
    ]
    [ viewPlayerRow m Black  "Computer" "♚"
    , viewPlayerRow m White  "You" "♔"
    ]

viewPlayerRow :: Model -> Side -> MisoString -> MisoString -> View Model Action
viewPlayerRow m side label icon =
  let isActive = gsTurn (mGameState m) == side
      bg = if isActive then RGB 80 70 55 else RGB 45 40 35
      border = if isActive then RGB 200 170 100 else RGB 60 55 50
  in H.div_
       [ CSS.style_
         [ CSS.backgroundColor bg
         , CSS.border "2px solid"
         , CSS.borderColor border
         , CSS.borderRadius "6px"
         , CSS.padding "8px 12px"
         , CSS.display "flex"
         , CSS.alignItems "center"
         , CSS.gap "8px"
         , CSS.transition "background-color 0.3s ease, border-color 0.3s ease"
         ]
       ]
       [ H.div_
           [ CSS.style_
             [ CSS.fontSize "24px"
             , CSS.lineHeight "1"
             ]
           ]
           [ text icon ]
       , H.div_
           [ CSS.style_
             [ CSS.color (RGB 230 215 190)
             , CSS.fontSize "14px"
             , CSS.fontWeight (if isActive then "bold" else "normal")
             ]
           ]
           [ text (label <> if isActive then " ◀" else "") ]
       ]

-- Captured pieces
viewCaptured :: Model -> View Model Action
viewCaptured m =
  H.div_
    [ CSS.style_
      [ CSS.backgroundColor (RGB 38 34 30)
      , CSS.border "1px solid"
      , CSS.borderColor (RGB 65 58 50)
      , CSS.borderRadius "6px"
      , CSS.padding "10px 12px"
      ]
    ]
    [ captRow "Captured by you:" (mBlackCapt m)
    , captRow "Captured by computer:" (mWhiteCapt m)
    ]

captRow :: MisoString -> [PieceType] -> View Model Action
captRow label pieces =
  H.div_
    [ CSS.style_ [CSS.marginBottom "6px"] ]
    [ H.div_
        [ CSS.style_
          [ CSS.color (RGB 160 140 120)
          , CSS.fontSize "11px"
          , CSS.marginBottom "3px"
          ]
        ]
        [ text label ]
    , H.div_
        [ CSS.style_
          [ CSS.display "flex"
          , CSS.flexWrap "wrap"
          , CSS.gap "2px"
          , CSS.minHeight "20px"
          ]
        ]
        [ H.span_
            [ CSS.style_
              [ CSS.fontSize "16px"
              , CSS.lineHeight "1"
              ]
            ]
            [ text (pieceCharSmall pt) ]
        | pt <- pieces
        ]
    ]

-- Control buttons
viewControls :: Model -> View Model Action
viewControls m =
  H.div_
    [ CSS.style_
      [ CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.gap "8px"
      ]
    ]
    [ chessBtn (RGB 60 100 60) (RGB 200 240 200) "↺  New Game" NewGame
    , chessBtn (RGB 30 50 80) (RGB 180 210 255) "⛶  Full Screen" FullScreen
    , case mStatus m of
        Checkmate _ -> gameOverBadge "Game over"
        Stalemate   -> gameOverBadge "Draw"
        _           -> H.div_
                         [ CSS.style_
                           [ CSS.color (RGB 140 120 100)
                           , CSS.fontSize "11px"
                           , CSS.textAlign "center"
                           , CSS.padding "4px"
                           ]
                         ]
                         [ text "You play as White" ]
    ]

chessBtn :: Color -> Color -> MisoString -> Action -> View Model Action
chessBtn bg fg lbl act =
  H.button_
    [ CSS.style_
      [ CSS.backgroundColor bg
      , CSS.color fg
      , CSS.border "none"
      , CSS.borderRadius "6px"
      , CSS.padding "12px"
      , CSS.cursor "pointer"
      , CSS.fontSize "15px"
      , CSS.fontWeight "bold"
      , CSS.width "100%"
      , CSS.fontFamily "'Segoe UI', Arial, sans-serif"
      , CSS.boxShadow "0 2px 6px rgba(0,0,0,0.3)"
      , CSS.transition "background-color 0.2s ease, transform 0.1s ease"
      , "touch-action" =: "manipulation"
      , "-webkit-tap-highlight-color" =: "transparent"
      ]
    , SVG.onClick act
    ]
    [ text lbl ]

gameOverBadge :: MisoString -> View Model Action
gameOverBadge msg =
  H.div_
    [ CSS.style_
      [ CSS.backgroundColor (RGB 80 60 30)
      , CSS.color (RGB 255 220 100)
      , CSS.borderRadius "6px"
      , CSS.padding "8px"
      , CSS.textAlign "center"
      , CSS.fontSize "13px"
      , CSS.fontWeight "bold"
      ]
    ]
    [ text ("🏁 " <> msg) ]
