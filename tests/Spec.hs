-----------------------------------------------------------------------------
-- | Native correctness tests for the rules and the engine: perft node
-- counts on the standard positions (which exercise castling, en passant,
-- promotion and pins), FEN round trips, algebraic notation, every draw
-- rule, and a few engine sanity checks (mate in one, free material).
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad (unless)
import           Data.IORef
import           Data.List (sort)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, isJust)
import           System.CPUTime (getCPUTime)
import           System.Exit (exitFailure)
-----------------------------------------------------------------------------
import           Chess
import           Engine
-----------------------------------------------------------------------------
pos :: String -> Position
pos fen = case fromFEN fen of
  Just p -> p
  Nothing -> error ("bad FEN in test: " ++ fen)
-----------------------------------------------------------------------------
mv :: String -> String -> Move
mv a b = Move (fromJust (parseSquare a)) (fromJust (parseSquare b)) Nothing
-----------------------------------------------------------------------------
-- | Play a line of moves given as from-to square pairs.
play :: Position -> [(String, String)] -> Position
play = foldl (\p (a, b) -> applyMove p (mv a b))
-----------------------------------------------------------------------------
-- | Positions visited while playing a line (for repetition tests).
trail :: Position -> [(String, String)] -> ([Position], Position)
trail p0 = foldl step ([], p0)
  where
    step (hist, p) (a, b) = (p : hist, applyMove p (mv a b))
-----------------------------------------------------------------------------
lcg :: Int -> [Double]
lcg s0 = go (abs (s0 * 2654435761 + 1) `mod` m0)
  where
    m0 = 2147483647
    go s = let s' = (s * 48271) `mod` m0
           in fromIntegral s' / fromIntegral m0 : go s'
-----------------------------------------------------------------------------
main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "  ok  " else " FAIL ") <> name)
        unless ok (modifyIORef failures (+ 1))

  putStrLn "-- perft ----------------------------------------------------"
  let start = startPosition
  check "start: 20 moves" (perft 1 start == 20)
  check "start: 400 after two plies" (perft 2 start == 400)
  check "start: 8902 after three plies" (perft 3 start == 8902)
  check "start: 197281 after four plies" (perft 4 start == 197281)
  let kiwi = pos "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
  check "kiwipete: 48 moves (castling both ways, captures, promotions nearby)"
    (perft 1 kiwi == 48)
  check "kiwipete: 2039 after two plies" (perft 2 kiwi == 2039)
  check "kiwipete: 97862 after three plies" (perft 3 kiwi == 97862)
  let p3 = pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
  check "position 3 (pins, en passant): 14 / 191 / 2812"
    (perft 1 p3 == 14 && perft 2 p3 == 191 && perft 3 p3 == 2812)
  let p4 = pos "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
  check "position 4 (promotions, checks): 6 / 264 / 9467"
    (perft 1 p4 == 6 && perft 2 p4 == 264 && perft 3 p4 == 9467)
  let p5 = pos "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
  check "position 5: 44 / 1486 / 62379"
    (perft 1 p5 == 44 && perft 2 p5 == 1486 && perft 3 p5 == 62379)

  putStrLn "-- rules ----------------------------------------------------"
  let ep = play start [("e2","e4"),("a7","a6"),("e4","e5"),("d7","d5")]
  check "double pawn push sets the en passant square"
    (posEP ep == parseSquare "d6")
  check "en passant capture is offered and removes the pawn"
    (let m = Move (fromJust (parseSquare "e5")) (fromJust (parseSquare "d6")) Nothing
         after = applyMove ep m
     in m `elem` legalMoves ep
          && Map.notMember (fromJust (parseSquare "d5")) (posBoard after)
          && Map.member (fromJust (parseSquare "d6")) (posBoard after))
  let castled = applyMove kiwi (mv "e1" "g1")
  check "castling moves the rook too"
    (fmap pieceType (Map.lookup (5, 0) (posBoard castled)) == Just Rook
       && Map.notMember (7, 0) (posBoard castled)
       && not (cWK (posCastling castled)) && not (cWQ (posCastling castled)))
  check "cannot castle through check"
    (let p = pos "r3k2r/8/8/8/8/8/8/R3K1qR w KQkq - 0 1" -- f1 attacked
     in mv "e1" "g1" `notElem` legalMoves p)
  check "cannot castle out of check"
    (let p = pos "4k3/8/8/8/8/8/4r3/R3K2R w KQ - 0 1"
     in mv "e1" "g1" `notElem` legalMoves p && mv "e1" "c1" `notElem` legalMoves p)
  check "a pinned piece cannot leave the line"
    (let p = pos "4k3/8/8/8/8/8/4B3/4K2r w - - 0 1" -- no: put rook on e-file
         q = pos "4k3/4r3/8/8/8/8/4B3/4K3 w - - 0 1"
     in null [ m | m <- legalMoves q, mvFrom m == (4, 1), fst (mvTo m) /= 4 ]
          && isJust (fromFEN (toFEN p)))
  check "promotion offers all four pieces and keeps the piece id"
    (let p = pos "8/P6k/8/8/8/8/8/K7 w - - 0 1"
         ms = [ m | m <- legalMoves p, mvFrom m == (0, 6) ]
         q = applyMove p (Move (0, 6) (0, 7) (Just Queen))
         before = fmap pieceId (Map.lookup (0, 6) (posBoard p))
         after = Map.lookup (0, 7) (posBoard q)
     in sort (map mvPromo ms) == map Just [Knight, Bishop, Rook, Queen]
          && fmap pieceType after == Just Queen && fmap pieceId after == before)
  check "halfmove clock resets on pawn moves and captures"
    (let a = applyMove kiwi (mv "e5" "g6")  -- knight takes pawn
         b = applyMove kiwi (mv "a2" "a3")  -- pawn move
         c = applyMove kiwi (mv "a1" "b1")  -- quiet rook move
     in posHalfmove a == 0 && posHalfmove b == 0 && posHalfmove c == 1)
  check "fullmove counter advances after Black"
    (posFullmove (play start [("e2","e4"),("e7","e5")]) == 2)

  putStrLn "-- status ---------------------------------------------------"
  let fools = play start [("f2","f3"),("e7","e5"),("g2","g4"),("d8","h4")]
  check "fool's mate is checkmate for Black" (status [] fools == Mate Black)
  check "check is reported"
    (status [] (pos "4k3/8/8/8/8/8/4r3/4K3 w - - 0 1") == Check)
  check "stalemate"
    (status [] (pos "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1") == Draw Stalemate)
  check "bare kings are a draw"
    (status [] (pos "4k3/8/8/8/8/8/8/4K3 w - - 0 1") == Draw InsufficientMaterial)
  check "king and knight cannot mate"
    (status [] (pos "4k3/8/8/8/8/8/8/4KN2 w - - 0 1") == Draw InsufficientMaterial)
  check "same-coloured bishops cannot mate"
    (status [] (pos "4k3/8/8/8/8/8/2B5/4KB2 w - - 0 1") == Draw InsufficientMaterial)
  check "opposite-coloured bishops can"
    (status [] (pos "4k3/8/8/8/8/8/3B4/4KB2 w - - 0 1") == Ongoing)
  check "fifty-move rule"
    (status [] (pos "4k3/8/8/8/8/8/8/R3K3 w - - 100 80") == Draw FiftyMoves)
  let (hist, rep) = trail start
        [ ("g1","f3"),("g8","f6"),("f3","g1"),("f6","g8")
        , ("g1","f3"),("g8","f6"),("f3","g1"),("f6","g8") ]
  check "threefold repetition" (status hist rep == Draw Repetition)
  check "twofold is not yet a draw"
    (let (h2, r2) = trail start [ ("g1","f3"),("g8","f6"),("f3","g1"),("f6","g8") ]
     in status h2 r2 == Ongoing)

  putStrLn "-- notation -------------------------------------------------"
  check "pawn push, knight move, capture"
    (san start (mv "e2" "e4") == "e4"
       && san start (mv "g1" "f3") == "Nf3"
       && san ep (mv "e5" "d6") == "exd6")
  check "castling both ways"
    (san kiwi (mv "e1" "g1") == "O-O" && san kiwi (mv "e1" "c1") == "O-O-O")
  check "check and mate suffixes"
    (let before = play start [("f2","f3"),("e7","e5"),("g2","g4")]
     in san before (mv "d8" "h4") == "Qh4#"
          && san (pos "4k3/8/8/8/8/8/8/R3K3 w - - 0 1") (mv "a1" "a8") == "Ra8+")
  check "disambiguation by file, rank, and both"
    (let twoKnights = pos "4k3/8/8/8/8/8/8/N1N1K3 w - - 0 1"
         twoRooks = pos "4k3/8/8/8/R7/8/8/R3K3 w - - 0 1"
         twoQueens = pos "4k3/8/8/8/8/8/8/QQ2K3 w - - 0 1"
         threeQueens = pos "8/7k/8/8/8/Q7/8/Q1Q1K3 w - - 0 1"
     in san twoKnights (mv "a1" "b3") == "Nab3"
          && san twoRooks (mv "a1" "a3") == "R1a3"
          && san twoQueens (mv "a1" "a2") == "Qaa2"
          && san threeQueens (mv "a1" "b2") == "Qa1b2")
  check "promotion with capture"
    (san (pos "1n5k/P7/8/8/8/8/8/K7 w - - 0 1") (Move (0, 6) (1, 7) (Just Queen)) == "axb8=Q+")
  check "FEN round trips"
    (all (\f -> fmap toFEN (fromFEN f) == Just f)
       [ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
       , "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
       , "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
       , "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2" ])
  check "garbage FEN is rejected"
    (all ((== Nothing) . fromFEN) [ "", "8/8/8 w - - 0 1", "9/8/8/8/8/8/8/8 w - - 0 1" ])

  putStrLn "-- engine ---------------------------------------------------"
  let mateIn1 = pos "6k1/5ppp/8/8/8/8/8/R3K3 w - - 0 1"
  check "master finds the back-rank mate"
    (fmap sMove (search Master (lcg 3) mateIn1) == Just (mv "a1" "a8"))
  check "and reports it as mate in one for White"
    (fmap sScore (search Master (lcg 3) mateIn1) == Just (MateIn White 1))
  let hangQ = pos "4k3/8/8/8/8/8/8/4K1Qq w - - 0 1"
  check "club captures a free queen"
    (fmap (mvTo . sMove) (search Club (lcg 5) hangQ) == parseSquare "h1")
  check "casual still finds a queen when it is free"
    (fmap (mvTo . sMove) (search Casual (lcg 7) hangQ) == parseSquare "h1")
  check "casual sees a mate in one"
    (fmap sScore (search Casual (lcg 2) mateIn1) == Just (MateIn White 1))
  check "the engine does not walk into mate when it can avoid it"
    (let p = pos "6k1/5ppp/8/8/8/8/8/1R2K3 b - - 0 1"  -- black must make luft
     in fmap (mvFrom . sMove) (search Club (lcg 4) p) /= parseSquare "g8"
          || fmap (mvTo . sMove) (search Club (lcg 4) p) == parseSquare "f8")
  check "evaluation is symmetric"
    (evaluate (posBoard start) == 0)
  check "a side with an extra rook evaluates ahead"
    (evaluate (posBoard (pos "4k3/8/8/8/8/8/8/R3K3 w - - 0 1")) > 400)
  check "no move when mated"
    (search Master (lcg 1) fools == Nothing)

  -- timings, to keep the WASM build honest (it runs a few times slower)
  let opening = play start [("e2","e4"),("e7","e5"),("g1","f3"),("b8","c6"),("f1","b5"),("a7","a6")]
      timed name lvl p = do
        t0 <- getCPUTime
        case search lvl (lcg 11) p of
          Just s -> do
            let chosen = san p (sMove s)
            t1 <- length chosen `seq` getCPUTime
            let took = (t1 - t0) `div` 1000000000
            putStrLn ("  " ++ levelName lvl ++ " chose " ++ chosen
                      ++ " in the " ++ name ++ " in " ++ show took ++ " ms")
            pure took
          Nothing -> pure 0
  a <- timed "Ruy Lopez" Master opening
  b <- timed "kiwipete middlegame" Master kiwi
  _ <- timed "kiwipete middlegame" Expert kiwi
  check "master moves in under two seconds natively" (a < 2000 && b < 2000)

  n <- readIORef failures
  if n == 0
    then putStrLn "all tests passed"
    else putStrLn (show n ++ " failure(s)") >> exitFailure
