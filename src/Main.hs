-----------------------------------------------------------------------------
-- | miso-chess: a quiet game of chess against a built-in engine, in the
-- miso game family.  The rules are in "Chess", the search in "Engine";
-- this module is the update loop and the view.
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (when, unless)
import qualified Data.IntSet as IS
import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust, isNothing)
-----------------------------------------------------------------------------
import           Miso hiding ((!!), view, status, Draw)
import           Miso.Lens hiding (view)
import           Miso.Random (replicateRM)
import           Miso.FFI.QQ (js)
import qualified Miso.CSS as CSS
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as HE
import qualified Miso.Html.Property as HP
-----------------------------------------------------------------------------
import           Chess
import           Drag
import           Engine
import           Model
import           Pieces
import           Sound
import           Styles (skin)
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component initialModel updateModel viewModel)
  { styles = [ Sheet skin ]
  , subs = [ keyboardSub Keys ]
  }
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
type Fx = Effect () () Model Action
-----------------------------------------------------------------------------
-- * Update
-----------------------------------------------------------------------------
updateModel :: Action -> Fx
updateModel = \case
  NoOp -> pure ()

  ToggleSound -> soundOn %= not

  ShowHelp -> showHelp .= True

  CloseHelp -> showHelp .= False

  FullScreen -> io_ goFullScreen

  ChooseSide c -> do
    sideChoice .= c
    playFx "click"

  ChooseLevel l -> do
    level .= l
    playFx "click"

  StartGame -> do
    io_ soundInit
    io_ dragInit
    io $ do
      supply <- replicateRM 2
      pure (Begin supply)

  Begin supply -> do
    m <- get
    let side = case m ^. sideChoice of
          PlayWhite -> White
          PlayBlack -> Black
          PlayRandom -> case supply of
            (r : _) | r < 0.5 -> Black
            _ -> White
    put initialModel
      { _phase = Playing
      , _human = side
      , _flipped = side == Black
      , _level = m ^. level
      , _sideChoice = m ^. sideChoice
      , _tally = m ^. tally
      , _soundOn = m ^. soundOn
      , _heldKeys = m ^. heldKeys
      }
    playFx "castle"
    engineIfDue

  Tap sq -> do
    m <- get
    when (canAct m) $
      case m ^. selected of
        Just from
          | from == sq -> clearSelection
          | (t : more) <- [ t | t <- m ^. targets, mvTo t == sq ] ->
              if null more
                then humanMove t
                else do
                  promotion .= Just (from, sq)
                  playFx "select"
        _ -> selectSquare sq

  Promote pt -> do
    m <- get
    case m ^. promotion of
      Just (from, to) -> do
        promotion .= Nothing
        humanMove (Move from to (Just pt))
      Nothing -> pure ()

  CancelPromotion -> do
    promotion .= Nothing
    hint .= Nothing
    clearSelection

  EngineReply stamp supply -> do
    m <- get
    when (m ^. pacer == stamp
            && m ^. phase == Playing
            && posTurn (m ^. position) == engineSide m) $
      case search (m ^. level) supply (m ^. position) of
        Nothing -> thinking .= False
        Just s -> do
          evalScore .= sScore s
          thinking .= False
          commitMove (sMove s)

  Undo -> do
    m <- get
    unless (null (m ^. plies) || m ^. phase == Title) $ do
      let (p, rest) = rewind (m ^. human) (m ^. plies)
      position .= p
      plies .= rest
      gameStatus .= status (map plyBefore rest) p
      phase .= Playing
      reviewing .= False
      thinking .= False
      pacer += 1
      promotion .= Nothing
      hint .= Nothing
      clearSelection
      playFx "undo"
      io_ followMoves
      engineIfDue

  AskHint -> do
    m <- get
    when (canAct m) $ do
      hint .= fmap sMove (search Expert (repeat 0.37) (m ^. position))
      clearSelection
      playFx "select"

  Flip -> do
    flipped %= not
    playFx "flip"

  ReviewBoard -> reviewing .= True

  CopyPGN -> do
    m <- get
    io_ (copyText (ms (pgn m)))
    playFx "click"

  BackToTitle -> do
    phase .= Title
    showHelp .= False
    thinking .= False
    promotion .= Nothing
    pacer += 1

  Keys ks -> do
    m <- get
    let fresh = IS.toList (ks IS.\\ (m ^. heldKeys))
    heldKeys .= ks
    mapM_ (issueKey m) fresh
-----------------------------------------------------------------------------
issueKey :: Model -> Int -> Fx
issueKey m k
  | m ^. showHelp = when (k == 27 || k == 72) (issue CloseHelp)
  | Title <- m ^. phase =
      if | k == 13 || k == 32 -> issue StartGame
         | k == 72 -> issue ShowHelp
         | otherwise -> pure ()
  | otherwise =
      if | k == 72 -> issue ShowHelp                       -- H
         | k == 85 -> issue Undo                           -- U
         | k == 70 -> issue Flip                           -- F
         | k == 77 -> issue ToggleSound                    -- M
         | k == 78 -> issue BackToTitle                    -- N
         | k == 27 -> issue CancelPromotion                -- esc
         | k == 13 || k == 32, Over <- m ^. phase, not (m ^. reviewing) -> issue StartGame
         | otherwise -> pure ()
-----------------------------------------------------------------------------
-- | The player may touch the board: their move, nothing pending.
canAct :: Model -> Bool
canAct m =
  humanToMove m && not (m ^. thinking) && isNothing (m ^. promotion) && not (m ^. showHelp)
-----------------------------------------------------------------------------
clearSelection :: Fx
clearSelection = do
  selected .= Nothing
  targets .= []
-----------------------------------------------------------------------------
selectSquare :: Square -> Fx
selectSquare sq = do
  m <- get
  case Map.lookup sq (posBoard (m ^. position)) of
    Just pc | pieceColor pc == m ^. human -> do
      selected .= Just sq
      targets .= movesFrom (m ^. position) sq
      hint .= Nothing
      playFx "select"
    _ -> clearSelection
-----------------------------------------------------------------------------
humanMove :: Move -> Fx
humanMove mv = do
  commitMove mv
  engineIfDue
-----------------------------------------------------------------------------
-- | Play a move onto the record and react to what it did.
commitMove :: Move -> Fx
commitMove mv = do
  m <- get
  let p = m ^. position
      p' = applyMove p mv
      st = status (p : history m) p'
      capture = isCapture p mv
      castle = isCastle p mv
  plies %= (Ply p mv (san p mv) :)
  position .= p'
  gameStatus .= st
  hint .= Nothing
  promotion .= Nothing
  clearSelection
  io_ followMoves
  case st of
    Mate winner -> finish (if winner == m ^. human then "win" else "lose")
    Draw _ -> finish "draw"
    Check -> playFx "check"
    Ongoing -> playFx $ if
      | isJust (mvPromo mv) -> "promote"
      | castle -> "castle"
      | capture -> "capture"
      | otherwise -> "move"
-----------------------------------------------------------------------------
finish :: MisoString -> Fx
finish outcome = do
  phase .= Over
  reviewing .= False
  thinking .= False
  tally %= \(w, d, l) -> case outcome of
    "win" -> (w + 1, d, l)
    "draw" -> (w, d + 1, l)
    _ -> (w, d, l + 1)
  playFx outcome
-----------------------------------------------------------------------------
-- | Hand the position to the engine after a short pause so the player's
-- move is seen landing first.  The stamp lets an undo cancel the reply.
engineIfDue :: Fx
engineIfDue = do
  m <- get
  when (m ^. phase == Playing && posTurn (m ^. position) == engineSide m) $ do
    pacer += 1
    thinking .= True
    stamp <- use pacer
    io $ do
      supply <- replicateRM 48
      threadDelay 450000
      pure (EngineReply stamp supply)
-----------------------------------------------------------------------------
-- | Take back plies until it is the player's move again.
rewind :: Side -> [Ply] -> (Position, [Ply])
rewind side = unwind
  where
    unwind [] = (startPosition, [])
    unwind (p : rest)
      | posTurn (plyBefore p) == side = (plyBefore p, rest)
      | otherwise = unwind rest
-----------------------------------------------------------------------------
playFx :: MisoString -> Fx
playFx name = do
  m <- get
  io_ (playSound (m ^. soundOn) name)
-----------------------------------------------------------------------------
goFullScreen :: IO ()
goFullScreen = [js|
  var d = document.documentElement;
  if (document.fullscreenElement) { document.exitFullscreen(); }
  else if (d.requestFullscreen) { d.requestFullscreen(); }
  else if (d.webkitRequestFullscreen) { d.webkitRequestFullscreen(); }
|]
-----------------------------------------------------------------------------
-- | Keep the newest move in view once the ledger has been redrawn.
followMoves :: IO ()
followMoves = [js|
  setTimeout(function () {
    var el = document.querySelector('.moves');
    if (el) { el.scrollTop = el.scrollHeight; }
  }, 80);
|]
-----------------------------------------------------------------------------
copyText :: MisoString -> IO ()
copyText s = [js|
  if (navigator.clipboard) { navigator.clipboard.writeText(${s}); }
|]
-----------------------------------------------------------------------------
-- | The game record as PGN.
pgn :: Model -> String
pgn m = unlines headers ++ "\n" ++ unwords (numbered 1 sans ++ [result])
  where
    headers =
      [ "[Event \"Miso Chess\"]"
      , "[Site \"https://chess.haskell-miso.org\"]"
      , "[White \"" ++ name White ++ "\"]"
      , "[Black \"" ++ name Black ++ "\"]"
      , "[Result \"" ++ result ++ "\"]"
      ]
    name s
      | s == m ^. human = "You"
      | otherwise = "Miso engine (" ++ levelName (m ^. level) ++ ")"
    result = case m ^. gameStatus of
      Mate White -> "1-0"
      Mate Black -> "0-1"
      Draw _ -> "1/2-1/2"
      _ -> "*"
    sans = reverse (map plySan (m ^. plies))
    numbered :: Int -> [String] -> [String]
    numbered _ [] = []
    numbered n (w : b : rest) = (show n ++ ". " ++ w ++ " " ++ b) : numbered (n + 1) rest
    numbered n [w] = [show n ++ ". " ++ w]
-----------------------------------------------------------------------------
-- * View
-----------------------------------------------------------------------------
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ m = case m ^. phase of
  Title -> H.div_ [] (titleView m : [ helpOverlay | m ^. showHelp ])
  _ -> H.div_ []
    ( [ topbar m, stage m ]
      ++ [ resultOverlay m | m ^. phase == Over, not (m ^. reviewing) ]
      ++ [ helpOverlay | m ^. showHelp ]
    )
-----------------------------------------------------------------------------
titleView :: Model -> View () Model Action
titleView m = H.div_ [ HP.class_ "title" ]
  [ H.div_ [ HP.class_ "heroKnight" ] [ pieceSvg White Knight ]
  , H.h1_ [ HP.class_ "wordmark" ] [ text "Chess" ]
  , H.p_ [ HP.class_ "tagline" ]
      [ text "A quiet game against the engine. Choose your colour and how hard it should think." ]
  , H.div_ [ HP.class_ "choose" ]
      [ H.div_ [ HP.class_ "sides" ]
          [ sideTile PlayWhite "Play white" [ pieceSvg White King ]
          , sideTile PlayRandom "Either" [ pieceSvg White King, pieceSvg Black King ]
          , sideTile PlayBlack "Play black" [ pieceSvg Black King ]
          ]
      , H.div_ [ HP.class_ "levels" ] [ levelTile lv | lv <- [minBound .. maxBound] ]
      , H.button_ [ HP.class_ "playBtn", HE.onClick StartGame ] [ text "Play" ]
      , H.button_ [ HP.class_ "howLink", HE.onClick ShowHelp ] [ text "How to play" ]
      ]
  , H.div_ [ HP.class_ "foot" ] [ text footText ]
  ]
  where
    (w, d, l) = m ^. tally
    footText
      | w + d + l == 0 = "Built with miso 🍜"
      | otherwise = "This session: " <> plural w "win" <> ", " <> plural d "draw"
          <> ", " <> plural l "loss" <> " · built with miso 🍜"
    plural n word = ms n <> " " <> word <> (if n == 1 then "" else if word == "loss" then "es" else "s")
    sideTile c label kings = H.button_
      [ HP.class_ (joinCls [ "sideTile", clsWhen (m ^. sideChoice == c) "on" ])
      , HE.onClick (ChooseSide c)
      ]
      [ H.div_ [ HP.class_ "kings" ] kings, text label ]
    levelTile lv = H.button_
      [ HP.class_ (joinCls [ "lvl", clsWhen (m ^. level == lv) "on" ])
      , HE.onClick (ChooseLevel lv)
      ]
      [ H.b_ [] [ text (ms (levelName lv)) ]
      , H.span_ [] [ text (ms (levelBlurb lv)) ]
      ]
-----------------------------------------------------------------------------
topbar :: Model -> View () Model Action
topbar m = H.div_ [ HP.class_ "top" ]
  [ H.div_ [ HP.class_ "brand" ] [ pieceSvg White Knight, text "Chess" ]
  , H.div_ [ HP.class_ "hud" ]
      [ text (ms (levelName (m ^. level)) <> " engine, move " <> ms (moveNumber m)) ]
  , H.div_ [ HP.class_ "tools" ]
      [ tool ShowHelp "?" "Help"
      , tool ToggleSound (if m ^. soundOn then "🔊" else "🔇") "Sound"
      , tool FullScreen "⛶" "Full screen"
      , tool BackToTitle "↩" "Leave"
      ]
  ]
  where
    tool act icon label = H.button_
      [ HP.class_ "tool", HE.onClick act, HP.title_ label ]
      [ text icon, H.span_ [] [ text label ] ]
-----------------------------------------------------------------------------
stage :: Model -> View () Model Action
stage m = H.div_ [ HP.class_ "stage" ]
  [ plate m (engineSide m)
  , evalBar m
  , boardWrap m
  , plate m (m ^. human)
  , movesView m
  , actionsView m
  ]
-----------------------------------------------------------------------------
plate :: Model -> Side -> View () Model Action
plate m side = H.div_
  [ HP.class_ (joinCls
      [ "plate", if isYou then "you" else "engine"
      , clsWhen active "active"
      , clsWhen (not isYou && m ^. thinking) "thinking"
      ])
  ]
  [ H.div_ [ HP.class_ "turn" ] []
  , H.div_ [ HP.class_ "who" ]
      [ H.b_ [] [ text (if isYou then "You" else "Engine") ]
      , H.span_ [] [ text subline ]
      ]
  , H.div_ [ HP.class_ "tray" ]
      ( [ pieceSvg (opponent side) t | t <- taken ]
        ++ [ H.span_ [ HP.class_ "adv" ] [ text ("+" <> ms lead) ] | lead > 0 ]
      )
  ]
  where
    isYou = side == m ^. human
    board = posBoard (m ^. position)
    active = m ^. phase == Playing && posTurn (m ^. position) == side
    taken = capturedBy board side
    lead = sum (map pointValue taken) - sum (map pointValue (capturedBy board (opponent side)))
    subline
      | m ^. phase == Over = case m ^. gameStatus of
          Mate w | w == side -> "Winner"
                 | otherwise -> "Checkmated"
          _ -> "Drawn"
      | not isYou = if
          | m ^. thinking -> "Thinking…"
          | active -> "To move"
          | otherwise -> ms (levelName (m ^. level)) <> " strength"
      | active = if
          | Just _ <- m ^. selected, null (m ^. targets) -> "That piece can't move"
          | isJust (m ^. promotion) -> "Choose a piece"
          | m ^. gameStatus == Check -> "You're in check"
          | isJust (m ^. hint) -> "The hint is marked"
          | otherwise -> "Your move"
      | otherwise = "Waiting"
-----------------------------------------------------------------------------
-- | Pieces this side has taken: the other side's missing men.
capturedBy :: Board -> Side -> [PieceType]
capturedBy board side =
  concat [ replicate (max 0 (n - Map.findWithDefault 0 t left)) t | (t, n) <- army ]
  where
    left = materialLeft board (opponent side)
    army = [ (Queen, 1), (Rook, 2), (Bishop, 2), (Knight, 2), (Pawn, 8) ]
-----------------------------------------------------------------------------
evalBar :: Model -> View () Model Action
evalBar m = H.div_ [ HP.class_ "evalWrap" ]
  [ H.div_ [ HP.class_ "eval" ]
      [ H.div_ [ HP.class_ "evalFill", CSS.style_ [ "--share" =: (ms share <> "%") ] ] [] ]
  , H.div_ [ HP.class_ "evalText" ] [ text label ]
  ]
  where
    share :: Int
    share = case (m ^. phase, m ^. gameStatus, m ^. evalScore) of
      (Over, Mate White, _) -> 100
      (Over, Mate Black, _) -> 0
      (Over, Draw _, _) -> 50
      (_, _, MateIn White _) -> 97
      (_, _, MateIn Black _) -> 3
      (_, _, Centipawns cp) ->
        round (100 / (1 + exp (negate (fromIntegral cp / 320 :: Double))))
    label = case (m ^. phase, m ^. gameStatus, m ^. evalScore) of
      (Over, Mate White, _) -> "1–0"
      (Over, Mate Black, _) -> "0–1"
      (Over, Draw _, _) -> "½–½"
      (_, _, MateIn White n) -> "M" <> ms n
      (_, _, MateIn Black n) -> "−M" <> ms n
      (_, _, Centipawns cp) ->
        let tenths = abs cp `div` 10
            body = ms (tenths `div` 10) <> "." <> ms (tenths `mod` 10)
        in (if cp < 0 then "−" else "+") <> body
-----------------------------------------------------------------------------
boardWrap :: Model -> View () Model Action
boardWrap m = H.div_
  [ HP.class_ (joinCls [ "boardWrap", clsWhen inCheck "inCheck" ]) ]
  [ boardView m ]
  where
    inCheck = m ^. gameStatus == Check
-----------------------------------------------------------------------------
boardView :: Model -> View () Model Action
boardView m = H.div_
  [ HP.class_ (joinCls [ "board", clsWhen (m ^. flipped) "flipped" ]) ]
  ( [ squareView m (f, r) | r <- [7, 6 .. 0], f <- [0 .. 7] ]
    ++ [ H.div_ [ HP.class_ "grain" ] []
       , H.div_ [ HP.class_ "pieces" ]
           [ pieceView sq p | (sq, p) <- sortOn (pieceId . snd) (Map.toList board) ]
       ]
    ++ promotionTray m
  )
  where
    board = posBoard (m ^. position)
-----------------------------------------------------------------------------
squareView :: Model -> Square -> View () Model Action
squareView m sq@(f, r) = H.div_
  [ HP.class_ (joinCls
      [ "sq", if even (f + r) then "dark" else "light"
      , clsWhen own "own"
      , clsWhen (m ^. selected == Just sq) "sel"
      , clsWhen isLast "last"
      , clsWhen (isTarget && not occupied) "dot"
      , clsWhen (isTarget && occupied) "cap"
      , clsWhen isCheck "check"
      , clsWhen (fmap mvFrom (m ^. hint) == Just sq) "hintFrom"
      , clsWhen (fmap mvTo (m ^. hint) == Just sq) "hintTo"
      ])
  , textProp "data-sq" (ms (squareName sq))
  , HE.onClick (Tap sq)
  ]
  ( [ H.span_ [ HP.class_ "coord rank" ] [ text (ms [rankChar r]) ] | f == edgeFile ]
    ++ [ H.span_ [ HP.class_ "coord file" ] [ text (ms [fileChar f]) ] | r == edgeRank ]
  )
  where
    pos = m ^. position
    board = posBoard pos
    edgeFile = if m ^. flipped then 7 else 0
    edgeRank = if m ^. flipped then 7 else 0
    here = Map.lookup sq board
    occupied = isJust here || (isTarget && Just sq == posEP pos)
    own = canAct m && fmap pieceColor here == Just (m ^. human)
    isTarget = any ((== sq) . mvTo) (m ^. targets)
    isLast = case lastMove m of
      Just mv -> mvFrom mv == sq || mvTo mv == sq
      Nothing -> False
    isCheck = case m ^. gameStatus of
      Check -> findKing board (posTurn pos) == Just sq
      Mate winner -> findKing board (opponent winner) == Just sq
      _ -> False
-----------------------------------------------------------------------------
pieceView :: Square -> Piece -> View () Model Action
pieceView sq@(f, r) p = H.div_
  [ key_ (pieceId p)
  , HP.class_ "pc"
  , textProp "data-sq" (ms (squareName sq))
  , CSS.style_
      [ CSS.transform ("translate(" <> ms (f * 100) <> "%, " <> ms ((7 - r) * 100) <> "%)") ]
  ]
  [ pieceSvg (pieceColor p) (pieceType p) ]
-----------------------------------------------------------------------------
promotionTray :: Model -> [View () Model Action]
promotionTray m = case m ^. promotion of
  Nothing -> []
  Just (_, (tf, tr)) ->
    [ H.div_ [ HP.class_ "promoBackdrop", HE.onClick CancelPromotion ] []
    , H.div_
        [ HP.class_ "promo"
        , CSS.style_
            [ CSS.left (ms (fromIntegral tf * 12.5 :: Double) <> "%")
            , CSS.top (if tr == 7 then "0" else "50%")
            ]
        ]
        [ H.button_ [ HP.class_ "promoBtn", HE.onClick (Promote pt), HP.title_ (ms (show pt)) ]
            [ pieceSvg (m ^. human) pt ]
        | pt <- if tr == 7 then choices else reverse choices
        ]
    ]
  where
    choices = [ Queen, Knight, Rook, Bishop ]
-----------------------------------------------------------------------------
movesView :: Model -> View () Model Action
movesView m = H.div_ [ HP.class_ "moves" ]
  ( if null sans
      then [ H.div_ [ HP.class_ "movesEmpty" ] [ text opening ] ]
      else zipWith row [1 :: Int ..] (pairs sans)
  )
  where
    sans = reverse (map plySan (m ^. plies))
    total = length sans
    opening
      | m ^. human == White = "The first move is yours."
      | otherwise = "The engine opens."
    pairs (a : b : rest) = (a, Just b) : pairs rest
    pairs [a] = [(a, Nothing)]
    pairs [] = []
    row n (w, b) = H.div_ [ HP.class_ "mrow", key_ n ]
      [ H.span_ [ HP.class_ "mno" ] [ text (ms n <> ".") ]
      , cell (2 * n - 1) w
      , maybe (H.span_ [] []) (cell (2 * n)) b
      ]
    cell k s = H.span_
      [ HP.class_ (joinCls [ "mv", clsWhen (k == total) "cur" ]) ]
      [ text (ms s) ]
-----------------------------------------------------------------------------
actionsView :: Model -> View () Model Action
actionsView m = H.div_ [ HP.class_ "actions" ]
  [ act Undo "Undo" (not (null (m ^. plies)))
  , act AskHint "Hint" (canAct m)
  , act Flip "Flip" True
  , if m ^. phase == Over
      then H.button_ [ HP.class_ "act primary", HE.onClick StartGame ] [ text "Play again" ]
      else act BackToTitle "New game" True
  ]
  where
    act a label enabled = H.button_
      [ HP.class_ "act", HE.onClick a, boolProp "disabled" (not enabled) ]
      [ text label ]
-----------------------------------------------------------------------------
resultOverlay :: Model -> View () Model Action
resultOverlay m = H.div_ [ HP.class_ "overlay" ]
  [ H.div_ [ HP.class_ "modal" ]
      [ H.div_ [ HP.class_ "resultKicker" ] [ text kicker ]
      , H.div_ [ HP.class_ (joinCls [ "resultTitle", cls ]) ] [ text title ]
      , H.p_ [ HP.class_ "resultSub" ] [ text sub ]
      , H.div_ [ HP.class_ "stats" ]
          [ stat 0 (ms (length (m ^. plies) `div` 2 + length (m ^. plies) `mod` 2)) "moves"
          , stat 1 (ms (length taken)) (if length taken == 1 then "capture" else "captures")
          , stat 2 (ms w <> "–" <> ms d <> "–" <> ms l) "session"
          ]
      , H.div_ [ HP.class_ "btnRow" ]
          [ H.button_ [ HP.class_ "btn primary", HE.onClick StartGame ] [ text "Play again" ]
          , H.button_ [ HP.class_ "btn", HE.onClick ReviewBoard ] [ text "Look at the board" ]
          , H.button_ [ HP.class_ "btn", HE.onClick CopyPGN ] [ text "Copy PGN" ]
          , H.button_ [ HP.class_ "btn", HE.onClick BackToTitle ] [ text "Title" ]
          ]
      ]
  ]
  where
    (w, d, l) = m ^. tally
    taken = capturedBy (posBoard (m ^. position)) (m ^. human)
    lvl = ms (levelName (m ^. level))
    (kicker, title, cls, sub) = case m ^. gameStatus of
      Mate winner
        | winner == m ^. human ->
            ( "Checkmate", "You win", "won"
            , "You mated the " <> lvl <> " engine. Play it again, or step up a strength." )
        | otherwise ->
            ( "Checkmate", "Engine wins", "lost"
            , "The engine found a mate. Undo takes you back to your last decision." )
      Draw Stalemate ->
        ( "Stalemate", "Drawn", ""
        , "The side to move has no legal move and is not in check." )
      Draw Repetition ->
        ( "Threefold repetition", "Drawn", ""
        , "The same position appeared three times." )
      Draw FiftyMoves ->
        ( "Fifty-move rule", "Drawn", ""
        , "Fifty moves went by without a capture or a pawn move." )
      Draw InsufficientMaterial ->
        ( "Insufficient material", "Drawn", ""
        , "Neither side has enough left to force a mate." )
      _ -> ( "Game over", "Drawn", "", "" )
    stat k v label = H.div_
      [ HP.class_ "stat", CSS.style_ [ CSS.animationDelay (ms (150 + k * 110 :: Int) <> "ms") ] ]
      [ H.b_ [] [ text v ], H.span_ [] [ text label ] ]
-----------------------------------------------------------------------------
helpOverlay :: View () Model Action
helpOverlay = H.div_ [ HP.class_ "overlay" ]
  [ H.div_ [ HP.class_ "modal help" ]
      [ H.button_ [ HP.class_ "helpClose", HE.onClick CloseHelp, HP.title_ "Close" ] [ text "✕" ]
      , H.div_ [ HP.class_ "helpH" ] [ text "How to play" ]
      , sec "The goal"
      , para $ "Checkmate the other king: attack it so that no move can save it. "
            <> "If your own king is attacked you must deal with that first, and "
            <> "the board glows red to say so."
      , sec "Moving"
      , para $ "Tap a piece to see where it can go, then tap a square. You can "
            <> "also drag. Mint dots are quiet moves, mint rings are captures, "
            <> "and the amber squares show the last move played."
      , sec "Special moves"
      , para $ "Castling slides the king two squares toward a rook and the rook "
            <> "hops over. A pawn that reaches the far side turns into a queen, "
            <> "knight, rook or bishop of your choosing. A pawn that has just "
            <> "jumped two squares can be captured in passing."
      , sec "Draws"
      , para $ "Stalemate, the same position three times, fifty moves without a "
            <> "capture or pawn move, or too little material to mate."
      , sec "The engine"
      , para $ "Four strengths, from one move ahead to four. The bar beside the "
            <> "board is its opinion of the position; a hint marks the move it "
            <> "would play for you. Undo takes back a full move, even after the game ends."
      , sec "Keys"
      , H.div_ [ HP.class_ "keys" ]
          [ key "U", text "undo", key "H", text "this help"
          , key "F", text "flip the board", key "M", text "sound on or off"
          , key "N", text "back to the title", key "esc", text "cancel a selection or promotion" ]
      , H.div_ [ HP.class_ "btnRow" ]
          [ H.button_ [ HP.class_ "btn primary", HE.onClick CloseHelp ] [ text "Got it" ] ]
      ]
  ]
  where
    sec s = H.div_ [ HP.class_ "helpSec" ] [ text s ]
    para s = H.p_ [ HP.class_ "helpP" ] [ text s ]
    key k = H.span_ [ HP.class_ "key" ] [ text k ]
-----------------------------------------------------------------------------
joinCls :: [MisoString] -> MisoString
joinCls = mconcat . map (<> " ")
-----------------------------------------------------------------------------
clsWhen :: Bool -> MisoString -> MisoString
clsWhen True c = c
clsWhen False _ = ""
