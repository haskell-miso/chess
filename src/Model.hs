-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- | Application state for miso-chess, with lenses via "Miso.Lens.TH".
-- The rules live in "Chess" and the engine in "Engine"; this module only
-- holds the game record and the pieces of UI state that must survive a
-- redraw (selection, a pending promotion, the hint, the eval bar).
-----------------------------------------------------------------------------
module Model where
-----------------------------------------------------------------------------
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
-----------------------------------------------------------------------------
import           Miso.Lens.TH (makeLenses)
-----------------------------------------------------------------------------
import           Chess
import           Engine (Level(..), Score(..))
-----------------------------------------------------------------------------
-- | One half-move in the game record: the position it was played from,
-- the move, and its notation (computed once, when played).
data Ply = Ply
  { plyBefore :: Position
  , plyMove :: Move
  , plySan :: String
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
data Phase
  = Title
  | Playing
  | Over          -- ^ the game has a result; the board stays reviewable
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | Which colour the player asked for on the title screen.
data SideChoice = PlayWhite | PlayBlack | PlayRandom
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | A piece under the pointer.  Pressing one of your own men arms a drag
-- without committing to one, so a plain click still selects; once the
-- pointer has travelled far enough the piece leaves the board and follows
-- the cursor until it is released.
data Drag
  = Armed Square (Double, Double)
    -- ^ pressed on this square, at this viewport point
  | Lifted Square (Maybe Square)
    -- ^ lifted from this square, currently over that one
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | The square a drag started from, armed or lifted.
dragFrom :: Drag -> Square
dragFrom = \case
  Armed sq _ -> sq
  Lifted sq _ -> sq
-----------------------------------------------------------------------------
-- | The square a lifted piece is being held over, if it is held over one.
dragOver :: Drag -> Maybe Square
dragOver = \case
  Armed _ _ -> Nothing
  Lifted _ over -> over
-----------------------------------------------------------------------------
-- | Is the piece off the board and on the cursor?
lifted :: Maybe Drag -> Bool
lifted = \case
  Just (Lifted _ _) -> True
  _ -> False
-----------------------------------------------------------------------------
data Model = Model
  { _position :: Position
  , _plies :: [Ply]              -- ^ newest first
  , _phase :: Phase
  , _gameStatus :: Status
  , _human :: Side
  , _level :: Level
  , _sideChoice :: SideChoice
  , _selected :: Maybe Square
  , _targets :: [Move]           -- ^ legal moves from the selected square
  , _drag :: Maybe Drag          -- ^ the piece the pointer is carrying
  , _dropped :: Maybe Int        -- ^ a piece placed by hand: it must not slide
  , _promotion :: Maybe (Square, Square) -- ^ a pawn waiting for its piece
  , _hint :: Maybe Move
  , _thinking :: Bool
  , _pacer :: Int                -- ^ stamps scheduled engine replies
  , _evalScore :: Score          -- ^ the engine's view, from White's side
  , _flipped :: Bool
  , _reviewing :: Bool           -- ^ result panel dismissed to look at the board
  , _tally :: (Int, Int, Int)    -- ^ wins, draws, losses this session
  , _soundOn :: Bool
  , _showHelp :: Bool
  , _heldKeys :: IntSet
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
makeLenses ''Model
-----------------------------------------------------------------------------
data Action
  = NoOp
  | ChooseSide SideChoice
  | ChooseLevel Level
  | StartGame                 -- ^ title button; inits audio and drag
  | Begin [Double]            -- ^ randomness for the side pick
  | Tap Square
  | DragPress Square (Double, Double) -- ^ pointer down on a square
  | DragMove (Double, Double)         -- ^ pointer moved while a drag is armed
  | DragOver Square                   -- ^ a lifted piece crossed onto a square
  | DragDrop Square                   -- ^ released over a square
  | DragCancel                        -- ^ released anywhere else, or cancelled
  | DragSettle                        -- ^ a dropped piece may animate again
  | Promote PieceType
  | CancelPromotion
  | EngineReply Int [Double]  -- ^ pacer stamp, search randomness
  | Undo
  | AskHint
  | Flip
  | ReviewBoard
  | CopyPGN
  | BackToTitle
  | ToggleSound
  | ShowHelp
  | CloseHelp
  | FullScreen
  | Keys IntSet
-----------------------------------------------------------------------------
initialModel :: Model
initialModel = Model
  { _position = startPosition
  , _plies = []
  , _phase = Title
  , _gameStatus = Ongoing
  , _human = White
  , _level = Club
  , _sideChoice = PlayWhite
  , _selected = Nothing
  , _targets = []
  , _drag = Nothing
  , _dropped = Nothing
  , _promotion = Nothing
  , _hint = Nothing
  , _thinking = False
  , _pacer = 0
  , _evalScore = Centipawns 0
  , _flipped = False
  , _reviewing = False
  , _tally = (0, 0, 0)
  , _soundOn = True
  , _showHelp = False
  , _heldKeys = IS.empty
  }
-----------------------------------------------------------------------------
engineSide :: Model -> Side
engineSide m = opponent (_human m)
-----------------------------------------------------------------------------
humanToMove :: Model -> Bool
humanToMove m = _phase m == Playing && posTurn (_position m) == _human m
-----------------------------------------------------------------------------
-- | Positions before the current one, oldest last (as 'status' wants).
history :: Model -> [Position]
history = map plyBefore . _plies
-----------------------------------------------------------------------------
lastMove :: Model -> Maybe Move
lastMove m = case _plies m of
  (p : _) -> Just (plyMove p)
  [] -> Nothing
-----------------------------------------------------------------------------
-- | Whole moves played so far, for the "move 12" readout.
moveNumber :: Model -> Int
moveNumber = posFullmove . _position
