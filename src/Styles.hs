-----------------------------------------------------------------------------
-- | The look: a chess study late at night.  A cool graphite room with one
-- pool of light, a board of bone and slate stone sitting on a dark
-- plinth, and everything else kept dim so the position is the only
-- bright thing in the room.  Two accents only: mint for what you can do,
-- amber for what just happened.
-----------------------------------------------------------------------------
module Styles (skin) where
-----------------------------------------------------------------------------
import           Miso ((=:))
import qualified Miso.CSS as CSS
import           Miso.CSS
  ( StyleSheet, sheet_, selector_, keyframes_, from_, to_, at, pct
  , media_, rule_, screen_, and_, maxWidth_, hover_, px
  )
import           Miso.CSS.Types (MediaQuery(..))
import           Miso.CSS.Color hiding (coral)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- palette: the room, the stone, the two accents
night, plinth, bone, slate, mint, amber, coral, mist, dim :: Color
night  = RGB  16  20  25   -- the room
plinth = RGB  27  33  42   -- panels and the board's base
bone   = RGB 231 223 204   -- light squares, primary text
slate  = RGB  95 107 128   -- dark squares
mint   = RGB 127 215 181   -- what you can do
amber  = RGB 233 180  76   -- what just happened, whose turn
coral  = RGB 228  87  79   -- check
mist   = RGB 138 148 166   -- secondary text
dim    = RGB  89  99 118   -- tertiary text
-----------------------------------------------------------------------------
serif, sans :: MisoString
serif = "'Cormorant Garamond', 'Cormorant', Georgia, 'Times New Roman', serif"
sans = "'Manrope', 'Avenir Next', 'Segoe UI', system-ui, sans-serif"
-----------------------------------------------------------------------------
roomBackground :: MisoString
roomBackground = mconcat
  [ "radial-gradient(60% 45% at 50% 0%, rgba(150,170,205,.13), rgba(0,0,0,0) 70%), "
  , "radial-gradient(90% 60% at 50% 110%, rgba(0,0,0,.5), rgba(0,0,0,0) 60%), "
  , "linear-gradient(180deg, #131820 0%, #101419 50%, #0C0F13 100%)"
  ]
-----------------------------------------------------------------------------
ledgerBackground :: MisoString
ledgerBackground = "linear-gradient(180deg, rgba(30,37,48,.92), rgba(22,27,35,.94))"
-----------------------------------------------------------------------------
plinthBackground :: MisoString
plinthBackground = "linear-gradient(180deg, #232B37 0%, #181E27 100%)"
-----------------------------------------------------------------------------
-- a faint stone grain laid over the squares
grain :: MisoString
grain = mconcat
  [ "url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='180' height='180'>"
  , "<filter id='g'><feTurbulence type='fractalNoise' baseFrequency='.85' numOctaves='2' stitchTiles='stitch'/>"
  , "<feColorMatrix values='0 0 0 0 0.5 0 0 0 0 0.5 0 0 0 0 0.5 0 0 0 .6 0'/></filter>"
  , "<rect width='100%' height='100%' filter='url(%23g)'/></svg>\")"
  ]
-----------------------------------------------------------------------------
hairline :: MisoString
hairline = "1px solid rgba(255,255,255,.07)"
-----------------------------------------------------------------------------
skin :: StyleSheet
skin = sheet_
  [ selector_ ":root"
      [ "--bs" =: "min(calc(100vw - 380px), calc(100dvh - 120px), 720px)"
      , "--sq" =: "calc(var(--bs) / 8)"
      ]
  , selector_ "*" [ CSS.boxSizing "border-box" ]
  , selector_ "html, body"
      [ CSS.margin "0", CSS.minHeight "100%" ]
  , selector_ "body"
      [ CSS.background roomBackground
      , "background-attachment" =: "fixed"
      , CSS.color bone
      , CSS.fontFamily sans
      , CSS.fontSize "15px"
      , CSS.userSelect "none"
      , "-webkit-tap-highlight-color" =: "transparent"
      , "-webkit-text-size-adjust" =: "100%"
      , "-webkit-font-smoothing" =: "antialiased"
      , CSS.overscrollBehavior "none"
      ]
  , selector_ "button"
      [ CSS.fontFamily sans, CSS.cursor "pointer", CSS.border "none"
      , CSS.background "none", "color" =: "inherit", CSS.padding "0"
      , "touch-action" =: "manipulation"
      ]
  , selector_ "button:focus-visible"
      [ CSS.outline "2px solid #7FD7B5", CSS.outlineOffset "3px" ]
  , selector_ ".psvg" [ CSS.display "block" ]
  -- title screen ---------------------------------------------------------------
  , selector_ ".title"
      [ CSS.minHeight "100dvh", CSS.display "flex", CSS.flexDirection "column"
      , CSS.alignItems "center", CSS.justifyContent "center"
      , CSS.padding "48px 20px 56px", CSS.position "relative"
      , CSS.overflow "hidden", CSS.textAlign "center"
      ]
  , selector_ ".heroKnight"
      [ CSS.position "absolute", CSS.right "-4vmin", CSS.top "50%"
      , CSS.transform "translateY(-58%) rotate(-6deg)"
      , CSS.width "min(76vmin, 620px)", CSS.opacity 0.11
      , CSS.pointerEvents "none", CSS.filter "blur(.6px)"
      ]
  , selector_ ".heroKnight .psvg" [ CSS.width "100%", CSS.height "auto" ]
  , selector_ ".wordmark"
      [ CSS.fontFamily serif, CSS.fontStyle "italic", CSS.fontWeight "600"
      , CSS.fontSize "clamp(96px, 19vmin, 176px)", CSS.lineHeight "0.88"
      , CSS.letterSpacing "-.015em", CSS.margin "0", CSS.color bone
      , CSS.textShadow "0 24px 70px rgba(0,0,0,.65)", CSS.position "relative"
      ]
  , selector_ ".tagline"
      [ CSS.marginTop "18px", CSS.fontSize "16px", CSS.color mist
      , CSS.maxWidth "36ch", CSS.lineHeight "1.55", CSS.position "relative"
      ]
  , selector_ ".choose"
      [ CSS.marginTop "40px", CSS.display "flex", CSS.flexDirection "column"
      , CSS.gap "24px", CSS.alignItems "center", CSS.width "100%"
      , CSS.maxWidth "640px", CSS.position "relative"
      ]
  , selector_ ".sides" [ CSS.display "flex", CSS.gap "12px", CSS.justifyContent "center" ]
  , selector_ ".sideTile"
      [ CSS.width "112px", CSS.padding "12px 8px 11px", CSS.borderRadius "14px"
      , CSS.backgroundColor plinth, CSS.border hairline
      , CSS.display "flex", CSS.flexDirection "column", CSS.alignItems "center"
      , CSS.gap "6px", CSS.color mist, CSS.fontWeight "600", CSS.fontSize "13px"
      , CSS.transition "transform .2s ease, box-shadow .2s ease, border-color .2s ease, color .2s ease"
      ]
  , selector_ ".sideTile .kings"
      [ CSS.height "58px", CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center" ]
  , selector_ ".sideTile .psvg"
      [ CSS.width "56px", CSS.height "56px"
      , CSS.filter "drop-shadow(0 4px 6px rgba(0,0,0,.45))"
      , CSS.transition "transform .25s ease"
      ]
  , selector_ ".sideTile .kings .psvg + .psvg" [ CSS.marginLeft "-22px" ]
  , selector_ ".sideTile.on"
      [ CSS.color bone, CSS.backgroundColor (RGB 34 42 53), CSS.borderColor mint
      , CSS.boxShadow "0 0 0 3px rgba(127,215,181,.22), 0 16px 34px rgba(0,0,0,.45)"
      , CSS.transform "translateY(-3px)"
      ]
  , selector_ ".sideTile.on .psvg" [ CSS.transform "scale(1.08)" ]
  , selector_ ".levels"
      [ CSS.display "grid", CSS.gridTemplateColumns "repeat(4, 1fr)"
      , CSS.gap "10px", CSS.width "100%"
      ]
  , selector_ ".lvl"
      [ CSS.textAlign "left", CSS.padding "12px 14px 13px", CSS.borderRadius "12px"
      , CSS.backgroundColor plinth, CSS.border hairline, CSS.color mist
      , CSS.transition "border-color .2s ease, background .2s ease, color .2s ease"
      ]
  , selector_ ".lvl b"
      [ CSS.display "block", CSS.color bone, CSS.fontSize "15px"
      , CSS.marginBottom "4px", CSS.fontWeight "700"
      ]
  , selector_ ".lvl span" [ CSS.fontSize "12px", CSS.lineHeight "1.45", CSS.display "block" ]
  , selector_ ".lvl.on"
      [ CSS.borderColor mint, CSS.backgroundColor (RGB 34 42 53), CSS.color (RGB 190 200 214) ]
  , selector_ ".playBtn"
      [ CSS.marginTop "4px", CSS.color night, CSS.fontWeight "800"
      , CSS.fontSize "17px", CSS.padding "16px 56px", CSS.borderRadius "999px"
      , CSS.background "linear-gradient(180deg, #9BE5C8 0%, #7FD7B5 60%, #66C9A3 100%)"
      , CSS.boxShadow "0 14px 34px rgba(127,215,181,.28), inset 0 1px 0 rgba(255,255,255,.35)"
      , CSS.transition "transform .15s ease, box-shadow .15s ease"
      ]
  , selector_ ".playBtn:active" [ CSS.transform "translateY(1px)" ]
  , selector_ ".howLink"
      [ CSS.color mist, CSS.textDecoration "underline", "text-underline-offset" =: "4px"
      , CSS.fontSize "14px", CSS.fontWeight "600"
      ]
  , selector_ ".foot"
      [ CSS.position "absolute", CSS.bottom "18px", CSS.color dim, CSS.fontSize "12px" ]
  -- game chrome ------------------------------------------------------------------
  , selector_ ".top"
      [ CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "space-between"
      , CSS.padding "12px 18px 10px", CSS.gap "12px"
      ]
  , selector_ ".brand"
      [ CSS.fontFamily serif, CSS.fontStyle "italic", CSS.fontSize "27px"
      , CSS.fontWeight "600", CSS.letterSpacing "-.01em", CSS.display "flex"
      , CSS.alignItems "center", CSS.gap "8px"
      ]
  , selector_ ".brand .psvg" [ CSS.width "30px", CSS.height "30px" ]
  , selector_ ".hud" [ CSS.color mist, CSS.fontSize "14px", CSS.fontWeight "600" ]
  , selector_ ".tools" [ CSS.display "flex", CSS.gap "6px" ]
  , selector_ ".tool"
      [ CSS.display "inline-flex", CSS.alignItems "center", CSS.gap "6px"
      , CSS.padding "8px 12px", CSS.borderRadius "10px"
      , CSS.background "rgba(255,255,255,.04)", CSS.border hairline
      , CSS.color bone, CSS.fontSize "13px", CSS.fontWeight "600"
      , CSS.transition "background .15s ease, transform .15s ease"
      ]
  , selector_ ".tool:active" [ CSS.transform "translateY(1px)" ]
  -- the stage: eval bar, board, ledger --------------------------------------------
  , selector_ ".stage"
      [ CSS.display "grid"
      , CSS.gridTemplateColumns "14px var(--bs) minmax(250px, 320px)"
      , CSS.gridTemplateRows "auto minmax(0, 1fr) auto auto"
      , "grid-template-areas" =: "\"eval board engine\" \"eval board moves\" \"eval board you\" \"eval board actions\""
      , CSS.gap "0 18px", CSS.justifyContent "center", CSS.alignItems "stretch"
      , CSS.padding "6px 16px 32px"
      , CSS.height "calc(var(--bs) + 58px)"
      ]
  , selector_ ".evalWrap"
      [ "grid-area" =: "eval", CSS.display "flex", CSS.flexDirection "column"
      , CSS.alignItems "center", CSS.gap "8px", CSS.padding "10px 0"
      ]
  , selector_ ".eval"
      [ CSS.flex "1", CSS.width "14px", CSS.borderRadius "7px"
      , CSS.backgroundColor (RGB 38 43 52), CSS.overflow "hidden", CSS.position "relative"
      , CSS.boxShadow "inset 0 0 0 1px rgba(255,255,255,.06)"
      ]
  , selector_ ".evalFill"
      [ CSS.position "absolute", CSS.left "0", CSS.right "0", CSS.bottom "0"
      , CSS.height "var(--share)"
      , CSS.background "linear-gradient(180deg, #F3EEE3, #D8D0BE)"
      , CSS.transition "height .7s cubic-bezier(.2,.8,.2,1), width .7s cubic-bezier(.2,.8,.2,1)"
      ]
  , selector_ ".evalText"
      [ CSS.fontSize "11px", CSS.fontWeight "700", CSS.color mist
      , "font-variant-numeric" =: "tabular-nums", CSS.whiteSpace "nowrap"
      ]
  , selector_ ".boardWrap"
      [ "grid-area" =: "board", CSS.padding "10px", CSS.borderRadius "12px"
      , CSS.background plinthBackground, CSS.position "relative"
      , CSS.boxShadow "0 34px 70px rgba(0,0,0,.6), 0 2px 0 rgba(255,255,255,.05) inset, 0 0 0 1px rgba(255,255,255,.04)"
      , CSS.transition "box-shadow .4s ease"
      ]
  , selector_ ".boardWrap.inCheck"
      [ CSS.boxShadow "0 34px 70px rgba(0,0,0,.6), 0 0 0 2px rgba(228,87,79,.55), 0 0 40px rgba(228,87,79,.25)" ]
  , selector_ ".board"
      [ CSS.position "relative", CSS.width "100%", CSS.aspectRatio "1"
      , CSS.display "grid", CSS.gridTemplateColumns "repeat(8, 1fr)"
      , CSS.gridTemplateRows "repeat(8, 1fr)", CSS.borderRadius "5px"
      , CSS.overflow "hidden", "touch-action" =: "none"
      , CSS.transition "transform .65s cubic-bezier(.65,0,.35,1)"
      ]
  , selector_ ".board.flipped" [ CSS.transform "rotate(180deg)" ]
  , selector_ ".sq" [ CSS.position "relative" ]
  , selector_ ".sq.light" [ CSS.backgroundColor bone ]
  , selector_ ".sq.dark" [ CSS.backgroundColor slate ]
  , selector_ ".grain"
      [ CSS.position "absolute", "inset" =: "0", CSS.pointerEvents "none"
      , CSS.backgroundImage grain, CSS.opacity 0.16, CSS.mixBlendMode "multiply"
      ]
  , selector_ ".coord"
      [ CSS.position "absolute", CSS.fontSize "calc(var(--sq) * .19)"
      , CSS.fontWeight "800", CSS.lineHeight "1", CSS.pointerEvents "none"
      , CSS.opacity 0.85, CSS.transition "transform .65s cubic-bezier(.65,0,.35,1)"
      ]
  , selector_ ".coord.rank" [ CSS.top "6%", CSS.left "7%" ]
  , selector_ ".coord.file" [ CSS.bottom "6%", CSS.right "8%" ]
  , selector_ ".flipped .coord" [ CSS.transform "rotate(180deg)" ]
  , selector_ ".flipped .coord.rank"
      [ CSS.top "auto", CSS.left "auto", CSS.bottom "6%", CSS.right "7%" ]
  , selector_ ".flipped .coord.file"
      [ CSS.bottom "auto", CSS.right "auto", CSS.top "6%", CSS.left "8%" ]
  , selector_ ".sq.light .coord" [ CSS.color slate ]
  , selector_ ".sq.dark .coord" [ CSS.color bone ]
  -- what just happened, what you can do
  , selector_ ".sq.last::after"
      [ "content" =: "''", CSS.position "absolute", "inset" =: "0"
      , CSS.background "rgba(240,186,72,.42)" ]
  , selector_ ".sq.dark.last::after" [ CSS.background "rgba(244,190,70,.58)" ]
  , selector_ ".sq.sel::after"
      [ "content" =: "''", CSS.position "absolute", "inset" =: "0"
      , CSS.background "rgba(127,215,181,.55)" ]
  , selector_ ".sq.dark.sel::after" [ CSS.background "rgba(127,215,181,.68)" ]
  , selector_ ".sq.check::after"
      [ "content" =: "''", CSS.position "absolute", "inset" =: "0"
      , CSS.background "radial-gradient(circle at 50% 50%, rgba(228,87,79,.95) 0%, rgba(228,87,79,.6) 42%, rgba(228,87,79,0) 72%)"
      ]
  , selector_ ".sq.dot::before"
      [ "content" =: "''", CSS.position "absolute", CSS.left "50%", CSS.top "50%"
      , CSS.width "30%", CSS.height "30%", CSS.transform "translate(-50%, -50%)"
      , CSS.borderRadius (pct 50), CSS.background "rgba(127,215,181,.9)"
      , CSS.boxShadow "0 2px 6px rgba(0,0,0,.3)", CSS.zIndex 1
      ]
  , selector_ ".sq.cap::before"
      [ "content" =: "''", CSS.position "absolute", "inset" =: "4%"
      , CSS.borderRadius (pct 50), CSS.border "calc(var(--sq) * .09) solid rgba(127,215,181,.85)"
      , CSS.zIndex 1
      ]
  , selector_ ".sq.hintFrom::after, .sq.hintTo::after"
      [ "content" =: "''", CSS.position "absolute", "inset" =: "7%"
      , CSS.border "3px dashed rgba(127,215,181,.95)", CSS.borderRadius "9px"
      , CSS.animation "dash 1.4s linear infinite"
      ]
  , selector_ ".sq.drop"
      [ CSS.boxShadow "inset 0 0 0 4px rgba(243,238,227,.95)", CSS.zIndex 2 ]
  , selector_ ".sq.own" [ CSS.cursor "grab" ]
  , selector_ ".sq.dot, .sq.cap" [ CSS.cursor "pointer" ]
  -- the pieces
  , selector_ ".pieces"
      [ CSS.position "absolute", "inset" =: "0", CSS.pointerEvents "none", CSS.zIndex 3 ]
  , selector_ ".pc"
      [ CSS.position "absolute", CSS.left "0", CSS.top "0"
      , CSS.width "12.5%", CSS.height "12.5%"
      , CSS.transition "transform .32s cubic-bezier(.2,.75,.25,1), opacity .15s ease"
      , CSS.willChange "transform"
      ]
  , selector_ ".pc .psvg"
      [ CSS.width "100%", CSS.height "100%"
      , CSS.filter "drop-shadow(0 3px 3px rgba(0,0,0,.38))"
      , CSS.transition "transform .65s cubic-bezier(.65,0,.35,1)"
      ]
  , selector_ ".flipped .pc .psvg" [ CSS.transform "rotate(180deg)" ]
  , selector_ ".pc.lifted" [ CSS.opacity 0.28 ]
  , selector_ ".pc.ghost"
      [ CSS.position "fixed", CSS.left "0", CSS.top "0", CSS.zIndex 1000
      , CSS.transform "translate(-50%, -60%) scale(1.14)", CSS.transition "none"
      , CSS.pointerEvents "none", CSS.opacity 0.96
      ]
  , selector_ ".pc.ghost .psvg" [ CSS.filter "drop-shadow(0 14px 14px rgba(0,0,0,.5))" ]
  -- promotion tray
  , selector_ ".promoBackdrop"
      [ CSS.position "absolute", "inset" =: "0", CSS.zIndex 4
      , CSS.background "rgba(16,20,25,.38)" ]
  , selector_ ".promo"
      [ CSS.position "absolute", CSS.width "12.5%", CSS.zIndex 5
      , CSS.display "flex", CSS.flexDirection "column"
      , CSS.background "rgba(20,25,32,.96)", CSS.borderRadius "8px"
      , CSS.boxShadow "0 18px 44px rgba(0,0,0,.65), 0 0 0 1px rgba(255,255,255,.08)"
      , CSS.overflow "hidden", CSS.animation "trayIn .2s cubic-bezier(.2,.8,.2,1)"
      ]
  , selector_ ".promoBtn"
      [ CSS.width "100%", CSS.aspectRatio "1", CSS.display "flex"
      , CSS.alignItems "center", CSS.justifyContent "center"
      , CSS.transition "background .15s ease" ]
  , selector_ ".promoBtn .psvg" [ CSS.width "84%", CSS.height "84%" ]
  , selector_ ".flipped .promoBtn .psvg" [ CSS.transform "rotate(180deg)" ]
  -- the ledger on the right
  , selector_ ".plate, .moves, .actions"
      [ CSS.background ledgerBackground
      , CSS.borderLeft hairline, CSS.borderRight hairline ]
  , selector_ ".plate"
      [ CSS.display "flex", CSS.alignItems "center", CSS.gap "12px"
      , CSS.padding "14px 16px" ]
  , selector_ ".plate.engine"
      [ "grid-area" =: "engine", CSS.borderTop hairline
      , CSS.borderRadius "12px 12px 0 0" ]
  , selector_ ".plate.you" [ "grid-area" =: "you" ]
  , selector_ ".turn"
      [ CSS.width "10px", CSS.height "10px", CSS.borderRadius (pct 50)
      , CSS.background "rgba(255,255,255,.12)", CSS.flexShrink 0
      , CSS.transition "background .3s ease, box-shadow .3s ease" ]
  , selector_ ".plate.active .turn"
      [ CSS.backgroundColor amber, CSS.boxShadow "0 0 14px rgba(233,180,76,.8)" ]
  , selector_ ".plate.thinking .turn" [ CSS.animation "pulse 1s ease-in-out infinite" ]
  , selector_ ".who" [ CSS.flex "1", CSS.minWidth "0" ]
  , selector_ ".who b" [ CSS.display "block", CSS.fontSize "15px", CSS.fontWeight "700" ]
  , selector_ ".who span" [ CSS.color mist, CSS.fontSize "12px", CSS.fontWeight "600" ]
  , selector_ ".plate.active .who span" [ CSS.color amber ]
  , selector_ ".tray"
      [ CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "flex-end"
      , CSS.flexWrap "wrap", CSS.maxWidth "150px" ]
  , selector_ ".tray .psvg"
      [ CSS.width "22px", CSS.height "22px", CSS.marginLeft "-7px"
      , CSS.filter "drop-shadow(0 1px 1px rgba(0,0,0,.5))" ]
  , selector_ ".tray .psvg:first-child" [ CSS.marginLeft "0" ]
  , selector_ ".adv"
      [ CSS.color amber, CSS.fontWeight "800", CSS.fontSize "12px", CSS.marginLeft "8px" ]
  , selector_ ".moves"
      [ "grid-area" =: "moves", CSS.overflowY "auto", CSS.minHeight "0"
      , CSS.padding "6px 0", CSS.display "flex", CSS.flexDirection "column"
      , CSS.scrollBehavior "smooth"
      , CSS.borderTop hairline, CSS.borderBottom hairline
      , "scrollbar-width" =: "thin", "scrollbar-color" =: "rgba(255,255,255,.12) transparent"
      ]
  , selector_ ".mrow"
      [ CSS.display "grid", CSS.gridTemplateColumns "38px 1fr 1fr"
      , CSS.alignItems "center", CSS.padding "1px 12px", CSS.fontSize "14px"
      , CSS.fontWeight "600" ]
  , selector_ ".mno"
      [ CSS.color dim, CSS.fontWeight "600", "font-variant-numeric" =: "tabular-nums"
      , CSS.paddingLeft "4px" ]
  , selector_ ".mv"
      [ CSS.padding "4px 8px", CSS.borderRadius "6px", CSS.color (RGB 205 210 220) ]
  , selector_ ".mv.cur" [ CSS.background "rgba(233,180,76,.16)", CSS.color amber ]
  , selector_ ".movesEmpty"
      [ CSS.color dim, CSS.fontSize "13px", CSS.padding "14px 16px"
      , CSS.textAlign "center", CSS.lineHeight "1.5" ]
  , selector_ ".actions"
      [ "grid-area" =: "actions", CSS.display "flex", CSS.gap "8px"
      , CSS.padding "12px 12px 14px", CSS.borderBottom hairline
      , CSS.borderRadius "0 0 12px 12px" ]
  , selector_ ".act"
      [ CSS.flex "1", CSS.padding "11px 4px", CSS.borderRadius "10px"
      , CSS.background "rgba(255,255,255,.05)", CSS.border hairline
      , CSS.color bone, CSS.fontWeight "700", CSS.fontSize "13px"
      , CSS.transition "background .15s ease, transform .15s ease, opacity .2s ease" ]
  , selector_ ".act:disabled" [ CSS.opacity 0.32, CSS.cursor "default" ]
  , selector_ ".act.primary"
      [ CSS.backgroundColor mint, CSS.color night, CSS.borderColor (RGBA 0 0 0 0) ]
  , selector_ ".act:active:not(:disabled)" [ CSS.transform "translateY(1px)" ]
  -- overlays ---------------------------------------------------------------------
  , selector_ ".overlay"
      [ CSS.position "fixed", "inset" =: "0", CSS.zIndex 50
      , CSS.background "rgba(8,10,14,.62)", CSS.backdropFilter "blur(6px)"
      , "-webkit-backdrop-filter" =: "blur(6px)"
      , CSS.display "flex", CSS.alignItems "center", CSS.justifyContent "center"
      , CSS.padding "20px", CSS.animation "fadeIn .25s ease-out"
      ]
  , selector_ ".modal"
      [ CSS.width "min(460px, 100%)", CSS.maxHeight "calc(100dvh - 40px)"
      , CSS.overflowY "auto"
      , CSS.background "linear-gradient(180deg, #1D2432, #141A22)"
      , CSS.border "1px solid rgba(255,255,255,.09)", CSS.borderRadius "18px"
      , CSS.padding "28px 26px 24px", CSS.textAlign "center", CSS.position "relative"
      , CSS.boxShadow "0 40px 90px rgba(0,0,0,.65)"
      , CSS.animation "rise .4s cubic-bezier(.2,.8,.2,1)"
      ]
  , selector_ ".resultKicker"
      [ CSS.color mist, CSS.fontSize "13px", CSS.fontWeight "700" ]
  , selector_ ".resultTitle"
      [ CSS.fontFamily serif, CSS.fontStyle "italic", CSS.fontWeight "600"
      , CSS.fontSize "52px", CSS.lineHeight "1", CSS.margin "8px 0 10px" ]
  , selector_ ".resultTitle.won" [ CSS.color mint ]
  , selector_ ".resultTitle.lost" [ CSS.color coral ]
  , selector_ ".resultSub"
      [ CSS.color mist, CSS.fontSize "15px", CSS.lineHeight "1.55"
      , CSS.maxWidth "34ch", CSS.margin "0 auto" ]
  , selector_ ".stats"
      [ CSS.display "grid", CSS.gridTemplateColumns "repeat(3, 1fr)", CSS.gap "8px"
      , CSS.margin "22px 0 20px" ]
  , selector_ ".stat"
      [ CSS.background "rgba(255,255,255,.04)", CSS.borderRadius "10px"
      , CSS.padding "10px 6px", CSS.animation "rise .5s cubic-bezier(.2,.8,.2,1) both" ]
  , selector_ ".stat b" [ CSS.display "block", CSS.fontSize "22px", CSS.fontWeight "800" ]
  , selector_ ".stat span" [ CSS.fontSize "11px", CSS.color mist, CSS.fontWeight "600" ]
  , selector_ ".btnRow"
      [ CSS.display "flex", CSS.gap "8px", CSS.flexWrap "wrap", CSS.justifyContent "center" ]
  , selector_ ".btn"
      [ CSS.padding "12px 18px", CSS.borderRadius "12px", CSS.fontWeight "700"
      , CSS.fontSize "14px", CSS.background "rgba(255,255,255,.06)"
      , CSS.border "1px solid rgba(255,255,255,.09)", CSS.color bone
      , CSS.transition "background .15s ease, transform .15s ease" ]
  , selector_ ".btn.primary" [ CSS.backgroundColor mint, CSS.color night, CSS.borderColor (RGBA 0 0 0 0) ]
  , selector_ ".btn:active" [ CSS.transform "translateY(1px)" ]
  , selector_ ".modal.help" [ CSS.textAlign "left", CSS.width "min(520px, 100%)" ]
  , selector_ ".helpClose"
      [ CSS.position "absolute", CSS.top "14px", CSS.right "14px", CSS.width "34px"
      , CSS.height "34px", CSS.borderRadius (pct 50), CSS.color mist
      , CSS.background "rgba(255,255,255,.06)", CSS.fontSize "16px" ]
  , selector_ ".helpH"
      [ CSS.fontFamily serif, CSS.fontStyle "italic", CSS.fontWeight "600"
      , CSS.fontSize "38px", CSS.lineHeight "1", CSS.margin "0 0 4px" ]
  , selector_ ".helpSec"
      [ CSS.marginTop "18px", CSS.fontSize "14px", CSS.fontWeight "800", CSS.color bone ]
  , selector_ ".helpP"
      [ CSS.margin "6px 0 0", CSS.color mist, CSS.fontSize "14px", CSS.lineHeight "1.6" ]
  , selector_ ".keys"
      [ CSS.display "grid", CSS.gridTemplateColumns "auto 1fr", CSS.gap "6px 14px"
      , CSS.margin "8px 0 0", CSS.fontSize "14px", CSS.color mist, CSS.alignItems "center" ]
  , selector_ ".key"
      [ CSS.display "inline-block", CSS.minWidth "28px", CSS.textAlign "center"
      , CSS.padding "3px 8px", CSS.borderRadius "6px", CSS.color bone
      , CSS.background "rgba(255,255,255,.08)", CSS.border "1px solid rgba(255,255,255,.1)"
      , CSS.fontWeight "700", CSS.fontSize "12px" ]
  , selector_ ".modal .btnRow" [ CSS.marginTop "22px" ]
  -- hover only where hover exists --------------------------------------------------
  , media_ (hover_ "hover")
      [ rule_ ".tool:hover" [ CSS.background "rgba(255,255,255,.09)" ]
      , rule_ ".act:hover:not(:disabled)" [ CSS.background "rgba(255,255,255,.1)" ]
      , rule_ ".act.primary:hover" [ CSS.backgroundColor (RGB 155 229 200) ]
      , rule_ ".btn:hover" [ CSS.background "rgba(255,255,255,.11)" ]
      , rule_ ".btn.primary:hover" [ CSS.backgroundColor (RGB 155 229 200) ]
      , rule_ ".playBtn:hover" [ CSS.transform "translateY(-2px)", CSS.boxShadow "0 20px 40px rgba(127,215,181,.36)" ]
      , rule_ ".sideTile:hover, .lvl:hover" [ CSS.borderColor (RGBA 255 255 255 0.18) ]
      , rule_ ".sq.own:hover" [ CSS.filter "brightness(1.07)" ]
      , rule_ ".promoBtn:hover" [ CSS.background "rgba(127,215,181,.28)" ]
      , rule_ ".mv:hover" [ CSS.background "rgba(255,255,255,.06)" ]
      ]
  -- phones and narrow windows -----------------------------------------------------
  , media_ (screen_ `and_` maxWidth_ (px 880))
      [ rule_ ":root" [ "--bs" =: "min(calc(100vw - 20px), calc(100dvh - 330px))" ]
      , rule_ ".stage"
          [ CSS.gridTemplateColumns "minmax(0, 1fr)"
          , CSS.gridTemplateRows "none"
          , "grid-template-areas" =: "\"engine\" \"eval\" \"board\" \"you\" \"moves\" \"actions\""
          , CSS.gap "8px 0", CSS.padding "0 10px 24px"
          , CSS.width "calc(var(--bs) + 20px)", CSS.margin "0 auto"
          , CSS.height "auto"
          ]
      , rule_ ".evalWrap" [ CSS.flexDirection "row", CSS.padding "0 2px", CSS.gap "8px" ]
      , rule_ ".eval" [ CSS.width "auto", CSS.height "8px", CSS.borderRadius "4px" ]
      , rule_ ".evalFill"
          [ CSS.top "0", CSS.bottom "0", CSS.right "auto", CSS.height "100%"
          , CSS.width "var(--share)"
          , CSS.background "linear-gradient(90deg, #F3EEE3, #D8D0BE)" ]
      , rule_ ".boardWrap" [ CSS.padding "6px", CSS.borderRadius "9px" ]
      , rule_ ".plate, .moves, .actions" [ CSS.borderRadius "12px", CSS.border hairline ]
      , rule_ ".plate" [ CSS.padding "10px 14px" ]
      , rule_ ".moves" [ CSS.maxHeight "126px" ]
      , rule_ ".top" [ CSS.padding "10px 12px 8px" ]
      , rule_ ".brand" [ CSS.fontSize "23px" ]
      , rule_ ".tool span" [ CSS.display "none" ]
      , rule_ ".tool" [ CSS.padding "8px 10px" ]
      , rule_ ".hud" [ CSS.display "none" ]
      , rule_ ".levels" [ CSS.gridTemplateColumns "1fr 1fr" ]
      , rule_ ".sideTile" [ CSS.width "98px" ]
      , rule_ ".wordmark" [ CSS.fontSize "clamp(84px, 24vmin, 140px)" ]
      , rule_ ".heroKnight" [ CSS.right "-24vmin", CSS.opacity 0.08 ]
      , rule_ ".title" [ CSS.padding "36px 16px 60px" ]
      , rule_ ".resultTitle" [ CSS.fontSize "44px" ]
      , rule_ ".modal" [ CSS.padding "24px 18px 20px" ]
      ]
  -- reduced motion -----------------------------------------------------------------
  , media_ (MediaQuery "(prefers-reduced-motion: reduce)")
      [ rule_ "*"
          [ "animation-duration" =: ".01ms"
          , "animation-iteration-count" =: "1"
          , "transition-duration" =: ".01ms"
          ]
      ]
  -- keyframes ------------------------------------------------------------------------
  , keyframes_ "fadeIn" [ from_ [ CSS.opacity 0 ], to_ [ CSS.opacity 1 ] ]
  , keyframes_ "rise"
      [ from_ [ CSS.transform "translateY(18px) scale(.97)", CSS.opacity 0 ]
      , to_ [ CSS.transform "translateY(0) scale(1)", CSS.opacity 1 ]
      ]
  , keyframes_ "trayIn"
      [ from_ [ CSS.transform "scaleY(.6)", CSS.opacity 0, CSS.transformOrigin "top" ]
      , to_ [ CSS.transform "scaleY(1)", CSS.opacity 1, CSS.transformOrigin "top" ]
      ]
  , keyframes_ "pulse"
      [ from_ [ CSS.opacity 1, CSS.transform "scale(1)" ]
      , at (pct 50) [ CSS.opacity 0.45, CSS.transform "scale(.8)" ]
      , to_ [ CSS.opacity 1, CSS.transform "scale(1)" ]
      ]
  , keyframes_ "dash"
      [ from_ [ CSS.opacity 0.55 ], at (pct 50) [ CSS.opacity 1 ], to_ [ CSS.opacity 0.55 ] ]
  ]
