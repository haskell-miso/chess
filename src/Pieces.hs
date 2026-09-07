-----------------------------------------------------------------------------
-- | The pieces as inline SVG: the Cburnett set (the outline artwork
-- Wikipedia and lichess use), authored in a 45×45 box.  White pieces are
-- ivory with a graphite outline; black pieces are graphite with pale
-- engraved lines, so the two sides read at a glance on stone squares.
-----------------------------------------------------------------------------
module Pieces
  ( pieceSvg
  , pieceGlyph
  ) where
-----------------------------------------------------------------------------
import           Miso (View, Attribute)
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Property as SP
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Chess
-----------------------------------------------------------------------------
-- | A piece drawn to fill its box; the caller sizes it with CSS.
pieceSvg :: Side -> PieceType -> View context model action
pieceSvg side pt =
  SVG.svg_
    [ SP.viewBox_ "0 0 45 45", HP.class_ "psvg" ]
    [ SVG.g_
        [ SP.stroke_ outline
        , SP.strokeWidth_ "1.5"
        , SP.strokeLinecap_ "round"
        , SP.strokeLinejoin_ "round"
        , SP.fillRule_ "evenodd"
        , SP.fill_ "none"
        ]
        (shapes side pt)
    ]
  where
    outline = if side == White then "#2B2A28" else "#111213"
-----------------------------------------------------------------------------
-- | The unicode figurine, for text contexts.
pieceGlyph :: Side -> PieceType -> MisoString
pieceGlyph White = \case
  King -> "♔"; Queen -> "♕"; Rook -> "♖"; Bishop -> "♗"; Knight -> "♘"; Pawn -> "♙"
pieceGlyph Black = \case
  King -> "♚"; Queen -> "♛"; Rook -> "♜"; Bishop -> "♝"; Knight -> "♞"; Pawn -> "♟"
-----------------------------------------------------------------------------
pth :: MisoString -> [Attribute model action] -> View context model action
pth dd extra = SVG.path_ (SP.d_ dd : extra)
-----------------------------------------------------------------------------
shapes :: Side -> PieceType -> [View context model action]
shapes side pt = case pt of

  Pawn ->
    [ pth pawnD [ SP.fill_ body, SP.strokeLinejoin_ "miter" ] ]

  King
    | white ->
        [ pth "M22.5 11.63V6M20 8h5"
            [ SP.fill_ "none", SP.strokeLinejoin_ "miter" ]
        , pth "M22.5 25s4.5-7.5 3-10.5c0 0-1-2.5-3-2.5s-3 2.5-3 2.5c-1.5 3 3 10.5 3 10.5"
            [ SP.fill_ body, SP.strokeLinecap_ "butt", SP.strokeLinejoin_ "miter" ]
        , pth "M11.5 37c5.5 3.5 15.5 3.5 21 0v-7s9-4.5 6-10.5c-4-6.5-13.5-3.5-16 4V27v-3.5c-3.5-7.5-13-10.5-16-4-3 6 5 10 5 10z"
            [ SP.fill_ body ]
        , pth "M11.5 30c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0"
            [ SP.fill_ "none", SP.stroke_ detail ]
        ]
    | otherwise ->
        [ pth "M22.5 11.6V6"
            [ SP.fill_ "none", SP.strokeLinejoin_ "miter" ]
        , pth "M22.5 25s4.5-7.5 3-10.5c0 0-1-2.5-3-2.5s-3 2.5-3 2.5c-1.5 3 3 10.5 3 10.5"
            [ SP.fill_ body, SP.strokeLinecap_ "butt", SP.strokeLinejoin_ "miter" ]
        , pth "M11.5 37a22.3 22.3 0 0 0 21 0v-7s9-4.5 6-10.5c-4-6.5-13.5-3.5-16 4V27v-3.5c-3.5-7.5-13-10.5-16-4-3 6 5 10 5 10z"
            [ SP.fill_ body ]
        , pth "M20 8h5"
            [ SP.fill_ "none", SP.strokeLinejoin_ "miter" ]
        , pth "M32 29.5s8.5-4 6-9.7C34.1 14 25 18 22.5 24.6v2.1-2.1C20 18 9.9 14 7 19.9c-2.5 5.6 4.8 9 4.8 9"
            [ SP.fill_ "none", SP.stroke_ detail ]
        , pth "M11.5 30c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0m-21 3.5c5.5-3 15.5-3 21 0"
            [ SP.fill_ "none", SP.stroke_ detail ]
        ]

  Queen
    | white ->
        [ pth "M8 12a2 2 0 1 1-4 0 2 2 0 1 1 4 0m16.5-4.5a2 2 0 1 1-4 0 2 2 0 1 1 4 0M41 12a2 2 0 1 1-4 0 2 2 0 1 1 4 0M16 8.5a2 2 0 1 1-4 0 2 2 0 1 1 4 0M33 9a2 2 0 1 1-4 0 2 2 0 1 1 4 0"
            [ SP.fill_ body ]
        , pth "M9 26c8.5-1.5 21-1.5 27 0l2-12-7 11V11l-5.5 13.5-3-15-3 15-5.5-14V25L7 14z"
            [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
        , pth "M9 26c0 2 1.5 2 2.5 4 1 1.5 1 1 .5 3.5-1.5 1-1.5 2.5-1.5 2.5-1.5 1.5.5 2.5.5 2.5 6.5 1 16.5 1 23 0 0 0 1.5-1 0-2.5 0 0 .5-1.5-1-2.5-.5-2.5-.5-2 .5-3.5 1-2 2.5-2 2.5-4-8.5-1.5-18.5-1.5-27 0z"
            [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
        , pth "M11.5 30c3.5-1 18.5-1 22 0M12 33.5c6-1 15-1 21 0"
            [ SP.fill_ "none", SP.stroke_ detail ]
        ]
    | otherwise ->
        [ SVG.circle_ [ SP.cx_ "6",    SP.cy_ "12", SP.r_ "2.75", SP.fill_ body, SP.stroke_ "none" ]
        , SVG.circle_ [ SP.cx_ "14",   SP.cy_ "9",  SP.r_ "2.75", SP.fill_ body, SP.stroke_ "none" ]
        , SVG.circle_ [ SP.cx_ "22.5", SP.cy_ "8",  SP.r_ "2.75", SP.fill_ body, SP.stroke_ "none" ]
        , SVG.circle_ [ SP.cx_ "31",   SP.cy_ "9",  SP.r_ "2.75", SP.fill_ body, SP.stroke_ "none" ]
        , SVG.circle_ [ SP.cx_ "39",   SP.cy_ "12", SP.r_ "2.75", SP.fill_ body, SP.stroke_ "none" ]
        , pth "M9 26c8.5-1.5 21-1.5 27 0l2.5-12.5L31 25l-.3-14.1-5.2 13.6-3-14.5-3 14.5-5.2-13.6L14 25 6.5 13.5z"
            [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
        , pth "M9 26c0 2 1.5 2 2.5 4 1 1.5 1 1 .5 3.5-1.5 1-1.5 2.5-1.5 2.5-1.5 1.5.5 2.5.5 2.5 6.5 1 16.5 1 23 0 0 0 1.5-1 0-2.5 0 0 .5-1.5-1-2.5-.5-2.5-.5-2 .5-3.5 1-2 2.5-2 2.5-4-8.5-1.5-18.5-1.5-27 0z"
            [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
        , pth "M11 38.5a35 35 1 0 0 23 0"
            [ SP.fill_ "none", SP.strokeLinecap_ "butt" ]
        , pth "M11 29a35 35 1 0 1 23 0m-21.5 2.5h20m-21 3a35 35 1 0 0 22 0m-23 3a35 35 1 0 0 24 0"
            [ SP.fill_ "none", SP.stroke_ detail ]
        ]

  Rook
    | white ->
        [ pth "M9 39h27v-3H9zm3-3v-4h21v4zm-1-22V9h4v2h5V9h5v2h5V9h4v5"
            [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
        , pth "m34 14-3 3H14l-3-3"
            [ SP.fill_ body ]
        , pth "M31 17v12.5H14V17"
            [ SP.fill_ body, SP.strokeLinecap_ "butt", SP.strokeLinejoin_ "miter" ]
        , pth "m31 29.5 1.5 2.5h-20l1.5-2.5"
            [ SP.fill_ body ]
        , pth "M11 14h23"
            [ SP.fill_ "none", SP.stroke_ detail, SP.strokeLinejoin_ "miter" ]
        ]
    | otherwise ->
        [ pth "M9 39h27v-3H9zm3.5-7 1.5-2.5h17l1.5 2.5zm-.5 4v-4h21v4z"
            [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
        , pth "M14 29.5v-13h17v13z"
            [ SP.fill_ body, SP.strokeLinecap_ "butt", SP.strokeLinejoin_ "miter" ]
        , pth "M14 16.5 11 14h23l-3 2.5zM11 14V9h4v2h5V9h5v2h5V9h4v5z"
            [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
        , pth "M12 35.5h21m-20-4h19m-18-2h17m-17-13h17M11 14h23"
            [ SP.fill_ "none", SP.stroke_ detail, SP.strokeWidth_ "1", SP.strokeLinejoin_ "miter" ]
        ]

  Bishop ->
    [ pth "M9 36c3.39-.97 10.11.43 13.5-2 3.39 2.43 10.11 1.03 13.5 2 0 0 1.65.54 3 2-.68.97-1.65.99-3 .5-3.39-.97-10.11.46-13.5-1-3.39 1.46-10.11.03-13.5 1-1.35.49-2.32.47-3-.5 1.35-1.94 3-2 3-2z"
        [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
    , pth "M15 32c2.5 2.5 12.5 2.5 15 0 .5-1.5 0-2 0-2 0-2.5-2.5-4-2.5-4 5.5-1.5 6-11.5-5-15.5-11 4-10.5 14-5 15.5 0 0-2.5 1.5-2.5 4 0 0-.5.5 0 2z"
        [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
    , pth "M25 8a2.5 2.5 0 1 1-5 0 2.5 2.5 0 1 1 5 0z"
        [ SP.fill_ body, SP.strokeLinecap_ "butt" ]
    , pth "M17.5 26h10M15 30h15m-7.5-14.5v5M20 18h5"
        [ SP.fill_ "none", SP.stroke_ detail, SP.strokeLinejoin_ "miter" ]
    ]

  Knight ->
    [ pth "M22 10c10.5 1 16.5 8 16 29H15c0-9 10-6.5 8-21"
        [ SP.fill_ body ]
    , pth "M24 18c.38 2.91-5.55 7.37-8 9-3 2-2.82 4.34-5 4-1.04-.94 1.41-3.04 0-3-1 0 .19 1.23-1 2-1 0-4 1-4-4 0-2 6-12 6-12s1.89-1.9 2-3.5c-.73-1-.5-2-.5-3 1-1 3 2.5 3 2.5h2s.78-2 2.5-3c1 0 1 3 1 3"
        [ SP.fill_ body ]
    , pth "M9.5 25.5a.5.5 0 1 1-1 0 .5.5 0 1 1 1 0m5.43-9.75a.5 1.5 30 1 1-.86-.5.5 1.5 30 1 1 .86.5"
        [ SP.fill_ detail, SP.stroke_ detail ]
    ] ++
    [ pth "m24.55 10.4-.45 1.45.5.15c3.15 1 5.65 2.49 7.9 6.75S35.75 29.06 35.25 39l-.05.5h2.25l.05-.5c.5-10.06-.88-16.85-3.25-21.34s-5.79-6.64-9.19-7.16z"
        [ SP.fill_ detail, SP.stroke_ "none" ]
    | not white
    ]
  where
    white = side == White
    body = if white then "#F4EFE4" else "#2C2F36"
    detail = if white then "#2B2A28" else "#D9D4C7"
    pawnD
      | white = "M22.5 9c-2.21 0-4 1.79-4 4 0 .89.29 1.71.78 2.38C17.33 16.5 16 18.59 16 21c0 2.03.94 3.84 2.41 5.03-3 1.06-7.41 5.55-7.41 13.47h23c0-7.92-4.41-12.41-7.41-13.47 1.47-1.19 2.41-3 2.41-5.03 0-2.41-1.33-4.5-3.28-5.62.49-.67.78-1.49.78-2.38 0-2.21-1.79-4-4-4z"
      | otherwise = "M22.5 9a4 4 0 0 0-3.22 6.38 6.48 6.48 0 0 0-.87 10.65c-3 1.06-7.41 5.55-7.41 13.47h23c0-7.92-4.41-12.41-7.41-13.47a6.46 6.46 0 0 0-.87-10.65A4.01 4.01 0 0 0 22.5 9z"
