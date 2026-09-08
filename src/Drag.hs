-----------------------------------------------------------------------------
-- | The pointer's own position, published to CSS.
--
-- Dragging itself lives in the model: 'Model.Drag' says which piece is up
-- and which square it is over, the squares report presses and releases
-- through ordinary pointer handlers, and the view draws the lifted piece.
-- The one thing a redraw per pointer event would be too slow for is
-- keeping that piece under the cursor, so this shim writes the pointer
-- straight to three custom properties on the root element and lets CSS
-- place the piece:
--
--   * @--dgx@, @--dgy@ — the pointer, in viewport pixels
--   * @--dgs@ — the side of a board square, measured when a press begins
--
-- It also hands back the implicit pointer capture a touch or pen press
-- takes on its target, so that a finger dragging across the board keeps
-- entering and releasing on the square it is actually over.
-----------------------------------------------------------------------------
module Drag
  ( dragInit
  ) where
-----------------------------------------------------------------------------
import           Miso.FFI.QQ (js)
-----------------------------------------------------------------------------
dragInit :: IO ()
dragInit = [js|
  if (!globalThis.__cdrag) {
    globalThis.__cdrag = true;
    var root = document.documentElement, held = false;
    function track(e) {
      root.style.setProperty('--dgx', e.clientX + 'px');
      root.style.setProperty('--dgy', e.clientY + 'px');
    }
    document.addEventListener('pointerdown', function (e) {
      var target = e.target;
      if (target && target.releasePointerCapture) {
        try { target.releasePointerCapture(e.pointerId); } catch (err) {}
      }
      var board = document.querySelector('.board');
      if (board) {
        var side = board.getBoundingClientRect().width / 8;
        root.style.setProperty('--dgs', side + 'px');
      }
      held = true;
      track(e);
    }, true);
    document.addEventListener('pointermove', function (e) {
      if (held) track(e);
    }, true);
    document.addEventListener('pointerup', function () { held = false; }, true);
    document.addEventListener('pointercancel', function () { held = false; }, true);
  }
|]
