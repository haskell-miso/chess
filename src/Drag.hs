-----------------------------------------------------------------------------
-- | Pointer-based drag for the pieces.  Squares own every click; pieces
-- are drawn above them with pointer events off, so a drag begins on a
-- square marked @own@, lifts a clone of the piece under the pointer, and
-- on release replays the tap flow (select the source, tap the target)
-- so the rules stay in the update function.  The synthetic click that
-- follows a real drag is swallowed at the document so it cannot undo the
-- selection.
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
    var src = null, piece = null, ghost = null, over = null;
    var sx = 0, sy = 0, active = false, moved = false, swallow = false;
    function squareAt(x, y) {
      var els = document.elementsFromPoint(x, y);
      for (var i = 0; i < els.length; i++) {
        if (els[i].classList && els[i].classList.contains('sq')) return els[i];
      }
      return null;
    }
    function clearOver() {
      if (over) { over.classList.remove('drop'); over = null; }
    }
    function cleanup() {
      if (ghost) { ghost.remove(); ghost = null; }
      if (piece) { piece.classList.remove('lifted'); }
      clearOver();
      active = false; moved = false; src = null; piece = null;
    }
    document.addEventListener('pointerdown', function (e) {
      if (e.pointerType === 'mouse' && e.button !== 0) return;
      var sq = e.target && e.target.closest ? e.target.closest('.sq') : null;
      if (!sq || !sq.classList.contains('own')) return;
      var name = sq.getAttribute('data-sq');
      var pc = document.querySelector('.pc[data-sq="' + name + '"]');
      if (!pc) return;
      src = sq; piece = pc; sx = e.clientX; sy = e.clientY;
      active = true; moved = false;
    });
    document.addEventListener('pointermove', function (e) {
      if (!active) return;
      if (!moved) {
        if (Math.hypot(e.clientX - sx, e.clientY - sy) < 6) return;
        moved = true;
        var size = src.getBoundingClientRect().width;
        ghost = piece.cloneNode(true);
        ghost.className = 'pc ghost';
        ghost.style.width = size + 'px';
        ghost.style.height = size + 'px';
        document.body.appendChild(ghost);
        piece.classList.add('lifted');
        if (!src.classList.contains('sel')) src.click();
      }
      if (ghost) {
        ghost.style.left = e.clientX + 'px';
        ghost.style.top = e.clientY + 'px';
      }
      var sq = squareAt(e.clientX, e.clientY);
      if (over !== sq) {
        clearOver();
        if (sq && sq !== src) { over = sq; sq.classList.add('drop'); }
      }
      if (e.cancelable) e.preventDefault();
    }, { passive: false });
    document.addEventListener('pointerup', function (e) {
      if (!active) return;
      if (!moved) { active = false; src = null; piece = null; return; }
      var target = squareAt(e.clientX, e.clientY);
      var from = src;
      cleanup();
      if (target && target !== from) target.click();
      swallow = true;
      setTimeout(function () { swallow = false; }, 0);
    });
    document.addEventListener('pointercancel', cleanup);
    document.addEventListener('click', function (e) {
      if (swallow) { e.stopPropagation(); e.preventDefault(); swallow = false; }
    }, true);
  }
|]
