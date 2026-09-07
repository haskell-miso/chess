-----------------------------------------------------------------------------
-- | Sound effects synthesized with the Web Audio API; no audio assets.
-- Pieces are wood: a move is a short knock (a filtered noise burst over a
-- low thump), a capture lands heavier, castling knocks twice.  Check is a
-- small bell, promotion a rising sparkle, and the results are three-note
-- figures.  'soundInit' must run inside a user gesture (the Play button)
-- so the AudioContext is allowed to start.
-----------------------------------------------------------------------------
module Sound
  ( soundInit
  , playSound
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (when)
-----------------------------------------------------------------------------
import           Miso.FFI.QQ (js)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
soundInit :: IO ()
soundInit = [js|
  if (!globalThis.__mch) {
    var AC = window.AudioContext || window.webkitAudioContext;
    var ctx = new AC();
    var master = ctx.createGain();
    master.gain.value = 0.5;
    master.connect(ctx.destination);
    function noiseBuf(dur) {
      var n = Math.floor(ctx.sampleRate * dur);
      var b = ctx.createBuffer(1, n, ctx.sampleRate);
      var d = b.getChannelData(0);
      for (var i = 0; i < n; i++) d[i] = Math.random() * 2 - 1;
      return b;
    }
    function env(g, t0, a, peak, d) {
      g.gain.setValueAtTime(0, t0);
      g.gain.linearRampToValueAtTime(peak, t0 + a);
      g.gain.exponentialRampToValueAtTime(0.0001, t0 + a + d);
    }
    function tone(freq, type, t0, a, peak, d, glide) {
      var o = ctx.createOscillator();
      o.type = type;
      o.frequency.setValueAtTime(freq, t0);
      if (glide) o.frequency.exponentialRampToValueAtTime(glide, t0 + a + d);
      var g = ctx.createGain();
      env(g, t0, a, peak, d);
      o.connect(g);
      g.connect(master);
      o.start(t0);
      o.stop(t0 + a + d + 0.05);
    }
    function knock(t0, peak, low, bright) {
      // wood on stone: a bandpassed tick plus a short low thump
      var s = ctx.createBufferSource();
      s.buffer = noiseBuf(0.06);
      var f = ctx.createBiquadFilter();
      f.type = 'bandpass';
      f.frequency.setValueAtTime(bright, t0);
      f.frequency.exponentialRampToValueAtTime(bright * 0.4, t0 + 0.05);
      f.Q.value = 1.6;
      var g = ctx.createGain();
      env(g, t0, 0.003, peak, 0.05);
      s.connect(f); f.connect(g); g.connect(master);
      s.start(t0);
      tone(low, 'sine', t0, 0.002, peak * 0.9, 0.09, low * 0.55);
    }
    function bell(freq, t0, peak, d) {
      tone(freq, 'sine', t0, 0.004, peak, d);
      tone(freq * 2.76, 'sine', t0, 0.004, peak * 0.35, d * 0.6);
      tone(freq * 5.4, 'sine', t0, 0.004, peak * 0.12, d * 0.3);
    }
    globalThis.__mch = {
      play: function (name) {
        if (ctx.state === 'suspended') ctx.resume();
        var t = ctx.currentTime + 0.01;
        var i;
        if (name === 'move') {
          knock(t, 0.55, 180, 2400);
        } else if (name === 'capture') {
          knock(t, 0.7, 120, 1500);
          knock(t + 0.05, 0.35, 95, 900);
        } else if (name === 'castle') {
          knock(t, 0.5, 180, 2400);
          knock(t + 0.13, 0.45, 160, 2100);
        } else if (name === 'select') {
          tone(1500, 'sine', t, 0.001, 0.08, 0.03);
        } else if (name === 'check') {
          knock(t, 0.5, 170, 2200);
          bell(1318, t + 0.06, 0.28, 0.5);
        } else if (name === 'promote') {
          var ns = [784, 988, 1319];
          for (i = 0; i < ns.length; i++) bell(ns[i], t + i * 0.09, 0.22, 0.45);
        } else if (name === 'illegal') {
          tone(140, 'triangle', t, 0.005, 0.18, 0.09, 110);
        } else if (name === 'undo') {
          tone(520, 'sine', t, 0.004, 0.12, 0.08, 380);
        } else if (name === 'win') {
          var w = [523.25, 659.25, 783.99, 1046.5];
          for (i = 0; i < w.length; i++) bell(w[i], t + i * 0.13, 0.3, 0.9);
        } else if (name === 'lose') {
          var l = [392, 311.1, 261.6];
          for (i = 0; i < l.length; i++) tone(l[i], 'triangle', t + i * 0.22, 0.01, 0.28, 0.7);
        } else if (name === 'draw') {
          bell(440, t, 0.25, 0.6);
          bell(440, t + 0.3, 0.2, 0.8);
        } else if (name === 'flip') {
          knock(t, 0.3, 140, 1200);
        } else if (name === 'click') {
          tone(900, 'sine', t, 0.001, 0.1, 0.04);
        }
      }
    };
  }
|]
-----------------------------------------------------------------------------
playSound :: Bool -> MisoString -> IO ()
playSound enabled name = when enabled
  [js| if (globalThis.__mch) { globalThis.__mch.play(${name}); } |]
