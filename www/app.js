// ── Page navigation ───────────────────────────────────────────────────────────
function lmsNavigate(tab) {
  document.querySelectorAll('.lms-page').forEach(function(el) {
    el.classList.add('d-none');
  });
  var target = document.getElementById('page_' + tab);
  if (target) target.classList.remove('d-none');

  document.querySelectorAll('.lms-nav-item').forEach(function(el) {
    el.classList.remove('active');
  });
  var navEl = document.getElementById('nav_' + tab);
  if (navEl) navEl.classList.add('active');

  if (window.Shiny) {
    Shiny.setInputValue('active_tab', tab, { priority: 'event' });
  }
}

// ── Per-canvas viewport state: { scale, tx, ty } ─────────────────────────────
var _vp = {};

function _vpGet(id) {
  if (!_vp[id]) _vp[id] = { scale: 1, tx: 0, ty: 0 };
  return _vp[id];
}

// Apply transform to image AND marker SVG inside a canvas
function _vpApplyTransform(canvas, tx, ty, scale, animate) {
  var t  = 'translate(' + tx + 'px,' + ty + 'px) scale(' + scale + ')';
  var tr = animate ? 'transform 0.15s ease' : 'none';
  var img = canvas.querySelector('img');
  if (img) {
    img.style.transformOrigin = 'center center';
    img.style.transition      = tr;
    img.style.transform       = t;
  }
  var svg = canvas.querySelector('.wsi-marker-svg');
  if (svg) {
    svg.style.transformOrigin = 'center center';
    svg.style.transition      = tr;
    svg.style.transform       = t;
  }
}

function _vpApply(id, animate) {
  var el = document.getElementById(id);
  if (!el) return;
  var s = _vpGet(id);
  _vpApplyTransform(el, s.tx, s.ty, s.scale, animate);
}

function lmsZoomIn(canvasId) {
  var s = _vpGet(canvasId);
  s.scale = Math.min(s.scale * 1.25, 8);
  _vpApply(canvasId, true);
}

function lmsZoomOut(canvasId) {
  var s = _vpGet(canvasId);
  s.scale = Math.max(s.scale / 1.25, 0.5);
  if (s.scale < 1.05) { s.tx = 0; s.ty = 0; }
  _vpApply(canvasId, true);
}

function lmsZoomReset(canvasId) {
  _vp[canvasId] = { scale: 1, tx: 0, ty: 0 };
  _vpApply(canvasId, true);
}

// ── Drag-to-pan ───────────────────────────────────────────────────────────────
function _initDragPan(canvasId) {
  var el = document.getElementById(canvasId);
  if (!el || el._dragInit) return;
  el._dragInit = true;

  var drag = { on: false, x0: 0, y0: 0, tx0: 0, ty0: 0 };

  el.addEventListener('mousedown', function(e) {
    if (e.button !== 0) return;
    var s = _vpGet(canvasId);
    drag.on  = true;
    drag.x0  = e.clientX;
    drag.y0  = e.clientY;
    drag.tx0 = s.tx;
    drag.ty0 = s.ty;
    el.style.cursor = 'grabbing';
    e.preventDefault();
  });

  document.addEventListener('mousemove', function(e) {
    if (!drag.on) return;
    var canvas = document.getElementById(canvasId);
    if (!canvas) { drag.on = false; return; }
    var s = _vpGet(canvasId);
    s.tx = drag.tx0 + (e.clientX - drag.x0);
    s.ty = drag.ty0 + (e.clientY - drag.y0);
    _vpApplyTransform(canvas, s.tx, s.ty, s.scale, false);
  });

  document.addEventListener('mouseup', function() {
    if (!drag.on) return;
    drag.on = false;
    var canvas = document.getElementById(canvasId);
    if (canvas) canvas.style.cursor = 'grab';
  });
}

// ── Scroll right strip to a specific patch card ───────────────────────────────
function lmsScrollToPatch(patchId) {
  var strip = document.querySelector('.inspect-right-strip > div:last-child');
  if (!strip) return;
  var card = strip.querySelector('[data-patch-id="' + patchId + '"]');
  if (!card) return;
  card.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
}

function _tryInitAll() {
  _initDragPan('wsi_canvas');
  _initDragPan('compare_canvas_a');
  _initDragPan('compare_canvas_b');
}

$(document).on('shiny:sessioninitialized', _tryInitAll);
$(document).on('shiny:value', _tryInitAll);
