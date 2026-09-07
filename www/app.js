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

// ── Minimap viewport-rect updater ────────────────────────────────────────────
function lmsUpdateMinimap(canvasId) {
  var minimap = document.querySelector('#' + canvasId + ' .minimap');
  if (!minimap) return;
  var rect = minimap.querySelector('.viewport-rect');
  if (!rect) return;
  var canvas = document.getElementById(canvasId);
  if (!canvas) return;
  var img = canvas.querySelector('img.wsi-image');
  if (!img || !img.naturalWidth) { rect.style.display = 'none'; return; }

  var cW = canvas.clientWidth,  cH = canvas.clientHeight;
  var iW = img.naturalWidth,    iH = img.naturalHeight;
  var s  = _vpGet(canvasId);

  // Letter-box fit at scale 1
  var fit = Math.min(cW / iW, cH / iH);
  var dW  = iW * fit,  dH = iH * fit;

  // Image edges on canvas at current zoom
  var imgL = (cW - dW * s.scale) / 2 + s.tx;
  var imgT = (cH - dH * s.scale) / 2 + s.ty;

  // Fraction of image visible (clamped 0–1)
  var fxL = Math.max(0, -imgL / (dW * s.scale));
  var fxR = Math.min(1, (cW - imgL) / (dW * s.scale));
  var fyT = Math.max(0, -imgT / (dH * s.scale));
  var fyB = Math.min(1, (cH - imgT) / (dH * s.scale));

  // Minimap rendered size (the <img> inside fills 100% width)
  var mmImg = minimap.querySelector('img');
  if (!mmImg || !mmImg.clientWidth) { rect.style.display = 'none'; return; }
  var mmW = mmImg.clientWidth, mmH = mmImg.clientHeight;

  var rW = (fxR - fxL) * mmW;
  var rH = (fyB - fyT) * mmH;

  // Hide when fully zoomed out (no useful rect)
  if (s.scale < 1.05) { rect.style.display = 'none'; return; }

  rect.style.display = 'block';
  rect.style.left    = (fxL * mmW) + 'px';
  rect.style.top     = (fyT * mmH) + 'px';
  rect.style.width   = Math.max(4, rW) + 'px';
  rect.style.height  = Math.max(4, rH) + 'px';
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
  lmsUpdateMinimap(id);
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

// Zoom (and pan) so that the SVG point (svgX, svgY) is centered in the canvas.
// svgW × svgH are the SVG viewBox dimensions (= slide extent).
// If already zoomed ≥ 2×, only pans; otherwise zooms to 3× first.
function lmsZoomToSvgPoint(canvasId, svgX, svgY, svgW, svgH) {
  var canvas = document.getElementById(canvasId);
  if (!canvas) return;
  var img = canvas.querySelector('img');
  if (!img || !img.naturalWidth) return;

  var cW  = canvas.clientWidth;
  var cH  = canvas.clientHeight;
  var iW  = img.naturalWidth;
  var iH  = img.naturalHeight;

  // How the image is letter-boxed into the canvas at scale 1
  var fit = Math.min(cW / iW, cH / iH);
  var dW  = iW * fit;
  var dH  = iH * fit;

  // Canvas-space pixel that corresponds to the SVG point
  var canvasX = (cW - dW) / 2 + svgX * (dW / svgW);
  var canvasY = (cH - dH) / 2 + svgY * (dH / svgH);

  // If not already zoomed in, jump to 3×; otherwise keep current zoom
  var s = _vpGet(canvasId);
  var newScale = (s.scale < 2) ? 3 : s.scale;

  // Center the canvas point on screen (transform-origin is canvas center)
  s.scale = newScale;
  s.tx    = -(canvasX - cW / 2) * newScale;
  s.ty    = -(canvasY - cH / 2) * newScale;
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
    lmsUpdateMinimap(canvasId);
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

// Re-apply stored viewport transforms to any newly-rendered SVG/img elements
function _reapplyViewports() {
  ['wsi_canvas', 'compare_canvas_a', 'compare_canvas_b'].forEach(function(id) {
    _vpApply(id, false);
  });
}

// Watch the wsi_marker_svg output wrapper for DOM changes and immediately
// re-apply the viewport transform so zoomed/panned state is preserved
// after Shiny re-renders the SVG (e.g. on highlighted patch change).
function _initMarkerObserver() {
  var outputEl = document.getElementById('wsi_marker_svg');
  if (!outputEl || outputEl._svgMO) return;
  outputEl._svgMO = new MutationObserver(function() {
    _vpApply('wsi_canvas', false);
  });
  outputEl._svgMO.observe(outputEl, { childList: true, subtree: true });
}

// ── Analysis Pipeline step explanations ─────────────────────────────────────
var _pipelineExplanations = [
  {
    title: "H&E Whole-Slide Images",
    input: "Glass tissue slides stained with hematoxylin and eosin (H&E), then scanned into high-resolution digital images.",
    output: "A digital whole-slide image (WSI) — a single file capturing the entire tissue sample at microscopic resolution, ready for computational analysis.",
    note: "These are the same slides a pathologist would review under a microscope. Digitizing them allows a computer to systematically examine regions that would take a human hours to assess."
  },
  {
    title: "Patch Extraction",
    input: "A whole-slide image, which can exceed 50,000 × 50,000 pixels — far too large to process all at once.",
    output: "Thousands of small 256 × 256-pixel image tiles (\"patches\"), each capturing a distinct region of the tumor tissue at 20× magnification.",
    note: "Tiling makes the image computationally manageable. Each tile covers roughly a 130 × 130 µm area of tissue — about the size of a few dozen cells."
  },
  {
    title: "Feature Extraction (UNI2-h)",
    input: "Each individual patch image produced in the previous step.",
    output: "A compact numerical \"fingerprint\" — 1,536 numbers — summarizing what each patch looks like: cell density, texture, structural patterns, and other visual characteristics.",
    note: "This step uses UNI2-h, a foundation model pre-trained on millions of pathology images from many cancer types. It translates raw pixels into a rich numerical description without needing explicit instructions about what to look for."
  },
  {
    title: "ABMIL Training (CLAM)",
    input: "The numerical fingerprints for all patches in a slide, together with each patient's clinical outcome (Favorable or Adverse).",
    output: "A trained model that has learned which patch characteristics are most associated with outcome — plus an attention score for every patch reflecting its contribution to the prediction.",
    note: "Training was repeated 5 times with different random initializations to assess how consistently the model identifies the same important regions across runs."
  },
  {
    title: "Attention Maps",
    input: "The trained model applied to a slide's patch fingerprints.",
    output: "A score for every patch reflecting how strongly the model relied on that region. High-attention areas appear as bright spots on the heatmap overlay.",
    note: "Attention maps make the model's reasoning visible. High-attention regions are not necessarily the most abnormal tissue — they are the areas the model found most informative for distinguishing Favorable from Adverse outcomes."
  }
];

var _activePipelineStep = null;

function lmsPipelineClick(idx) {
  var panel = document.getElementById('pipeline-detail-panel');
  if (!panel) return;

  if (_activePipelineStep === idx) {
    panel.hidden = true;
    _activePipelineStep = null;
    document.querySelectorAll('.pipeline-step').forEach(function(el) {
      el.classList.remove('pipeline-step-active');
    });
    return;
  }

  _activePipelineStep = idx;
  var exp = _pipelineExplanations[idx];
  panel.innerHTML =
    '<div class="pipeline-detail-inner">' +
      '<div class="pipeline-detail-title">' + exp.title + '</div>' +
      '<div class="pipeline-detail-row">' +
        '<span class="pipeline-detail-badge pipeline-detail-badge-in">Input</span>' +
        '<span class="pipeline-detail-text">' + exp.input + '</span>' +
      '</div>' +
      '<div class="pipeline-detail-row">' +
        '<span class="pipeline-detail-badge pipeline-detail-badge-out">Output</span>' +
        '<span class="pipeline-detail-text">' + exp.output + '</span>' +
      '</div>' +
      '<div class="pipeline-detail-note">' + exp.note + '</div>' +
    '</div>';
  panel.hidden = false;

  document.querySelectorAll('.pipeline-step').forEach(function(el, i) {
    el.classList.toggle('pipeline-step-active', i === idx);
  });
}

// ── Zoom to SVG point after image loads (retries until ready) ────────────────
function lmsZoomAfterLoad(canvasId, svgX, svgY, svgW, svgH) {
  var attempts = 0;
  function tryZoom() {
    var canvas = document.getElementById(canvasId);
    if (!canvas) { if (++attempts < 25) setTimeout(tryZoom, 150); return; }
    var img = canvas.querySelector('img');
    if (!img || !img.naturalWidth) { if (++attempts < 25) setTimeout(tryZoom, 150); return; }
    lmsZoomToSvgPoint(canvasId, svgX, svgY, svgW, svgH);
  }
  setTimeout(tryZoom, 100);
}

// ── Gallery patch strip arrow scroll ─────────────────────────────────────────
function lmsGalleryStripScroll(btn, dir) {
  var wrapper = btn.closest('.gallery-strip-wrapper');
  if (!wrapper) return;
  var strip = wrapper.querySelector('.gallery-slide-row-patches');
  if (!strip) return;
  strip.scrollBy({ left: dir * 280, behavior: 'smooth' });
}

// ── Gallery mode / K toggles ─────────────────────────────────────────────────
function lmsGalleryMode(mode) {
  document.querySelectorAll('.gallery-mode-btn').forEach(function(btn) {
    btn.classList.toggle('mode-btn-active', btn.dataset.mode === mode);
  });
  var kSel  = document.getElementById('gallery-k-selector');
  var patch = mode === 'topk' || mode === 'recurring';
  if (kSel) kSel.style.display = patch ? 'flex' : 'none';
  if (window.Shiny) Shiny.setInputValue('gallery_mode', mode, {priority: 'event'});
}

function lmsGalleryK(k) {
  document.querySelectorAll('.gallery-k-btn').forEach(function(btn) {
    btn.classList.toggle('mode-btn-active', parseInt(btn.dataset.k) === k);
  });
  if (window.Shiny) Shiny.setInputValue('gallery_k', k, {priority: 'event'});
}

// ── Brand logo: patch grid globe ─────────────────────────────────────────────
(function () {
  function viridis(t) {
    var stops = [
      [68,1,84],[72,34,116],[64,67,135],[52,94,141],
      [41,120,142],[32,144,140],[34,167,132],[68,190,112],
      [121,209,81],[189,223,38],[253,231,37]
    ];
    var v = Math.max(0, Math.min(0.9999, t));
    var i = Math.floor(v * 10);
    var f = v * 10 - i;
    var a = stops[i], b = stops[i + 1];
    return 'rgb('+Math.round(a[0]+f*(b[0]-a[0]))+','+
                  Math.round(a[1]+f*(b[1]-a[1]))+','+
                  Math.round(a[2]+f*(b[2]-a[2]))+')';
  }

  function buildPatchGlobe(el) {
    if (!el) return;
    var ns = 'http://www.w3.org/2000/svg';
    var defs = document.createElementNS(ns, 'defs');
    var cp   = document.createElementNS(ns, 'clipPath');
    cp.id    = 'cp-brand-globe';
    var cc   = document.createElementNS(ns, 'circle');
    cc.setAttribute('cx','50'); cc.setAttribute('cy','50'); cc.setAttribute('r','47');
    cp.appendChild(cc); defs.appendChild(cp); el.appendChild(defs);

    var g = document.createElementNS(ns, 'g');
    g.setAttribute('clip-path', 'url(#cp-brand-globe)');
    var bg = document.createElementNS(ns, 'circle');
    bg.setAttribute('cx','50'); bg.setAttribute('cy','50'); bg.setAttribute('r','47');
    bg.setAttribute('fill','#ffffff');
    g.appendChild(bg);

    var pSz=5.6, gap=1.4, step=7.0, start=2;
    var count = Math.ceil(96 / step);
    var R=47, hx=0.707, hy=-0.707;

    for (var row=0; row<count; row++) {
      for (var col=0; col<count; col++) {
        var x=start+col*step, y=start+row*step;
        var nx=(x+2.8-50)/R,  ny=(y+2.8-50)/R;
        var dx=nx-hx, dy=ny-hy;
        var t=Math.max(0, Math.min(1, 1-Math.sqrt(dx*dx+dy*dy)/2));
        var rect=document.createElementNS(ns,'rect');
        rect.setAttribute('x',String(x)); rect.setAttribute('y',String(y));
        rect.setAttribute('width',String(pSz)); rect.setAttribute('height',String(pSz));
        rect.setAttribute('rx','0.7'); rect.setAttribute('fill',viridis(t));
        g.appendChild(rect);
      }
    }
    el.appendChild(g);

    var ring=document.createElementNS(ns,'circle');
    ring.setAttribute('cx','50'); ring.setAttribute('cy','50'); ring.setAttribute('r','47');
    ring.setAttribute('fill','none');
    ring.setAttribute('stroke','rgba(255,255,255,0.55)');
    ring.setAttribute('stroke-width','1.8');
    el.appendChild(ring);
  }

  document.addEventListener('DOMContentLoaded', function() {
    buildPatchGlobe(document.getElementById('lms-brand-globe'));
  });
})();

// ── Gallery slide hover preview ───────────────────────────────────────────────
(function () {
  var _popup  = null;
  var POPUP_W = 340;

  function getPopup() {
    if (_popup) return _popup;
    _popup = document.createElement('div');
    _popup.id = 'slide-hover-preview';
    var img = document.createElement('img');
    img.style.cssText = 'display:block; width:100%; max-height:300px; object-fit:contain;';
    _popup.appendChild(img);
    document.body.appendChild(_popup);
    return _popup;
  }

  document.addEventListener('mouseover', function (e) {
    var card = e.target.closest && e.target.closest('.slide-card');
    if (!card) return;
    var thumb = card.querySelector('img[data-preview-src]');
    if (!thumb) return;

    var p   = getPopup();
    var img = p.querySelector('img');
    var src = thumb.getAttribute('data-preview-src');
    if (img.src !== src) img.src = src;

    var rect = card.getBoundingClientRect();
    var left = rect.right + 10;
    if (left + POPUP_W > window.innerWidth - 8) left = rect.left - POPUP_W - 10;
    if (left < 8) left = 8;
    var top  = Math.max(8, Math.min(rect.top, window.innerHeight - 320));

    p.style.left    = left + 'px';
    p.style.top     = top  + 'px';
    p.style.display = 'block';
  });

  document.addEventListener('mouseout', function (e) {
    var card = e.target.closest && e.target.closest('.slide-card');
    if (!card) return;
    if (e.relatedTarget && card.contains(e.relatedTarget)) return;
    var p = document.getElementById('slide-hover-preview');
    if (p) p.style.display = 'none';
  });
})();

$(document).on('shiny:sessioninitialized', _tryInitAll);
$(document).on('shiny:value', function() {
  _tryInitAll();
  _initMarkerObserver();
  setTimeout(_reapplyViewports, 0);
});
