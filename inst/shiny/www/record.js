// The recorder. Talks to the meters over Web Bluetooth, stamps every sample
// with this page's clock, and hands batches to R, which owns the files.
//
// Two things here are easy to undo by accident and break the recording.
//
// First, requestDevice() may only be called from a user gesture, so the Pair
// button is handled in this file rather than by an observer in R. Routing the
// click through the websocket and back would arrive without a gesture and the
// chooser would never open.
//
// Second, one clock stamps every meter. The stamp is taken here, on arrival,
// and nothing the meter reports about time is used, so the offsets are zero by
// construction rather than recovered by cross-correlation afterwards.

(function () {
  'use strict';

  var POWER_SERVICE = 'cycling_power';
  var POWER_MEASUREMENT = 'cycling_power_measurement';

  // Samples are batched rather than sent one at a time. Shiny collapses repeat
  // sets of the same input name within a tick, so a meter notifying at 4 Hz
  // would silently lose samples if each were its own setInputValue. The cost is
  // that killing the tab loses at most this window; R flushes every batch it
  // receives straight to disk, so nothing else is held in memory.
  var FLUSH_MS = 500;

  // The trace is drawn from this window only. The point on screen is that the
  // meters are tracking each other now, not the shape of the whole ride, and
  // the whole ride is on disk anyway.
  var TRACE_SECONDS = 180;

  // Okabe-Ito, matching source_palette() in R, so a meter keeps its colour from
  // the recording screen into the charts.
  var PALETTE = ['#0072B2', '#D55E00', '#009E73', '#CC79A7', '#E69F00'];

  var meters = [];
  // Never reused, so unpairing the second of three meters cannot hand the next
  // one a slot number that still has an input box and an observer attached to
  // it in R.
  var nextSlot = 1;
  var recording = false;
  var pending = [];
  var batch = 0;
  var wakeLock = null;

  function send(name, value) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(name, value, { priority: 'event' });
    }
  }

  function findMeter(slot) {
    for (var i = 0; i < meters.length; i++) {
      if (meters[i].slot === slot) return meters[i];
    }
    return null;
  }

  // ---- Connection --------------------------------------------------------

  function setState(meter, state) {
    meter.state = state;
    send('meter_state', { slot: meter.slot, state: state });
    drawStatus();
  }

  async function pair() {
    if (!navigator.bluetooth) return;

    var device;
    try {
      device = await navigator.bluetooth.requestDevice({
        filters: [{ services: [POWER_SERVICE] }]
      });
    } catch (err) {
      // Dismissing the chooser throws, and is not a failure to report.
      return;
    }

    for (var i = 0; i < meters.length; i++) {
      if (meters[i].device.id === device.id) return;
    }

    var meter = {
      slot: nextSlot++,
      device: device,
      name: device.name || 'Unnamed meter',
      label: null,
      colour: PALETTE[(nextSlot - 2) % PALETTE.length],
      state: 'connecting',
      power: null,
      trace: [],
      dropped: false
    };
    meters.push(meter);

    device.addEventListener('gattserverdisconnected', function () {
      reconnect(meter);
    });

    send('meter_paired', {
      slot: meter.slot,
      name: meter.name,
      colour: meter.colour
    });

    try {
      await connect(meter);
    } catch (err) {
      setState(meter, 'failed');
    }
  }

  async function connect(meter) {
    setState(meter, 'connecting');
    var server = await meter.device.gatt.connect();
    var service = await server.getPrimaryService(POWER_SERVICE);
    var characteristic = await service.getCharacteristic(POWER_MEASUREMENT);

    // A fresh characteristic object comes back from every connect, so the
    // listener is new each time and there is nothing to detach.
    characteristic.addEventListener('characteristicvaluechanged', function (e) {
      onSample(meter, e.target.value);
    });
    await characteristic.startNotifications();
    setState(meter, 'connected');
  }

  // BLE connections drop, and a drop mid-ride must be visible rather than
  // quietly leaving a flat file. A power meter that has gone to sleep can take
  // minutes of pedalling to advertise again, so retries back off to five
  // seconds and then carry on for as long as the page is open. After twelve
  // failures the tile reads lost, and flips back to live the moment the meter
  // answers. The file has a gap over the dropout, which is what align_rides()
  // and paired_seconds() expect.
  async function reconnect(meter) {
    if (meter.dropped || meter.retrying) return;
    meter.retrying = true;
    setState(meter, 'reconnecting');

    for (var attempt = 1; !meter.dropped; attempt++) {
      await wait(Math.min(1000 * attempt, 5000));
      if (meter.dropped) break;
      try {
        await connect(meter);
        break;
      } catch (err) {
        if (attempt >= 12) setState(meter, 'lost');
      }
    }
    meter.retrying = false;
  }

  function wait(ms) {
    return new Promise(function (resolve) {
      setTimeout(resolve, ms);
    });
  }

  // ---- Samples -----------------------------------------------------------

  function onSample(meter, view) {
    var parsed = parseCyclingPower(view);
    var now = Date.now();

    meter.power = parsed.power;
    meter.lastAt = now;
    meter.trace.push({ at: now, power: parsed.power });
    var cutoff = now - TRACE_SECONDS * 1000;
    while (meter.trace.length > 0 && meter.trace[0].at < cutoff) {
      meter.trace.shift();
    }

    if (recording && meter.label) {
      pending.push({
        source: meter.label,
        time: new Date(now).toISOString(),
        power: parsed.power
      });
    }
  }

  function flush() {
    if (pending.length === 0) return;
    batch += 1;
    send('samples', { batch: batch, rows: pending });
    pending = [];
  }

  setInterval(function () {
    if (recording) flush();
  }, FLUSH_MS);

  // ---- Wake lock ---------------------------------------------------------

  // Chrome throttles background tabs and stalls notifications on a sleeping
  // display, which would show up as a gap in every file at once.
  async function holdScreen() {
    if (!navigator.wakeLock) return;
    try {
      wakeLock = await navigator.wakeLock.request('screen');
    } catch (err) {
      wakeLock = null;
    }
  }

  function releaseScreen() {
    if (wakeLock) {
      wakeLock.release();
      wakeLock = null;
    }
  }

  // The lock is lost whenever the tab is hidden, and is not restored on its
  // own when it comes back.
  document.addEventListener('visibilitychange', function () {
    if (recording && document.visibilityState === 'visible') holdScreen();
  });

  // ---- Drawing -----------------------------------------------------------

  var STATE_LABELS = {
    connecting: 'Connecting…',
    connected: 'Live',
    reconnecting: 'Reconnecting…',
    lost: 'Lost',
    failed: 'Failed'
  };

  // Elapsed time on the recording's own clock, shown beside the controls.
  var startedAt = null;
  var elapsedMs = 0;

  // Freezes the clock at the final time, so the length of the ride is still on
  // screen after Stop.
  function stopClock() {
    if (startedAt !== null) elapsedMs = Date.now() - startedAt;
    startedAt = null;
  }

  function drawClock() {
    var el = document.getElementById('record_clock');
    if (!el) return;
    var ms = startedAt === null ? elapsedMs : Date.now() - startedAt;
    var total = Math.floor(ms / 1000);
    var m = Math.floor(total / 60);
    var s = total % 60;
    el.textContent = m + ':' + (s < 10 ? '0' : '') + s;
  }

  // The tiles are built when a state changes, so the watts are refreshed in
  // place here. A meter that has said nothing for three seconds reads as a dash
  // rather than as its last value.
  function drawWatts() {
    var now = Date.now();
    for (var i = 0; i < meters.length; i++) {
      var meter = meters[i];
      if (!meter.wattsEl) continue;
      var live = meter.power !== null && meter.lastAt && now - meter.lastAt < 3000;
      meter.wattsEl.textContent = live ? meter.power + ' W' : '—';
    }
  }

  setInterval(function () {
    drawClock();
    drawWatts();
  }, 250);

  function drawStatus() {
    var container = document.getElementById('meter_status');
    if (!container) return;

    container.innerHTML = '';
    for (var i = 0; i < meters.length; i++) {
      var meter = meters[i];
      var chip = document.createElement('div');
      chip.className = 'meter-chip meter-' + meter.state;

      chip.style.setProperty('--meter-colour', meter.colour);

      var name = document.createElement('span');
      name.className = 'meter-name';
      name.textContent = meter.label || meter.name;
      chip.appendChild(name);

      var watts = document.createElement('span');
      watts.className = 'meter-watts';
      watts.textContent = meter.power === null ? '—' : meter.power + ' W';
      meter.wattsEl = watts;
      chip.appendChild(watts);

      var state = document.createElement('span');
      state.className = 'meter-state';
      state.textContent = STATE_LABELS[meter.state] || meter.state;
      chip.appendChild(state);

      container.appendChild(chip);
    }
  }

  // Client-side and on a canvas, because the R process has the analysis tabs to
  // run and a trace that stutters while you are riding is worse than none.
  function drawTrace() {
    paintTrace();
    requestAnimationFrame(drawTrace);
  }

  var AXIS_WIDTH = 42;
  var HEAD_ROOM = 14;

  function paintTrace() {
    var canvas = document.getElementById('record_trace');
    if (!canvas || canvas.offsetParent === null) return;

    var width = canvas.clientWidth;
    var height = canvas.clientHeight;
    if (width === 0 || height === 0) return;

    var ratio = window.devicePixelRatio || 1;
    if (canvas.width !== width * ratio || canvas.height !== height * ratio) {
      canvas.width = width * ratio;
      canvas.height = height * ratio;
    }

    var ctx = canvas.getContext('2d');
    ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    ctx.clearRect(0, 0, width, height);

    var plot = height - HEAD_ROOM;
    var peak = Math.ceil((tracePeak() * 1.1) / 50) * 50;

    ctx.strokeStyle = '#e7e5e4';
    ctx.lineWidth = 1;
    ctx.fillStyle = '#57534e';
    ctx.font = '14px system-ui, sans-serif';
    for (var g = 0; g <= 4; g++) {
      var y = height - plot * (g / 4);
      ctx.beginPath();
      ctx.moveTo(AXIS_WIDTH, y);
      ctx.lineTo(width, y);
      ctx.stroke();
      ctx.fillText(Math.round((peak * g) / 4), 2, y - 3);
    }

    var now = Date.now();
    var span = TRACE_SECONDS * 1000;
    for (var m = 0; m < meters.length; m++) {
      var trace = meters[m].trace;
      if (trace.length < 2) continue;
      ctx.strokeStyle = meters[m].colour;
      ctx.lineWidth = 2.5;
      ctx.beginPath();
      for (var k = 0; k < trace.length; k++) {
        var x =
          AXIS_WIDTH +
          (width - AXIS_WIDTH) * (1 - (now - trace[k].at) / span);
        var py = height - plot * (trace[k].power / peak);
        if (k === 0) ctx.moveTo(x, py);
        else ctx.lineTo(x, py);
      }
      ctx.stroke();
    }
  }

  // A floor of 100 W, so an empty canvas has a sensible axis rather than one
  // running to zero.
  function tracePeak() {
    var peak = 100;
    for (var i = 0; i < meters.length; i++) {
      for (var j = 0; j < meters[i].trace.length; j++) {
        if (meters[i].trace[j].power > peak) peak = meters[i].trace[j].power;
      }
    }
    return peak;
  }

  // ---- Wiring ------------------------------------------------------------

  // Capture phase, so the final batch is queued before Shiny sends the button
  // press. Together with the priority on the samples observer in R, that is
  // what stops the last half-second of a ride arriving after the files have
  // been closed.
  document.addEventListener(
    'click',
    function (e) {
      if (!e.target.closest) return;
      if (e.target.closest('#pair_meter')) {
        pair();
      } else if (e.target.closest('#stop_record')) {
        recording = false;
        flush();
        releaseScreen();
        stopClock();
      }
    },
    true
  );

  $(document).on('shiny:connected', function () {
    send('ble_supported', !!navigator.bluetooth);
    drawStatus();
    requestAnimationFrame(drawTrace);

    Shiny.addCustomMessageHandler('record_start', function (msg) {
      for (var i = 0; i < meters.length; i++) {
        var label = msg.labels[String(meters[i].slot)];
        meters[i].label = label || null;
        meters[i].trace = [];
      }
      pending = [];
      batch = 0;
      recording = true;
      elapsedMs = 0;
      startedAt = Date.now();
      holdScreen();
      drawStatus();
    });

    // The argument is unused, and declaring it is not optional: Shiny checks
    // the handler's arity and throws if it takes none. Thrown from inside
    // shiny:connected, that aborts the socket's open handler and the session
    // never renders anything at all.
    Shiny.addCustomMessageHandler('record_stop', function (msg) {
      recording = false;
      flush();
      releaseScreen();
      stopClock();
      drawStatus();
    });

    // Dropping a meter is a user action, not a fault, so it must not go round
    // the reconnection loop.
    Shiny.addCustomMessageHandler('meter_drop', function (msg) {
      var meter = findMeter(msg.slot);
      if (!meter) return;
      meter.dropped = true;
      if (meter.device.gatt.connected) meter.device.gatt.disconnect();
      meters = meters.filter(function (m) {
        return m.slot !== msg.slot;
      });
      drawStatus();
    });
  });
})();
