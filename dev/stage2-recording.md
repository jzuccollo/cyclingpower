# Stage 2 — record from the meters in the browser

A self-contained brief for a fresh session. Everything under "Established facts"
was researched and verified during the Stage 1 work; take it as given and do not
spend the session re-deriving it.

---

## The problem

`cyclingpower` compares two or three power meters that recorded the same riding.
Getting those recordings currently means one head unit per meter: a Garmin for the
pedals, a Wahoo for the crank, a phone for the trainer. Three devices to start
within a second of each other, three to stop, three to export from. Most of the
protocol in `vignette("recording_rides")` exists to manage that, and the failures
it warns about — auto-pause on one unit and not the other, a clock that never got
a GPS fix, smart recording quietly decimating one file — are all consequences of
the recordings being independent.

They do not have to be. A browser can talk to a BLE power meter directly. Stage 2
makes the app the recorder: pair each meter once, press record, and the files land
in a directory the Rides tab can already scan.

## Established facts

Verified, not assumed:

- **The cycling power service is not blocklisted.** Web Bluetooth maintains a GATT
  blocklist at
  `https://github.com/WebBluetoothCG/registries/blob/master/gatt_blocklist.txt`
  (67 lines when checked). Service `0x1818` and characteristic `0x2A63` do not
  appear on it, so a page may read them.
- **`http://localhost` is a secure context**, per the W3C definition, so Web
  Bluetooth is available with no TLS certificate and no self-signed-cert warning.
  `launch_app()` already serves on localhost.
- **One `requestDevice()` per user gesture.** The chooser cannot be opened
  programmatically or in a loop, so pairing three meters needs three separate
  button presses. This is a hard browser rule; design around it rather than
  fighting it.
- **Chrome and Edge only.** Safari has never shipped Web Bluetooth on macOS and
  WebKit has no active bug for it; Mozilla declined the specification on privacy
  grounds. The app must detect `navigator.bluetooth` and say plainly that
  recording needs Chrome, rather than presenting a button that silently does
  nothing.
- **BLE is one-connection-per-peripheral.** While the app holds the trainer's BLE
  connection, Zwift or TrainerRoad cannot also have it. Pair the training app over
  ANT+ instead, and say so in the UI where the user will hit it.

## What to build

### The recorder — `inst/shiny/www/record.js`

Per meter:

1. `navigator.bluetooth.requestDevice({filters: [{services: ['cycling_power']}]})`
2. `device.gatt.connect()`
3. `getPrimaryService('cycling_power')` → `getCharacteristic('cycling_power_measurement')`
   (`0x2A63`)
4. `startNotifications()`, and on `characteristicvaluechanged` parse the packet
5. `Shiny.setInputValue('sample', {...}, {priority: 'event'})`

**Packet layout.** The Cycling Power Measurement characteristic is little-endian:
`uint16` flags at offset 0, then `sint16` instantaneous power in watts at offset 2.
Both fields are mandatory, so power is always at offset 2 regardless of the flags.
Everything after it (pedal balance, torque, wheel and crank revolutions,
accumulated energy) is optional and positioned according to the flag bits — ignore
it for now, but parse the flags anyway so the parser can be extended without
becoming wrong.

Sample at whatever rate the meter notifies (typically ~1 Hz, some meters 4 Hz) and
let R do the resampling; `align_rides()` already puts everything on a 1 Hz grid.

### Data custody

**R appends each sample to `<run>_<source>.csv` as it arrives**, through a
connection opened in append mode, and never holds the ride only in memory. A
websocket hiccup, a browser tab crash or a mistimed Stop should then cost at most
the last sample. The CSV columns are the canonical ones — `time`, `power` — which
`read_csv_ride()` (`R/read_csv.R`) already reads and `canonicalise()` already
validates, so the recorder writes exactly what the analyser expects, and the
filename convention is the one `ride_spec_from_names()` already parses.

Stamp each sample with the browser's clock at arrival, written as ISO 8601 with
the trailing `Z` — `Date.prototype.toISOString()` — which `parse_csv_time()`
reads directly, milliseconds and all. One clock stamping every meter is the whole
point of recording this way, so the stamp must come from the page and never from
anything the meter reports.

**Write a row only when a sample arrives.** A meter that drops out should leave a
gap, which `align_rides()` resamples over and `paired_seconds()` simply has no
row for. Padding the gap with zeros instead would put the dropout into the
comparison as genuine coasting, where it is dropped as a second both meters
agreed on — turning a lost connection into evidence of agreement. Nothing
downstream can recover from that, because by then the two cases look identical.

**The live trace is drawn client-side on a canvas.** Do not round-trip per-second
samples through the reactive graph to redraw a ggplot; the R process is busy
enough on the analysis tabs, and a recorder that stutters while you are riding is
worse than no recorder.

### The handoff

The filesystem, and nothing else. The recorder writes into a directory; the Rides
tab scans that directory. No in-process coupling, no shared reactive, no passing
of data frames between the two halves of the app. This is deliberate: analysis is
repeatable and recording is not, so a bug in the analysis costs a re-run while a
bug in the recorder costs a ride. The boundary keeps one from taking the other
down.

### Also needed

- **Screen Wake Lock API** (`navigator.wakeLock.request('screen')`). Chrome
  throttles background tabs and will stall notifications on a sleeping display.
  Re-acquire on `visibilitychange`.
- **Reconnection on drop.** BLE connections fail; handle
  `gattserverdisconnected` by retrying, and make the connection state visible per
  meter so the user can see mid-ride that one has gone.
- **Zero-offset reminder** before starting, since the whole comparison rests on it.

## The protocol to ride

`vignette("recording_rides")` tells you to ride surges and steps, and warns that
a clean erg session is the worst possible ride. Almost all of that shape exists
because the recordings are independent: the surges are there so the
cross-correlation has an edge to lock onto, and the steps are a compromise
between needing features and needing coverage. Once one clock stamps every
sample, the offset is zero by construction and the compromise dissolves. Erg
mode then becomes the *best* ride rather than the worst, because it puts a
chosen number of seconds into a chosen band instead of leaving band coverage to
how the day went.

One piece has to survive, and for a reason that is easy to miss. With no feature
to lock onto, `check_alignment()` cannot tell "these clocks agree" from "these
traces cannot be compared at all": it collapses both into `unresolved`, forces
the offset to zero and warns. A recorder that stamped every meter wrongly would
pass that check exactly as a correct one does. So a short sync block stays — not
to find the offset, but to make finding it possible, so that a resolved zero is
evidence rather than a default.

### The session

Ride it identically every time. A chained comparison rests on the bridge meter
reading the same in both runs, and `chain_report()$overlap` can only confirm the
two runs covered the same power if they were asked to.

| Block | Duration | Mode | Why |
| --- | --- | --- | --- |
| Zero-offset every meter, then sit still | 30 s | — | A shared floor of true zeros at the head of every file |
| Sync surges: 4 × (8 s hard / 30 s coasting) | 2.5 min | Level | Coast-to-power edges, so `check_alignment()` can resolve rather than default |
| Ladder up: 55, 70, 85, 100, 110% FTP | 5 × 4 min | Erg | Several hundred stable seconds in each band |
| Coast | 60 s | — | Exercises the coasting drop, and marks the halves for the drift check |
| Ladder down: 110, 100, 85, 70, 55% FTP | 5 × 4 min | Erg | Same bands, later in the ride |
| Sync surges again | 2.5 min | Level | Clock drift within the recorder, if it has any |
| Stop everything at once | 30 s | — | A shared tail |

About 50 minutes. Halve the steps to two minutes if that is too long; two minutes
still leaves each band far above `min_seconds` and gives the block bootstrap
several 60-second blocks to draw from.

**The ladder runs up and then down deliberately.** A meter that reads high
because it has warmed up gives different numbers at the same target on the way
down; a genuinely level-dependent meter repeats itself. Ridden in one direction
only, the two are indistinguishable, and `level_dependence()` would report the
drift as an error that depends on power.

**Do not surge in erg.** The trainer fights the surge and rounds off precisely
the edge the cross-correlation needs. Switch to level or slope mode for the sync
blocks.

**Erg control has to come over ANT+.** While the page holds the trainer's BLE
connection, nothing else can, including whatever is commanding the targets. A
dual-band trainer takes FE-C control over ANT+ and broadcasts power over BLE at
the same time, so the training app or head unit drives the ladder while the
recorder listens. This is the constraint noted above, met head-on rather than
worked around.

**Erg makes the trainer the controlled variable**, holding its own reading at
target, so within a step the trainer is nearly flat and almost all the spread
comes from the other meter. That does not bias anything — `level` is the mean of
the two, not the trainer's number — but it does mean the trainer decides which
band a second lands in. Worth knowing before reading a by-level chart from an erg
session as though both meters had wandered into the band together.

Hold a steady cadence through each step, and if you want one extra thing from the
session, repeat a single step at two cadences perhaps 25 rpm apart. A crank or
pedal meter's error can depend on cadence, the channel is carried through the
schema, and nothing in the package looks at it yet — so this is a hook for later
work rather than something the current pipeline will tell you.

## Testing

Unit-test the packet parser against captured byte arrays: flags with and without
the optional fields set, a negative power value (the field is signed), and a
realistic multi-sample sequence. That part is testable and should be tested
properly.

The radio path is not. It needs a physical meter and cannot run in CI or headless.
**Say so explicitly** in the test file and the vignette rather than papering over
it — this is a real departure from the suite's discipline of planting a known
answer and asserting the machinery recovers it, and the rest of the package's
credibility rests on that discipline being visible where it holds and where it
does not.

## Acceptance

1. `navigator.bluetooth` absent → the tab explains the browser requirement and
   offers no dead buttons.
2. Pair one meter → its name and connection state appear; pair a second → both.
3. Record → a canvas trace updates live, and the CSVs on disk grow as it does.
4. Kill the browser tab mid-ride → the CSVs hold everything up to the last sample.
5. Stop → the Rides tab scans the directory and the spec pre-fills from the
   filenames with no editing.
6. The full pipeline runs on the recorded files, and `alignment_report()` shows an
   offset of zero with status `resolved`, since one clock produced all of them.
   `resolved` is the load-bearing half: an offset of zero on its own is what
   `check_alignment()` returns when it could not tell, so a recording that ends
   `unresolved` has not passed this, and a session with no sync block cannot
   pass it at all.
7. A meter unpaired mid-ride leaves a gap in its CSV, not a run of zeros, and the
   seconds it missed are absent from `paired_seconds()` rather than counted as
   agreement.

## Out of scope

Anything ANT+. It needs a USB stick and a native driver, which is the coupling
this design exists to avoid. If a meter is ANT+-only, it keeps its head unit and
the file gets scanned like any other.
