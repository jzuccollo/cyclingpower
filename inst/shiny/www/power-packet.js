// Cycling Power Measurement (characteristic 0x2A63), parsed here and nowhere
// else. The file touches no browser global — no window, no Shiny, no DOM —
// because it is the one part of the recorder that can be tested without a
// radio, and test-record.R runs it under V8. Keep it that way: anything
// needing a browser belongs in record.js.

// The characteristic is little-endian throughout. Two fields are mandatory:
// uint16 flags at offset 0 and sint16 instantaneous watts at offset 2. Power is
// therefore always at offset 2 whatever the flags say, and signed, because a
// pedal meter can report a small negative value when it is being back-driven.

// The optional fields, in the order the specification lays them out, with the
// flag bit announcing each and the bytes it occupies. Nothing here reads their
// values; the table is walked anyway so the flags are decoded against the real
// layout. Adding cadence later is then reading a field at an offset this
// already computed, rather than rediscovering the layout and getting it a byte
// wrong.
var CYCLING_POWER_FIELDS = [
  { bit: 0, name: 'pedalPowerBalance', bytes: 1 },
  { bit: 2, name: 'accumulatedTorque', bytes: 2 },
  { bit: 4, name: 'wheelRevolutionData', bytes: 6 },
  { bit: 5, name: 'crankRevolutionData', bytes: 4 },
  { bit: 6, name: 'extremeForceMagnitudes', bytes: 4 },
  { bit: 7, name: 'extremeTorqueMagnitudes', bytes: 4 },
  { bit: 8, name: 'extremeAngles', bytes: 3 },
  { bit: 9, name: 'topDeadSpotAngle', bytes: 2 },
  { bit: 10, name: 'bottomDeadSpotAngle', bytes: 2 },
  { bit: 11, name: 'accumulatedEnergy', bytes: 2 }
];

// Bits that qualify another field rather than announcing one of their own, so
// they cost no bytes and shift nothing.
var CYCLING_POWER_INDICATORS = [
  { bit: 1, name: 'pedalPowerBalanceReference' },
  { bit: 3, name: 'accumulatedTorqueSource' },
  { bit: 12, name: 'offsetCompensationIndicator' }
];

// Returns the two mandatory fields, the byte offset of every optional field
// the flags claim is present (null where absent), and the packet length those
// flags imply. The length is the extensibility check: a parser that has the
// layout right agrees with the meter about how long its own packet is.
function parseCyclingPower(view) {
  var flags = view.getUint16(0, true);
  var offsets = {};
  var indicators = {};
  var offset = 4;

  for (var i = 0; i < CYCLING_POWER_FIELDS.length; i++) {
    var field = CYCLING_POWER_FIELDS[i];
    if (flags & (1 << field.bit)) {
      offsets[field.name] = offset;
      offset += field.bytes;
    } else {
      offsets[field.name] = null;
    }
  }

  for (var j = 0; j < CYCLING_POWER_INDICATORS.length; j++) {
    var indicator = CYCLING_POWER_INDICATORS[j];
    indicators[indicator.name] = (flags & (1 << indicator.bit)) !== 0;
  }

  return {
    flags: flags,
    power: view.getInt16(2, true),
    offsets: offsets,
    indicators: indicators,
    length: offset
  };
}

globalThis.parseCyclingPower = parseCyclingPower;
