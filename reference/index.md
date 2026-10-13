# Package index

## Reading rides

Get files onto one schema. Every reader returns the same shape whatever
the container format.

- [`read_file()`](https://jzuccollo.github.io/cyclingpower/reference/read_file.md)
  : Read a ride file
- [`read_rides()`](https://jzuccollo.github.io/cyclingpower/reference/read_rides.md)
  : Read several ride files into one long tibble
- [`ride_spec_from_names()`](https://jzuccollo.github.io/cyclingpower/reference/ride_spec_from_names.md)
  : Build a ride spec from filenames
- [`demo_rides()`](https://jzuccollo.github.io/cyclingpower/reference/demo_rides.md)
  : Write a set of demonstration ride files

## Aligning

Put the sources on a common one-second clock. Everything downstream
assumes this has happened.

- [`align_rides()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md)
  [`alignment_report()`](https://jzuccollo.github.io/cyclingpower/reference/align_rides.md)
  : Put sources onto a common one-second clock

## Comparing

How far apart two meters read, overall and across the power range. All
intervals come from a moving-block bootstrap.

- [`power_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/power_agreement.md)
  : How far apart two meters read
- [`agreement_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/agreement_by_level.md)
  : How the difference between two meters varies with power
- [`level_dependence()`](https://jzuccollo.github.io/cyclingpower/reference/level_dependence.md)
  : Whether a pair's error changes across the power range
- [`print(`*`<cyclingpower_agreement>`*`)`](https://jzuccollo.github.io/cyclingpower/reference/cyclingpower_print.md)
  [`print(`*`<cyclingpower_chain>`*`)`](https://jzuccollo.github.io/cyclingpower/reference/cyclingpower_print.md)
  : The answer, before the table

## Chaining

Meters that never rode together, linked through one that rode with both.

- [`chain_sources()`](https://jzuccollo.github.io/cyclingpower/reference/chain_sources.md)
  [`chain_report()`](https://jzuccollo.github.io/cyclingpower/reference/chain_sources.md)
  : Put meters from different rides on one scale
- [`chain_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/chain_by_level.md)
  : How the chained comparison varies with power
- [`bridge_candidates()`](https://jzuccollo.github.io/cyclingpower/reference/bridge_candidates.md)
  : Sources that could bridge two runs

## Charts and reports

- [`plot_timeseries()`](https://jzuccollo.github.io/cyclingpower/reference/plot_timeseries.md)
  : Power over time, by source
- [`plot_agreement()`](https://jzuccollo.github.io/cyclingpower/reference/plot_agreement.md)
  : Bland-Altman plot of the difference between two meters
- [`plot_by_level()`](https://jzuccollo.github.io/cyclingpower/reference/plot_by_level.md)
  : Difference between two meters across the power range
- [`plot_chain()`](https://jzuccollo.github.io/cyclingpower/reference/plot_chain.md)
  : Gains between meters on one scale
- [`compare_report()`](https://jzuccollo.github.io/cyclingpower/reference/compare_report.md)
  : Render the whole comparison from a set of files
- [`launch_app()`](https://jzuccollo.github.io/cyclingpower/reference/launch_app.md)
  : Explore a comparison interactively

## Recording a comparison

The session to ride, as data rather than prose, and as a file a trainer
app can follow.

- [`comparison_protocol()`](https://jzuccollo.github.io/cyclingpower/reference/comparison_protocol.md)
  : The session to ride for a comparison
- [`collapse_protocol()`](https://jzuccollo.github.io/cyclingpower/reference/collapse_protocol.md)
  : The protocol as blocks to read
- [`write_workout()`](https://jzuccollo.github.io/cyclingpower/reference/write_workout.md)
  : Write the protocol as a trainer workout file

## Ride metrics

The Coggan definitions, and a simulator that hits a chosen NP and AP.

- [`average_power()`](https://jzuccollo.github.io/cyclingpower/reference/average_power.md)
  : Average power
- [`intensity_factor()`](https://jzuccollo.github.io/cyclingpower/reference/intensity_factor.md)
  : Intensity factor
- [`normalised_power()`](https://jzuccollo.github.io/cyclingpower/reference/normalised_power.md)
  : Normalised power
- [`tss()`](https://jzuccollo.github.io/cyclingpower/reference/tss.md) :
  Training stress score
- [`variability_index()`](https://jzuccollo.github.io/cyclingpower/reference/variability_index.md)
  : Variability index
- [`simulate_power()`](https://jzuccollo.github.io/cyclingpower/reference/simulate_power.md)
  : Simulate a power series with a given average and normalised power
