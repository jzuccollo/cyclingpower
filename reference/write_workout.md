# Write the protocol as a trainer workout file

Turns
[`comparison_protocol()`](https://jzuccollo.github.io/cyclingpower/reference/comparison_protocol.md)
into something a training app can ride, so the session is commanded
rather than remembered. The format follows the file extension, the way
[`read_file()`](https://jzuccollo.github.io/cyclingpower/reference/read_file.md)
dispatches on it.

## Usage

``` r
write_workout(path, ftp = 250, protocol = comparison_protocol())
```

## Arguments

- path:

  File to write. The extension chooses the format: `.zwo` or `.erg`.

- ftp:

  Functional threshold power in watts. `.zwo` stores targets as
  fractions of the rider's FTP, so this only reaches the description
  there; `.erg` stores absolute watts, where a wrong FTP is a wrong
  workout. The default is the package's reference rider; set your own.

- protocol:

  A protocol from
  [`comparison_protocol()`](https://jzuccollo.github.io/cyclingpower/reference/comparison_protocol.md).

## Value

`path`, invisibly.

## Details

`.zwo` (Zwift) carries the distinction the protocol depends on: erg
steps are written as `SteadyState` and the sync surges and coasts as
`FreeRide`, which releases erg control for exactly the blocks that must
not be ridden in it. `.erg` (Golden Cheetah, TrainerDay, PerfPro) has no
such concept and is a plain watts-against-time course, so the surge
blocks come out as targets the trainer will try to hold. Drop out of erg
for them by hand.

The trainer holding the targets must be commanded over ANT+: while the
Record tab has the trainer's Bluetooth connection, nothing else can have
it.

## Examples

``` r
path <- tempfile(fileext = ".zwo")
write_workout(path, ftp = 250)
```
