# run1 carries A and B, run2 carries B and C. A and C never ride together, so
# A against C has to come through B.
GAIN_A <- 1.00
GAIN_B <- 1.03
GAIN_C <- 0.97

chained_rides <- function(lag_c = 0) {
  ride <- fixture_ride()
  first <- ride[1:1800, ]
  second <- ride[1801:3601, ]

  dplyr::bind_rows(
    as_rides(
      list(
        A = make_source(first, gain = GAIN_A),
        B = make_source(first, gain = GAIN_B)
      ),
      run = "run1"
    ),
    as_rides(
      list(
        B = make_source(second, gain = GAIN_B),
        C = make_source(second, gain = GAIN_C, lag = lag_c)
      ),
      run = "run2"
    )
  )
}

test_that("the bridge is found automatically", {
  aligned <- align_rides(chained_rides())
  out <- withr::with_seed(1, chain_sources(aligned))

  expect_equal(chain_report(out)$bridge, "B")
})

test_that("a gain between meters that never rode together is recovered", {
  aligned <- align_rides(chained_rides())
  out <- withr::with_seed(1, chain_sources(aligned))

  ac <- out[out$source_a == "A" & out$source_b == "C", ]

  expect_equal(ac$basis, "chained")
  expect_equal(ac$via, "B")
  expect_equal(ac$gain_pc, 100 * (GAIN_A / GAIN_C - 1), tolerance = 1e-6)
})

test_that("directly observed pairs are labelled as such", {
  aligned <- align_rides(chained_rides())
  out <- withr::with_seed(1, chain_sources(aligned))

  expect_equal(out$basis, c("direct", "chained", "direct"))
  expect_equal(
    out$gain_pc[out$source_a == "A" & out$source_b == "B"],
    100 * (GAIN_A / GAIN_B - 1),
    tolerance = 1e-6
  )
  expect_equal(
    out$gain_pc[out$source_a == "B" & out$source_b == "C"],
    100 * (GAIN_B / GAIN_C - 1),
    tolerance = 1e-6
  )
})

test_that("chaining survives a clock offset in the second run", {
  aligned <- align_rides(chained_rides(lag_c = 8))
  out <- withr::with_seed(1, chain_sources(aligned))

  ac <- out[out$source_a == "A" & out$source_b == "C", ]
  expect_equal(ac$gain_pc, 100 * (GAIN_A / GAIN_C - 1), tolerance = 1e-6)
})

test_that("a chained comparison is less certain than the ones it is built from", {
  ride <- fixture_ride()
  first <- ride[1:1800, ]
  second <- ride[1801:3601, ]

  rides <- dplyr::bind_rows(
    as_rides(
      list(
        A = make_source(first, gain = GAIN_A, noise = 12, seed = 1),
        B = make_source(first, gain = GAIN_B, noise = 12, seed = 2)
      ),
      run = "run1"
    ),
    as_rides(
      list(
        B = make_source(second, gain = GAIN_B, noise = 12, seed = 3),
        C = make_source(second, gain = GAIN_C, noise = 12, seed = 4)
      ),
      run = "run2"
    )
  )

  out <- withr::with_seed(1, chain_sources(align_rides(rides)))
  width <- out$gain_hi - out$gain_lo

  chained <- width[out$basis == "chained"]
  direct <- width[out$basis == "direct"]

  # A to C carries the sampling error of both runs, so it cannot be tighter
  # than either link.
  expect_gt(chained, max(direct))
})

test_that("chaining needs a source common to more than one run", {
  ride <- fixture_ride()[1:600, ]
  rides <- dplyr::bind_rows(
    as_rides(list(A = ride, B = make_source(ride, gain = 1.03)), run = "run1"),
    as_rides(list(C = ride, D = make_source(ride, gain = 0.97)), run = "run2")
  )

  expect_error(
    chain_sources(align_rides(rides)),
    "No source appears in more than one run"
  )
})

test_that("an ambiguous bridge must be named", {
  ride <- fixture_ride()[1:600, ]
  rides <- dplyr::bind_rows(
    as_rides(list(A = ride, B = make_source(ride, gain = 1.03)), run = "run1"),
    as_rides(list(A = ride, B = make_source(ride, gain = 1.03)), run = "run2")
  )
  aligned <- align_rides(rides)

  expect_error(
    chain_sources(aligned),
    "More than one source could be the bridge"
  )
  expect_no_error(withr::with_seed(1, chain_sources(aligned, bridge = "A")))
})

test_that("a named bridge must actually bridge", {
  expect_error(
    chain_sources(align_rides(chained_rides()), bridge = "A"),
    "appears in only one run"
  )
})

test_that("the report carries the coverage and states the assumption", {
  aligned <- align_rides(chained_rides())
  report <- withr::with_seed(1, chain_sources(aligned)) |> chain_report()

  expect_equal(nrow(report$coverage), 2)
  expect_true(all(report$coverage$seconds > 0))
  expect_gte(report$overlap, 0)
  expect_lte(report$overlap, 1)
  expect_match(report$assumption, "read the same in every run")
})

test_that("a pair ridden twice is pooled, band by band as well as overall", {
  ride <- fixture_ride()
  thirds <- list(ride[1:1200, ], ride[1201:2400, ], ride[2401:3601, ])

  # A meets B twice, reading differently each time. Take only the first run and
  # A against B is -2.91%; take only the third and it is +6.80%.
  rides <- dplyr::bind_rows(
    as_rides(
      list(
        A = make_source(thirds[[1]], gain = 1.00),
        B = make_source(thirds[[1]], gain = GAIN_B)
      ),
      run = "run1"
    ),
    as_rides(
      list(
        B = make_source(thirds[[2]], gain = GAIN_B),
        C = make_source(thirds[[2]], gain = GAIN_C)
      ),
      run = "run2"
    ),
    as_rides(
      list(
        A = make_source(thirds[[3]], gain = 1.10),
        B = make_source(thirds[[3]], gain = GAIN_B)
      ),
      run = "run3"
    )
  )
  aligned <- align_rides(rides)

  low <- 100 * (1.00 / GAIN_B - 1)
  high <- 100 * (1.10 / GAIN_B - 1)

  # A is in two runs as well now, so the bridge has to be named.
  overall <- withr::with_seed(1, chain_sources(aligned, bridge = "B"))
  ab <- overall[overall$source_a == "A" & overall$source_b == "B", ]
  expect_gt(ab$gain_pc, low)
  expect_lt(ab$gain_pc, high)

  # The level-wise view has to pool the same two runs. Matching on the band
  # alone takes whichever run happens to sort first and reports run1's gain as
  # though run3 had never been ridden.
  bands <- chain_by_level(aligned, bridge = "B", width = 25)
  ab_bands <- bands[bands$source_a == "A" & bands$source_b == "B", ]

  expect_gt(nrow(ab_bands), 0)
  expect_true(all(ab_bands$gain_pc > low + 0.1))
  expect_true(all(ab_bands$gain_pc < high - 0.1))
})

test_that("a pair that rode together is compared from its own seconds", {
  ride <- fixture_ride()
  first <- ride[1:1800, ]
  second <- ride[1801:3601, ]

  # Three meters in run1, so A and C rode together even though B could link
  # them. Noise and a power floor mean the seconds behind A against C are not
  # quite the seconds behind either leg of the bridge, so a comparison built
  # from the legs is a different number from the pair's own.
  rides <- dplyr::bind_rows(
    as_rides(
      list(
        A = make_source(first, gain = GAIN_A, noise = 10, seed = 1),
        B = make_source(first, gain = GAIN_B, noise = 10, seed = 2),
        C = make_source(first, gain = GAIN_C, noise = 10, seed = 3)
      ),
      run = "run1"
    ),
    as_rides(
      list(
        B = make_source(second, gain = GAIN_B, noise = 10, seed = 4),
        D = make_source(second, gain = 1.05, noise = 10, seed = 5)
      ),
      run = "run2"
    )
  )
  aligned <- align_rides(rides)

  out <- withr::with_seed(1, chain_sources(aligned, min_power = 100))
  own <- withr::with_seed(1, power_agreement(aligned, min_power = 100))

  ac <- out[out$source_a == "A" & out$source_b == "C", ]
  direct <- own[own$source_a == "A" & own$source_b == "C", ]

  expect_equal(ac$basis, "direct")
  expect_equal(ac$gain_pc, direct$gain_pc)

  # Routed through B it would not be, which is what makes the assertion above
  # worth making rather than an identity.
  legs <- out[out$source_a == "A" & out$source_b == "B", ]$gain_pc
  via_b <- out[out$source_a == "B" & out$source_b == "C", ]$gain_pc
  expect_false(isTRUE(all.equal(
    100 * ((1 + legs / 100) * (1 + via_b / 100) - 1),
    ac$gain_pc
  )))

  expect_equal(out$basis[out$source_a == "A" & out$source_b == "D"], "chained")
})

test_that("level-wise chaining keeps only bands both runs covered", {
  aligned <- align_rides(chained_rides())
  by_level <- chain_by_level(aligned, width = 25)

  ac <- by_level[by_level$source_a == "A" & by_level$source_b == "C", ]

  expect_gt(nrow(ac), 0)
  expect_equal(
    ac$gain_pc,
    rep(100 * (GAIN_A / GAIN_C - 1), nrow(ac)),
    tolerance = 1e-6
  )

  bands_per_pair <- table(paste(by_level$source_a, by_level$source_b))
  expect_equal(length(unique(bands_per_pair)), 1)
})
