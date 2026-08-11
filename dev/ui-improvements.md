# Shiny app: remaining UI work

Plan for the findings left after the first pass (Chain merged into Agreement,
intervals in single cells, provenance line, scan-twice dead-end, swallowed
directory errors, report reproducibility, facet fix, `fillable = FALSE`). Six
phases, each ending with the app working and verified.

Sources: a static source review, and a later **visual** review at 1600×857 and
844×871 with the app running. The visual pass found things the static one could
not — most of phase 0 comes from it — and refuted one static finding, which is
recorded under "Recommended against".

## Where the code actually is

The plan below was written before a visual pass, and some of it was then
implemented ad hoc rather than in phase order. **Read this table first** — the
phases still describe the reasoning, but this says what is left.

**Everything in this table is now done.** What follows records how, and the
four places where doing it changed the plan.

| Item | State |
|---|---|
| 0a page must scroll | **Done.** `fillable = FALSE` is not enough: `page_navbar()` routes through `page_fillable()` whenever it carries a sidebar, so the body keeps `bslib-page-fill` and there is no argument that avoids it. CSS overrides `body.bslib-page-fill` to `height: auto`, drops the inner scroll on `.main`, and makes the sidebar `position: sticky`. Verified `documentElement.scrollHeight > innerHeight` and `.main` scrollHeight == clientHeight |
| 0b by-level plot invisible | **Done** (fixed by 0a) |
| 0c line measure | **Done** — `max-width: 68ch` on `.guidance`, `.alert`, `.form-text` |
| 0d card headers | **Done** — 1.05rem/600, above the form labels |
| 0e chained row reads as broken | **Done.** The pair table is built by hand rather than through `renderTable` so a chained row can carry a class: row tint, a `chained` badge in the `From` cell, and a footnote glossing the em-dash directly under the table |
| 0f agreement table width | **Done.** Estimates are padded to a fixed width and the cells set in `white-space: pre`, so the point estimates form a real column — measured, all three now start at the same x (702px for gain, 888px for watts) where they were at 760/756/749. Table is 979px inside a 1,305px pane. Bounds are signed like the estimate throughout, limits of agreement included |
| 0g responsive | **Done.** Path column truncated to the basename with the full path as a tooltip; `overflow-x: auto` on the table wrappers; `Pair` reads `a vs b` rather than `a - b`, so the separator no longer looks like a minus. The hamburger finding did not reproduce — see "What the plan got wrong" |
| 0h smaller items | **Done.** Alignment table rebuilt with `status` second and `offset` as a signed integer; spec table unpaginated, by-level table paginated at 10; Report tab carries a settings summary and file list beside the render controls |
| 1a load feedback | **Done.** `input_task_button` for load and demo, plus a `load_status` line depending on `rides()` and an `actionLink` to Alignment |
| 1b report render/download split | **Done.** Render to a tempfile, fingerprint the settings, and show Download only while the fingerprint matches. Verified: changing `min_power` after a render replaces the button with the staleness warning |
| 1c cosmetic batch | **Done**, except `layout_columns` on Agreement — see "What the plan got wrong" |
| 2 band gain intervals, band labels, per-cent scale | **Done.** `block_indices()` extracted, `level_summary()` evaluates both statistics on one set of draws, `band_range` labelled in `agreement_by_level()` where `width` is in scope, `plot_by_level(scale =)` added |
| 3a value boxes | **Done** on all three results tabs |
| 3b guidance restructure | **Done** — `details()` accordions, closed by default |
| 3c data-driven caveats | **Done.** `level_dependence()` is exported and drives both the app alert and a report callout; thin-band and edge-of-search alerts added |
| 4 sidebar consolidation, reseed, spec exclusion | **Done.** All four controls moved in under headings; `max_offset` folded into the debounced `settings()` bundle; `provenance()` is one component rendered on three tabs; reseed comparison and row removal both work |
| 5a plot legibility | **Done.** Strip backgrounds and panel spacing so a stacked strip is not read as the previous panel's axis title; `aspect.ratio` fixed on Bland-Altman; the three reference lines given a real legend; traces alpha and linewidth rebalanced |
| 5b Bland-Altman density | **Done** — `geom_bin2d(bins = 60)` on a log fill scale |

### What the plan got wrong

Four things did not survive contact, and the code follows the evidence rather
than the plan.

- **`level_dependence()` fires for a clean percentage meter under the plan's own
  acceptance test.** `make_source(fixture_ride(), gain = 1.03, noise = 4)` gives
  a per-cent span of 0.70 points and separated extreme bands in 6 seeds out of
  6, so it is a property of the data rather than bootstrap noise. The cause is
  the helper putting all the noise on one source: at that amplitude the
  asymmetry bends the log ratio enough to separate 3,600 seconds' worth of
  bands. The synthetic test therefore uses `noise = 2` (span 0.21, fires in 0
  seeds out of 6), and a second test asserts the distinction on the demo data,
  where both meters carry noise and the planted answer — fires for quarq, not
  for kickr — is the one the app actually shows. The trigger itself is
  unchanged; it is the acceptance criterion that was wrong.
- **`layout_columns` on Agreement starves both halves.** Measured, the pair
  table wants ~979px for its six columns and the faceted Bland-Altman wants at
  least as much again. Splitting a 900px pane 5/7 gave a table scrolling
  sideways behind its own card edge with only `Pair` and `From` visible, and a
  456px plot in which a tilt cannot be read. Both cards are full width and
  stacked. `table.shiny-table { width: auto }` is what fixes the original
  complaint, and it already did.
- **The hamburger in 0g does not reproduce in bslib 0.11**, which emits no
  `navbar-expand` class at all: the links stay a wrapping row down to 844px.
  `navbar_options(collapsible = FALSE)` is an active trap — with no expand class
  to fall back on, Bootstrap stacks the five links vertically and costs 470px of
  page before any content, which is worse than the menu it was meant to remove.
  Nothing in the app now touches the navbar's collapse behaviour.
- **`chart_theme()`'s `scale_colour_manual()` had to go too**, not just the dead
  `scale_fill_manual()`. It silently replaced the Bland-Altman plot's own colour
  scale — the one naming zero, the mean difference and the limits — so the
  legend the phase asked for could not exist while it was there. The palette is
  now applied explicitly by the two plots that map a source or a basis.

One item in 5a was deliberately left: suppressing or annotating the limits of
agreement when `level_dependence()` fires. The plan raised it as "worth
considering", and a plot whose geometry changes at a hidden threshold is the
thing the plan argues against elsewhere.

Also done outside the plan: a `bslib::bs_theme()` with a warm grey scale and a
single slate accent, on the principle that **every saturated colour belongs to
data** — the Okabe-Ito series should be the only strong colour on screen.
Results tables are formatted through `with_ci()` and `by_level_rows()` rather
than dumping raw tibbles.

## Verifying in the browser

`testServer()` exercises the reactive graph but never serialises the UI, so it
cannot catch a broken layout, a zero-height container or a missing JS
dependency — the by-level plot was invisible for the entire life of the app and
every headless check passed. Look at it.

Claude in Chrome works for this. The recipe that worked, in order:

1. Start the app on a fresh port, in the background:
   `nohup R -q -e 'devtools::load_all("."); launch_app(port = 8404, launch.browser = FALSE)' > log 2>&1 &`
   Wait ~30 s, then `curl -s -o /dev/null -w "%{http_code}" http://127.0.0.1:8404/`.
2. `list_connected_browsers` → **ask which one** if more than one is connected,
   then `select_browser` with its deviceId.
3. `tabs_context_mcp{createIfEmpty: true}`, then `navigate`.
4. Load the demo **via JS**: `document.querySelector('#demo').click()`. Clicking
   by coordinate or by `ref` failed silently more than once when the button sat
   below a fold or the nav had collapsed to a hamburger.
5. Switch tabs the same way:
   `[...document.querySelectorAll('a.nav-link')].find(a => a.textContent.trim() === 'By level').click()`.
6. **Wait for the computation, then check it actually finished.** Ten seconds is
   not always enough at 500 replicates. Assert
   `document.querySelectorAll('.recalculating').length === 0` before screenshotting,
   or you will photograph an empty card and think the output is broken.
7. Screenshot with `save_to_disk: true` when it needs to be shown.
8. Kill the server and close any tab you created.

Measure rather than eyeball. `javascript_tool` returning
`getBoundingClientRect()` values is what proved the plot occlusion and what
proved the fix; a screenshot alone would have shown "a plot appeared". Useful
assertions: `container.clientHeight` against `img.height` for a collapsed plot
container, `img.bottom` against `table.top` for overlap,
`document.documentElement.scrollHeight > window.innerHeight` for page scroll,
`getComputedStyle(el).width` for a table that will not shrink.

`resize_window` was a no-op for several minutes in one session and then started
working; if a width claim matters, re-verify `window.innerWidth` in JS rather
than trusting that the resize took.

**Assert `document.visibilityState === 'visible'` before believing an output is
stuck.** This is the single worst trap in the loop, and it cost most of a
session. Shiny 1.14 resumes a suspended output from a `ResizeObserver` and an
`IntersectionObserver` on the output element. Chrome does not run either in a
backgrounded tab — the automation window is backgrounded whenever anything else
has focus — so switching tabs leaves every output on the new tab suspended
**for ever**, with `.clientdata_output_<id>_hidden` stuck at `true`, R at 0%
CPU, and nothing in the log. It looks exactly like a reactive deadlock, and
`testServer()` will cheerfully show you the same graph resolving fine, which
sends you looking for a client-side bug that is not there.

Taking a screenshot forces a paint and unsticks it, which is why the outputs
always seemed to appear "just after" a screenshot and why a `resize_window`
appeared to fix things. So: **screenshot first, then measure** — never measure a
tab you have not just painted. The one-line check is

```js
({visible: document.visibilityState, raf: await new Promise(r =>
  { let f = false; requestAnimationFrame(() => {f = true; r(true)}); setTimeout(() => f || r(false), 1000) })})
```

If `raf` is `false` the tab is frozen and every timing measurement from it is
meaningless.

## Traps hit while implementing

- **Quotes inside the CSS string.** The theme is `bs_add_rules("…")`, a plain R
  string. A `"` in a CSS comment terminates it and the app dies at source time
  with a misleading "possible missing comma" 800 lines later. Use no double
  quotes in that block. `parse("inst/shiny/app.R")` catches it in a second and
  is worth running before every restart.
- **The reciprocal gain.** Writing the value-box sentence as "kickr reads 2.58%
  below assioma" for a `+2.58%` gain is wrong — that figure is 2.51%. Phrase
  every sentence in `source_a`'s own orientation and vary only the word
  higher/lower. This was written into the plan as a risk and then done anyway;
  it is easy to slip into.
- **`reactiveVal` does not invalidate when set to an identical value**, which is
  what made scanning the same directory twice a silent no-op. Counters, not
  values, for anything that must re-fire.
- **`validate()` inside a reactive consumed only by an observer** is swallowed
  and the message never reaches the screen. Return the problem as a value.
- **`updateTextInput` is asynchronous**, so a handler that sets a box and then
  reads it gets the previous value. Write to a `reactiveVal` instead.

## Verified against the installed packages

shiny 1.14.0, bslib 0.11.0, DT 0.34.0, ggplot2 4.0.3. These claims are the ones
the plan rests on; each was checked rather than recalled.

| Claim | Verdict |
|---|---|
| `bslib::input_task_button()`, `bind_task_button()`, `update_task_button()` | exist; `type` is pasted into `btn-{type}`, so `type = "outline-secondary"` works |
| Task button pairs with plain `observeEvent`/`eventReactive` | yes — it reverts after the server has dealt with the reactivity triggered by the click. `ExtendedTask` is **not** required |
| `updateActionButton(disabled = TRUE)` on a task button | **broken.** The binding is `receiveMessage(el, {state})` only; a `{disabled: …}` message is ignored |
| `bslib::nav_select(id, selected)` | exists; the navbar already has `id = "nav"` and panel titles are the values |
| `value_box()`, `value_box_theme()`, `layout_columns()`, `layout_column_wrap()`, `breakpoints()`, `navset_card_tab()`, `accordion()`, `popover()` | all exist |
| `accordion_panel(open = FALSE)` | **wrong API.** `open` belongs to `accordion()` |
| `downloadButton(enabled = "auto")` | exists, but only means "once the handler is registered" — it cannot express "once data is loaded" |
| `shiny::busyIndicatorOptions()` | exists in **shiny**, not bslib. Styles spinners for *outputs*; does nothing for a `downloadHandler` |
| `geom_hex()` | **`hexbin` is not installed** — it would fail at runtime. Use `geom_bin2d()` |
| DT single-click editing | not available; DT requires a double-click. The fix is discoverability, not configuration |
| `agreement_by_level()` returns `gain_lo`/`gain_hi` | **no.** `level_summary()` bootstraps bias only and returns `gain_pc` as a bare point estimate |
| `attr(by_level, "dropped_seconds")` survives the `out[kept, ]` subset | yes — 134 seconds dropped on demo data at defaults |
| `chart_theme()`'s `scale_fill_manual()` is used by any plot | **no** — nothing maps `fill`. It is dead, and it is what would collide with a continuous fill in phase 5 |
| `page_navbar(fillable = FALSE)` gives a non-fill page container | **no** when there is a sidebar. `page_navbar()` picks `page()` only `if (isFALSE(fillable) && is.null(sidebar))`; otherwise it is `page_fillable()` and the body keeps `bslib-page-fill`. There is no argument for it, so 0a is a CSS override |
| bslib 0.11 `page_navbar()` collapses the navbar below 992px | **no** — it emits no `navbar-expand` class, so the links wrap rather than collapse. `navbar_options(collapsible = FALSE)` makes it worse, not better |
| `input_task_button(type = "outline-secondary")` reverts under plain `observeEvent` | yes, both confirmed running |

Demo-data reference points (seed 1, defaults, `min_seconds = 30`): 2 direct
pairs and 1 chained; `assioma – kickr` `+2.58% (2.41 to 2.75)`; `assioma –
quarq` `−0.61% (−1.61 to 0.44)` with band gains from `+3.7%` at 87.5 W to
`−5.0%` at 387.5 W; 23 by-level rows; thinnest kept band 31 s;
`power_agreement()` ≈ 0.8 s, `agreement_by_level()` ≈ 1.2 s at 500 replicates.

## Order

Phase 0 first — it is what makes the app legible, and later phases depend on it.
A value box is no use inside a porthole, and a caveat that cannot share a screen
with the row it qualifies is not a caveat.

After that, ranked by value against effort: load feedback; report
render/download split; the cosmetic batch; headline value boxes; per-cent
by-level with band gain intervals; data-driven caveats; guidance restructure;
sidebar consolidation; spec exclusion; reseed comparison; plot legibility and
density.

**One sequencing point overrides that ranking.** The strongest data-driven
caveat — "this meter's error is not constant across the range" — cannot be
built honestly until the by-level gain intervals exist. Triggered on watts, the
only intervals available today, it fires for *every* clean percentage meter
including the demo's kickr, whose watts difference rises from 2.85 W to 7.78 W
with non-overlapping intervals purely because 2.5% of 300 W exceeds 2.5% of
100 W. That is the precise confusion the caveat exists to dispel. Package work
precedes alert work.

---

## Phase 0 — layout and legibility (from the visual review)

Do this first. It is the phase that makes the app legible at all, and several
later items are pointless until it lands — a value box is no use inside a
porthole, and a data-driven caveat that cannot be on screen at the same time as
the row it qualifies is not a caveat.

### 0a. The page must scroll (partly done)

`page_navbar(fillable = TRUE)` is the bslib default, and it turns every card
into a fixed-height porthole with a hidden inner scroll. macOS overlay
scrollbars mean there is no cue that anything is below. Measured at 844×871:
the "Every pair" card body showed **448 px of 1,275 px**; Bland-Altman **180 px
of 542 px**. At 1600×857 with the sidebar collapsed it reaches only 56%.

Consequences photographed, all of which read as rendering faults rather than as
scrolling:

- The footnote under the agreement table sliced horizontally **through the
  middle of its glyphs** by the card edge.
- The Bland-Altman y-axis title reading `erence (watts)`.
- Card bodies opening mid-sentence, because the body is scrolled.
- Table headers scrolling away and **not sticky**, so By level becomes 23 rows
  of unlabelled numbers.
- At 844 px, three nested scroll regions inside one card on Rides.
- At 844 px, **"Load demo rides" is unreachable** — its rect is y=436–483 while
  the card body ends at y=422, and `elementFromPoint` returns the next card's
  header. The onboarding action is behind an invisible scroll. Bites below
  roughly 1100 px.

**Done:** `fillable = FALSE` on `page_navbar`. This restored the plot
containers (0b) and is verified.

**Still to do:** `document.body` retains `bslib-page-fill`; the window does not
scroll and `.main` has an inner scroll of 1,936 px inside 566 px of viewport.
Establish whether a non-fill page container is reachable through bslib's own
arguments before reaching for CSS overrides, and verify by asserting
`document.documentElement.scrollHeight > window.innerHeight` in the browser
rather than by eye.

### 0b. The by-level plot was invisible (done)

The most serious finding, and invisible from source. `plotOutput(height = "auto")`
inside a fill container collapsed to `clientHeight: 0` while
`renderPlot(height = …)` sized the `<img>` to 520 px. The image overflowed a
zero-height div, so the DT was laid out immediately after it and painted over
the top — **496 px of a 520 px plot**, leaving only the title strip. It affected
By level alone because that is the one card where anything follows the plot.

Fixed by 0a and verified in the browser: container 520 px, image 520 px, table
top at 539 px, zero overlap.

This corrects a static finding: "the by-level table duplicates its plot" was
wrong. The table was *destroying* it.

### 0c. Line measure

The real typographic failure, and not the one the static review guessed at.
Paragraphs run ~100 characters per line at 844 px and **190–230 characters at
1600 px**. The amber "Some rows are chained" callout — the most important
caveat in the app — is set at ~195 characters. Nothing has a `max-width`.

Perverse consequence: the sidebar's incidental helper text is the same size and
colour as the main-panel explanations but wraps to 5–6 words a line, so the
least important prose in the app is the most comfortable to read.

Fix: `max-width: 70ch` on all prose, **including the alerts**, not only the
guidance blocks.

### 0d. Card headers are smaller than the labels beside them

Measured: card headers 14.4 px/600, form labels 16 px/400, guidance 14 px.
"Every pair", "Bland-Altman" and "Clock offsets" are among the smallest text in
the interface, and the largest string on any screen is `cyclingpower` in the
navbar — the one string carrying no information. Raise header size above body
text and give the navbar title less weight.

### 0e. The chained row does not read as chained

Verified honestly at 1600 px: the three em-dashes sit ~400 px apart and ~700 px
from the "chained via assioma" label, three-quarters of the row is white, and
**it reads as missing data**. Nothing links the dashes to an explanation — the
footnote beneath talks about bootstrap replicates and never mentions them, and
the amber callout that does explain them is 200 px above and cannot share a
screen with the row at 844 px.

Worse for the stated failure mode: `-3.11 (-4.22 to -2.02)` looks exactly as
authoritative as `+2.58 (2.42 to 2.77)` above it. No tint, no italic, no badge;
"chained" is set in the same plain black as "run1".

Fix, cheaply: a row tint or badge on chained rows, and a footnote glossing the
dash (`— no second both meters recorded`) directly under the table. Keep the
amber callout, shorten its measure, and keep it adjacent to the rows it covers.

### 0f. The agreement table is unreadable across its own width

At 1600 px the table spans **1,355 px for six short values** — `Pair` at x=73,
the `1,800` in `Seconds` at x=1,428, with a 445 px void between `From` and
`Gain %`. Three data rows.

Compounding it, the numeric columns are right-aligned **composite strings**, so
the headline figures do not line up: `+2.58`, `-0.61` and `-3.11` start at
x=760, 756 and 749 respectively, and only the closing bracket aligns. **You
cannot scan down the column of numbers that matter.**

Fix: constrain the table's width, and align the composite cells so the point
estimates form a column — pad the estimate to a fixed width, or split point and
interval into separate right- and left-aligned columns. This supersedes the
static review's `layout_columns` suggestion as the reason to act, though
`layout_columns` is still the mechanism.

Also inconsistent within one cell: `+4.7 (4.2 to 5.3)` signs the point estimate
but not the positive bounds, while negatives are signed throughout. Pick one.

### 0g. Responsive behaviour

- **Below 992 px the whole tab navigation collapses into a hamburger**, and
  opening it pushes the app down 190 px as five plain links with no active
  state. Inherited Bootstrap default, not a decision, and wrong for a desktop
  measurement tool. Set a lower `collapsible` breakpoint or disable collapse.
- At 844 px the main pane overflows horizontally by ~40 px, producing a
  page-level h-scrollbar and clipping every card's right edge.
- At 844 px the spec table's `path` column — a 90-character temp path —
  consumes the full width and pushes `run` and `source`, the editable columns,
  off screen. At 1600 px the path still takes 570 px while the two meaningful
  columns are squeezed at the far right. Truncate or hide the path.
- At 844 px the agreement table's `Gain %` cells break into four lines with an
  orphaned `to`, and `Pair` breaks as `assioma` / `- kickr` so the hyphen reads
  as a minus sign. Clean at 1600 px; a narrow-width problem only.

### 0h. Smaller, from the same pass

- `offset` renders as `0.000` / `7.000` — three decimals on a quantity that is
  integer by construction.
- `status` (`aligned`/`assumed`/`unresolved`), arguably the most important
  column on Alignment, is last and off screen at 844 px.
- DT paginates a 4-row table while the 23-row by-level table has none.
- The Report tab at 1600 px is one paragraph, one button and ~70% empty white.
  It reads as broken rather than minimal; phase 1b's render/download UI helps,
  but it also wants content — the settings summary and file list that go into
  the report itself would sit naturally there.

---

## Phase 1 — the app responds, and the download cannot lie

App-only. No tests; verification is running the demo.

### 1a. Load feedback

**Task buttons.** Replace the `load` and `demo` `actionButton`s with
`bslib::input_task_button(..., label_busy = "Reading files…")`. `demo` takes
`type = "outline-secondary"`. Leave `scan` a plain button, demoted to
`btn-outline-secondary`.

No `ExtendedTask` needed: the button reverts when the reactive cycle started by
the click completes, and that cycle already contains the file read because
`observeEvent(rides(), data_version(…))` reads `rides()` eagerly.

**A status line that depends on `rides()`** — the missing link, since nothing on
the Rides tab currently does:

```r
output$load_status <- renderUI({
  req(load_trigger() > 0)
  loaded <- rides()
  div(
    class = "alert alert-success py-2 mb-0",
    sprintf(
      "Loaded %s files: %s sources across %s runs, %s seconds of riding.",
      nrow(spec_rv()),
      dplyr::n_distinct(loaded$source),
      dplyr::n_distinct(loaded$run),
      format(nrow(loaded), big.mark = ",")
    ),
    actionLink("to_alignment", "Check the clock offsets", class = "ms-2")
  )
})
```

**Do not auto-advance to Alignment.** It would take the user off the only tab
carrying the spec table and the blocking alerts, exactly when they most need to
check that the labels parsed — and Alignment is not the answer anyway. Offer the
link instead: `observeEvent(input$to_alignment, bslib::nav_select("nav", "Alignment"))`.

**Do not try to disable Load until the spec is valid.** `updateActionButton(disabled=)`
does not reach a task button's binding, and rendering the button through
`renderUI` resets its input value on every re-render — a new failure mode for a
benefit the red alert directly above already delivers.

*Risk:* the task button reverts early if the status output is suspended. It is
on the visible tab when Load is clicked, so it is not — but moving the status
line to another tab would make the spinner lie.

### 1b. Report: render, then download, with staleness fencing

The spinner is not the important part. Today, changing `min_power` after
downloading gives a file that silently disagrees with the screen, and clicking
Download before loading gives a browser error with no explanation.

```r
report_path  <- reactiveVal(NULL)
report_error <- reactiveVal(NULL)
report_stamp <- reactiveVal(NULL)

# What the rendered file is a picture of. Any change here and the file on disk
# is no longer what the screen says.
report_key <- reactive(list(align_key(), settings(), level_opts(), bridge()))

observeEvent(input$render, {
  path <- file.path(tempdir(), "power-comparison.html")
  out <- try(render_comparison(report_results(), path), silent = TRUE)
  if (inherits(out, "try-error")) {
    report_error(conditionMessage(attr(out, "condition")))
    report_path(NULL)
  } else {
    report_error(NULL)
    report_path(path)
    report_stamp(isolate(report_key()))
  }
})

output$download <- downloadHandler(
  filename = function() "power-comparison.html",
  content = function(file) file.copy(report_path(), file, overwrite = TRUE)
)
```

`output$report_ui` then shows: "Load some rides first" when `load_trigger() == 0`;
the render button; the error if any; the download button only when
`identical(report_stamp(), report_key())`; and a "settings have changed since
this was rendered" warning otherwise.

Three deliberate choices:

- **No `withProgress`.** `rmarkdown::render()` reports nothing, so a bar would
  invent its position — the thing the UI comment at the top of `app.R` already
  refuses to do. The task button spinner is the honest indicator.
- **`try()` around the render.** Against house style in general, but this is a
  genuine boundary: an observer error tears down the session, and the user can
  act on "pandoc not found". `chain_result()` already does this.
- **Tempfile plus `file.copy`, not rendering inside `content`.** This is what
  makes the staleness check possible, and the second click is then instant.

*Risk:* `report_key()` must cover everything the report depends on. `spec_rv()`
edits that do not trigger a reload cannot change the results but do change the
spec table printed in the report — accept, or add `spec_rv()` to the key.

### 1c. Cosmetic batch

- **Row sag.** `textInput` renders `div.form-group.shiny-input-container`, which
  Bootstrap 5 gives `mb-3`; bare buttons have none, so under `align-items-end`
  the buttons sit a rem low. Add `class = "mb-3"` to `shinyDirButton`, `scan`,
  `load` and `demo`.
- **Button semantics.** Scan → `btn-outline-secondary`; Load is the only
  `btn-primary`, so the green "done" colour goes. **Do not collapse scan and
  load into one button** — scanning exists so you can correct the labels before
  reading, which is what the editable table is for.
- **Demo toast.** Drop `showNotification(duration = 15)`; render the same text
  as a dismissible `alert-info` in the "Runs and sources" card above the table
  it describes, from a `demo_note` reactiveVal cleared on any scan.
- **`min_power`**: `0, 400, 0, step = 5` — 400 W covers "threshold and above"
  for a strong rider, step 5 makes 150 expressible.
- **`full_screen = TRUE`** on the three cards holding plots.
- **By-level plot and table** into `bslib::navset_card_tab()`, the table as a
  `DT::datatable` with `pageLength = 10`, `dom = "tip"`, `rownames = FALSE` and
  formatted columns. Keep the table — in a measurement tool the numbers are the
  point — but stop it being 23 unpaginated rows of `mean_power = 94.47097`.
- **Full-width cards.** `bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)))`
  on Agreement, sitting "Every pair" beside Bland-Altman on a wide monitor and
  stacking below `lg`.

---

## Phase 2 — package: band gain intervals, band labels, a per-cent scale

Touches `bootstrap.R`, `agreement.R`, `comparison_charts.r`, `comparison.Rmd`.

### 2a. Share bootstrap indices (`R/bootstrap.R`)

Extract index generation so a band can be resampled once and two statistics
evaluated on the same draws:

```r
#' @noRd
block_indices <- function(n, block = 60, replicates = 1000) {
  if (n < 2 * block) block <- max(1, floor(n / 4))
  if (n < 8) return(NULL)
  n_blocks <- ceiling(n / block)
  starts <- seq_len(n - block + 1)
  lapply(seq_len(replicates), function(i) {
    idx <- vapply(
      sample(starts, n_blocks, replace = TRUE),
      function(s) seq(s, length.out = block),
      numeric(block)
    )
    as.integer(idx)[seq_len(n)]
  })
}
```

`block_bootstrap_draws()` then calls it. The `sample()` calls happen in the same
order and count as before, so **every existing interval is numerically unchanged
under the same seed** — that property is the thing to check first, and the
existing 159 tests passing unedited is the evidence.

Do **not** collapse `agreement_summary()`'s two independent index sets into one.
It would change published numbers for no benefit.

### 2b. `level_summary()` gains a gain interval; `agreement_by_level()` gains a band label

```r
level_summary <- function(band, block, replicates) {
  idx <- block_indices(nrow(band), block, replicates)
  draws <- function(f) {
    if (is.null(idx)) rep(NA_real_, replicates) else vapply(idx, f, numeric(1))
  }
  bias_ci <- percentile_ci(draws(function(i) mean(band$difference[i])))
  gain_ci <- percentile_ci(draws(function(i) 100 * (exp(mean(band$log_ratio[i])) - 1)))
  …
}
```

Index generation dominates and now happens once instead of twice, so the
per-cent interval is close to free: expect ~1.3–1.5 s against a 1.2 s baseline.

The band label goes in `agreement_by_level()`, where `width` is in scope, so it
can never be labelled with the wrong width downstream — the exact failure of
doing it in the app: `band_range = sprintf("%g–%g W", band - width / 2, band + width / 2)`,
giving `75–100 W` where the table now shows `87.50`.

### 2c. `plot_by_level(by_level, scale = c("watts", "percent"))`

Switch `y`/`ymin`/`ymax` between `bias_w`/`bias_lo`/`bias_hi` and
`gain_pc`/`gain_lo`/`gain_hi`, and swap the labels. Default `"watts"`, so the
report is unchanged unless asked. On the per-cent scale add to the caption the
sentence that makes the toggle worth having: *a constant percentage error is a
rising line in watts and a flat line in per cent.*

### 2d. Report knock-on

- `plot_by_level(results$by_level)` unchanged; add a second call with
  `scale = "percent"` — the data is already computed, so it costs a plot.
- `knitr::kable(results$by_level)` now carries `band_range`, `gain_lo`,
  `gain_hi`. Select and order columns explicitly rather than dumping the tibble.
- A sentence explaining the two panels, in the register of the surrounding prose.

### 2e. Tests

The tests that pay for the phase, using the planted-error helpers:

1. **A pure gain is flat in per cent and rising in watts.** `make_source(fixture_ride(), gain = 1.03, noise = 4)`:
   `bias_w` monotonically increasing across bands, `max(gain_pc) - min(gain_pc)`
   small, extreme bands' gain intervals overlapping. This is the claim the
   toggle exists to make.
2. **A level-dependent meter separates in per cent.** `make_level_dependent(…, threshold = 150)`:
   the extreme bands' gain intervals do **not** overlap.
3. **Band labels match the width.** The 87.5 band at `width = 25` is `"75–100 W"`.
4. **Reproducibility** — same seed, same `gain_lo`/`gain_hi`, guarding the
   `block_indices` refactor.

Both bootstrap tests inside `withr::with_seed()`; the pure-gain case needs
`noise > 0` or the interval collapses to a point.

---

## Phase 3 — the answer on screen, and caveats that fire only when they bite

### 3a. Headline value boxes

A `uiOutput("headline")` above the "Every pair" card, one `value_box` per pair,
wrapped in `layout_column_wrap(width = "280px")`.

**Never invert a gain to phrase it.** `+2.58%` is assioma *above* kickr; kickr
below assioma is `100 × (1/1.0258 − 1) = −2.51%`. Phrase strictly in the
estimate's own orientation — "assioma reads 2.58% higher than kickr" — and the
number on screen always matches the table, the report and `test-demo.R`. This is
the largest risk in the item and the reason to build the sentence from
`source_a`/`source_b` rather than a hard-coded direction.

- **Neutral colours.** No `theme = "success"`/`"danger"`. A meter 2.5% low is a
  measurement, not a pass or a fail.
- **Three or more pairs:** show all, direct first, chained after. No cap, no
  hidden rule about which pairs are worth showing.
- **Chained pairs belong in it**, marked: `showcase = shiny::icon("link")` and a
  body line "Chained via assioma; no shared seconds, so no watts." Excluding
  them would make the headline row disagree with the table beneath about how
  many pairs exist — the class of error the merged `From` column just fixed.

*Refactor:* split `pair_table()` into `pair_rows()` (raw numerics plus
`from`/`basis`) and `pair_table()` (the formatting), so the boxes and the table
are guaranteed to show the same rows.

### 3b. Guidance restructure

Two helpers replacing `guidance()`:

```r
orientation <- function(...) p(class = "mb-2", ...)

details <- function(title, ...) {
  bslib::accordion(bslib::accordion_panel(title, ...), open = FALSE, class = "mb-3")
}
```

Accordion rather than popover: the blocks run 130–190 words, they need to be
keyboard-reachable and to survive printing, and a popover holding four sentences
is a bad container. Every existing word is kept, redistributed — one sentence of
orientation in normal body text per card, the rest inside the panel. The one
deletion is "read `seconds` alongside every estimate", which becomes the alert
in 3c because a number can now answer it.

### 3c. Data-driven caveats

Following the `output$align_alert` model: driven off a column, so the alert
cannot drift from the table it sits above.

**(i) The gain is not constant across the range** (needs phase 2). Put the test
in the package so it can be tested against planted errors and reused by the
report:

```r
#' @export
level_dependence <- function(by_level) {
  by_level |>
    dplyr::group_by(.data$run, .data$source_a, .data$source_b) |>
    dplyr::summarise(
      low_band  = .data$band_range[which.min(.data$gain_pc)],
      low_gain  = min(.data$gain_pc),
      high_band = .data$band_range[which.max(.data$gain_pc)],
      high_gain = max(.data$gain_pc),
      separated = .data$gain_lo[which.max(.data$gain_pc)] >
                  .data$gain_hi[which.min(.data$gain_pc)],
      .groups = "drop"
    )
}
```

The trigger is non-overlapping 95% intervals between the extreme bands — no
arbitrary threshold constant, and directly testable: it must fire for
`make_level_dependent()` and must not fire for a clean `make_source(gain = 1.03, noise = 4)`.
On demo data it fires for quarq and not kickr, which is exactly the planted
distinction.

**(ii) Bands too thin for the stated block.** Better than an arbitrary seconds
threshold: `block_bootstrap_draws()` silently shortens the block to
`floor(n / 4)` when `n < 2 * block`, so the provenance line's "60-second blocks"
is not true of thin bands. Trigger on `seconds < 2 * block` and name what
actually happened — the thinnest band, its seconds, the block length actually
used, and the `dropped_seconds` attribute, which nothing currently surfaces
anywhere in the app or the report.

**(iii) Best offset at the edge of the search window.** When
`abs(offset) == max_offset` and `status == "aligned"`, the true offset may lie
outside the window and the app is reporting a boundary as an answer. Four lines,
real failure mode, no package change.

**Not worth adding:** a "min_power dropped X% of your seconds" caveat. Computing
it honestly needs the unfiltered paired-second count — a package and report
change for a fact the `Seconds` column and provenance line already half-tell.

---

## Phase 4 — one set of knobs, and two loose ends

### 4a. Commit to the sidebar principle

**Move the four tab-local controls in. Do not adopt per-panel sidebars.**

Three of the four (`max_offset`, `width`, `min_seconds`) reach the downloaded
report, and the report outlives the session. A control that determines a saved
document's contents but lives on a tab the reader may never open is precisely
the failure this app exists to prevent. `max_offset` is worse: it is in
`align_key()`, so dragging it invalidates every cached result on tabs the user
cannot see, and the only present evidence is that things are slow later.
Per-panel sidebars would formalise that invisibility.

Headings and rules, not nested accordions — 260 px is too narrow, and the
argument for moving these in is visibility. Sections: Comparison
(`min_power`, `block`, `replicates`, `seed`, Reseed), Alignment (`max_offset`,
`smooth`), Power bands (`width`, `min_seconds`).

Two consequential details:

- **Debounce `max_offset`.** Now that anyone can drag past it, each intermediate
  value invalidates `align_key()` and everything downstream. Fold it into the
  debounced `settings()` bundle and have `alignment()`/`align_key()` read it
  from there. Without this, phase 4 makes the app slower than it was.
- **One provenance component on three tabs.** Turn `output$provenance` into a
  function returning the `div`, rendered under the Agreement table, the by-level
  plot (adding width and min_seconds) and the alignment table (naming
  max_offset). Each tab's numbers then name their own settings, which is what
  the consolidation costs and must repay.

**Report knock-on:** `smooth` and `min_seconds` do not reach the report at all
today — `comparison.Rmd` calls `plot_timeseries(aligned)` at the default 30, and
`compare_all()` omits `min_seconds`, so `compare_report()` silently uses 30
while the app passes the user's value. Add both to `compare_all()` and
`compare_report()`, carry them in `settings`, and use them in the Rmd.

*New test* (`tests/testthat/test-report.R`, which does not exist):
`compare_all(aligned, min_seconds = 200)$by_level` has `all(seconds >= 200)`,
and `settings` round-trips every knob. Smallest test that catches a dropped
parameter.

### 4b. Reseed comparison

```r
comparable <- reactive(list(align_key(), settings()[c("min_power", "block", "replicates")]))
previous   <- reactiveVal(NULL)

observeEvent(input$reseed, {
  previous(list(seed = input$seed, rows = isolate(pair_rows()), key = isolate(comparable())))
  updateNumericInput(session, "seed", value = input$seed + 1L)
})
```

In the value box body, when `identical(previous()$key, comparable())`, add
"Seed 1 gave +2.58 (2.41 to 2.75)."

**The trap:** the obvious implementation clears the snapshot with
`observeEvent(list(align_key(), settings()$min_power, …), previous(NULL))`.
`observeEvent` fires on *invalidation*, not on value change, so `settings()`
re-firing after the seed bump wipes the snapshot immediately every time.
Comparing a stored fingerprint at render time is the version that works — and it
must be checked at display time, or a previous-seed number appears beside a
current one computed under different settings.

### 4c. Spec table: exclusion and an edit hint

DT cannot edit on single click. Instead: a one-line hint in normal body text
("Double-click a run or source cell to edit it. Select rows and use Remove to
leave a file out."), `selection = "multiple"` (selection and `editable` coexist —
click selects, double-click edits), and a Remove button doing
`set_spec(spec_rv()[-input$spec_rows_selected, ])`. This is the missing
capability: today a stray `.csv` can only be dealt with by moving the file.

---

## Phase 5 — plot legibility and density

All of this changes `R/comparison_charts.r`, so it changes the rendered report
too. That is why it is last and separable.

### 5a. Legibility, from the visual pass

- **Facet strips render at roughly 6–7 px** in a 1240 px-wide plot. They are the
  only thing identifying which pair each panel shows, and they are the smallest
  type in the app. Set an explicit `strip.text` size in `chart_theme()`.
- On the traces plot the strip label sits *below* the previous panel's x-tick
  labels, so `run2` reads as an axis title for the panel above rather than a
  header for the one below.
- **Two typefaces fighting in every plot card.** The card header is Open Sans
  14.4 px/600; the ggplot title renders in ggplot's default sans at ~18–19 px
  and says nearly the same thing. The plot title is bigger and in a different
  face than the card title above it. Either drop the ggplot titles where a card
  header already says it, or set the theme's base family to match.
- **Bland-Altman aspect ratio is width-driven** — 1243×360 (3.5:1) at 1600 px,
  4:1 with the sidebar collapsed. A Bland-Altman plot's job is to let you see
  tilt, and stretching it horizontally **visually flattens the slope by an
  amount that depends on the reader's window width**. That is a
  measurement-communication bug, not decoration. Constrain the aspect ratio
  rather than letting height float free of width.
- **The line legend is incomplete and colour-ambiguous.** Mean difference and
  limits of agreement share one colour (solid vs dashed); the grey zero line is
  never mentioned in the subtitle. In the run2 panel the zero and mean lines sit
  ~5 W apart and read as a double-stroke artefact.
- **The run2 panel undermines itself.** The quarq's planted level-dependent bias
  is plainly visible as a cloud sitting +5 W to ~200 W then dropping to
  −10…−25 W, with a visible discontinuity — and a single mean line and single
  pair of limits are drawn straight across it, visually asserting something the
  data contradicts. Worth considering whether the LoA lines should be suppressed
  or annotated when `level_dependence()` (phase 3c) fires for that pair.
- On the traces plot the faint raw-power layers overplot: in run2 the quarq's
  alpha band almost entirely buries the assioma line beneath it. Z-order and
  alpha need attention.

### 5b. Bland-Altman density

At 1,800 points per panel on the demo and 3,600+ on a real hour,
`geom_point(alpha = 0.15)` is a smear.

```r
ggplot2::geom_bin2d(bins = 60) +
ggplot2::scale_fill_gradient(low = "#DEEBF7", high = source_palette()[[1]], transform = "log10")
```

**Clear the blocker first:** `chart_theme()` includes
`scale_fill_manual(values = source_palette())`, which would collide with a
continuous fill. No current plot maps `fill` at all, so it is dead code — delete
it and the collision never arises.

Apply unconditionally rather than switching geometry above some point count. A
plot whose meaning changes at a hidden threshold is worse in a measurement tool
than one that is slightly chunky at low n.

**This changes the report**, which calls the same `plot_agreement()`. That is why
it is last: if the report's Bland-Altman panel is the one you print and show
people, weigh that before doing it. No test — pinning plot output is the
characterisation testing CLAUDE.md argues against.

---

## Verification

```r
devtools::load_all("."); devtools::test()
invisible(parse("inst/shiny/app.R"))   # catches a stray quote in the theme block
launch_app()          # Load demo rides, walk every tab
compare_report(ride_spec_from_names(list.files(demo_rides(), full.names = TRUE)), seed = 1)
```

Then look at it in the browser — see "Verifying in the browser" above. Any
change to layout, card structure, plot sizing or table formatting needs eyes on
the rendered page; the test suite cannot see any of it.

The demo is the fastest exercise of everything: a clean 2.5% gain (kickr), a
level-dependent error (quarq), a 7-second offset and a chained pair, so every
new alert has a known right answer to check against.

## Recommended against

- **Darkening the muted guidance text.** Measured at `rgba(29,31,33,0.75)` on
  white ≈ **6.4:1**, comfortably past AA. The static review's instinct that it
  was hard to read was wrong: the problem is size and measure (0c), not colour.
  Spending effort here would fix nothing.
- **Switching the Bland-Altman geometry above a point-count threshold.** A plot
  whose meaning changes at a hidden threshold is worse in a measurement tool
  than one slightly chunky at low n.
- **`ExtendedTask` for loading.** Needs a promise-returning function and a
  `mirai`/`future` backend, neither in `Imports`, to buy responsiveness a single
  local user does not need. Blocking is honest here.
- **`updateActionButton(disabled=)` on a task button.** Not implemented in
  bslib's binding.
- **`withProgress()` around `rmarkdown::render()`.** Could only show an invented
  position.
- **Auto-advancing to Alignment after load.** Replaced by a link.
- **Collapsing Scan and Load.** Removes the label-correction step the editable
  table exists for.
- **A "min_power dropped X% of seconds" caveat.** Needs a package and report
  change to compute honestly.
- **`geom_hex()`.** `hexbin` is not installed.
