# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```sh
make doc         # Regenerate man/, NAMESPACE, and DESCRIPTION Collate (devtools::document())
make install     # R CMD INSTALL .
make test        # Install package then run full test suite
make check       # doc + build + R CMD CHECK
make quick       # Fast check without vignettes/tests/manual
make readme      # Render README.Rmd -> README.md
make data        # Rebuild /data from data-raw/data.R
make demo-doc    # Render the inst/demo-*.Rmd|R docs to PDF (needs LaTeX)
make demo-check  # Run only tests/testthat/test-demo-check.R
make spelling    # spelling::spell_check_package() against inst/WORDLIST
```

Run a single test file (from `tests/testthat/`, since some tests read relative paths):
```r
testthat::test_file("tests/testthat/test-<name>.R")
```

Run tests matching a pattern:
```r
testthat::test_local(filter = "pattern")
```

`make doc` is required after touching roxygen blocks: `NAMESPACE`, `man/`, and the
`Collate:` field in `DESCRIPTION` are all generated. Collate order matters — files
that must load early (`class-digits.R`, `class-new_names.R`, `utils.R`,
`summary-functions.R`) are pinned by `@include` directives in `R/AAAA.R`.

## Architecture

pmtables generates LaTeX-formatted summary tables for pharmacometrics workflows.
Output is a character vector of `tex` lines (class `stable`), meant to be pasted
into a `tex` document or emitted from an Rmd chunk with `results = "asis"` via
`pt_wrap()` / `st_wrap()`.

**Two entry paths, one renderer:**
1. Raw data → `pt_*()` summarization → `pmtable` (a list: `data` slot + slots that
   are literally `stable()` arguments — `panel`, `span`, `units`, `notes`, …)
2. `pmtable` or data frame → `st_new()` → `stobject` → chained `st_*()` calls →
   `st_make()`
3. Both converge on `stable()` / `stable_long()`, which do all the tex assembly.

`st_make()` simply collects the stobject's bindings into a list and `do.call`s
`stable()`. So every `st_*()` function is a setter for a `stable()` argument; adding
a new configuration knob usually means adding the argument to `stable.data.frame()`
(or a `tab_*()` helper it forwards `...` to) *and* a matching `st_*()` setter plus an
entry in `st_arg_names` (`R/table-object.R`).

**`stable.data.frame()` is the pipeline** (`R/table-stable.R`). Order of operations
matters and is load-bearing: triage → hlines → clear reps → drop → panel → units →
sumrows → spanners → column headers → align → `make_tabular()` (escaping happens
here) → hlines inserted → panel rows inserted → header rows → notes → wrap in
`threeparttable`. Many arguments are not in the `stable()` signature at all; they
ride in `...` and are consumed by `tab_hlines()`, `tab_spanners()`, `tab_notes()`,
`tab_clear_reps()`, `make_tabular()`, `tab_cols()`, `tab_size()`.
`stable_argument_names()` enumerates that union.

Pass `inspect = TRUE` to `stable()` to attach a `stable_data` attribute holding every
intermediate (data, header rows, spans, align, notes, sizes) — the fastest way to
debug a rendering problem without reading tex.

**`stobject` has reference semantics.** It is an `environment`, and `st_*()`
functions mutate it in place and return it; assigning to a new variable does not
copy. Use `st_clone()` when a caller needs an independent copy.

**Main public API:**
- `pt_cont_wide()` / `pt_cont_long()` — continuous covariate summaries
- `pt_cat_wide()` / `pt_cat_long()` — categorical/discrete summaries
- `pt_data_inventory()` — observation and individual counts
- `pt_demographics()` — mixed continuous/discrete demographics
- `stable()` / `stable_long()` — render a data frame directly
- `st_*()` — pipe-friendly configuration (align, span, notes, hlines, sizes, panel,
  units, sumrow, clear_reps, glossary)
- `st_preview()` / `st2report()` / `st2article()` — render tex to PDF/image; these
  need a working LaTeX install plus `texPreview`, and are `# nocov` in tests

**Key S3 classes:** `pmtable`, `stobject` (+ `ptobject` when built from a `pmtable`;
several `st_*()` functions refuse to operate on those via `stop_if_ptobject()`),
`stable`, `digits`, `new_names`, `rowpanel`, `glossary`.

**File naming:**
- `class-*.R` — S3 class definitions
- `table-*.R` — rendering and styling machinery
- `continuous_table.R`, `discrete_table.R`, `data_inventory_table.R`,
  `demographics-table.R` — the `pt_*()` summarizers
- `tab_*()` — internal helpers called from `stable()`; not part of the pipe API
- `check.R` — input validation helpers (`check_exists()`, `check_continuous()`, …)
  that accumulate messages and stop once with all problems

**Data prep:** `triage_data()` (ungroup, factors → character) then `tab_prime()`
(escape + bracket masking) normalize input before tabular assembly.

**LaTeX sanitization:** `%` and `_` in values are escaped by `tab_escape()`; `~`,
`>`, `<` are converted to math forms. A value containing `$...$` or a `\\`-escaped
character is left alone. `options(pmtables.escape = NULL)` disables it. See
`inst/demo-sanitize.Rmd` for the full contract.

**Options** read at runtime: `pmtables.escape`, `pmtables.dir`, `pmtables.path.type`,
`pmtables.big.mark`, `pmtables.textwidth`, `pmtables.maxex`, `pmtables.image.*`.

**Generated tex assumes a preamble** with `threeparttable`, `booktabs`, `longtable`,
and `array` — see `inst/tex/` and the demo Rmd YAML headers.

**Sample datasets** (`/data/`): `pmt_first`, `pmt_obs`, `pmt_pk`, `pmt_summarized`,
`analysis1`. Sources in `/data-raw/`, worked examples in `inst/demo-*.Rmd`.

## Testing

Test IDs (bracketed tags at the end of the `test_that()` description, e.g.
`[PMT-TEST-0248]`) were used historically but are no longer in use. Do not put test
IDs into new tests.

`tests/testthat/test-expected.R` compares rendered output byte-for-byte against
golden files in `tests/testthat/validate/*.tex`. Those files are generated by
knitting `tests/testthat/validate/validate.Rmd` (which also produces `validate.pdf`).
If a change legitimately alters output, re-knit that Rmd, **visually check the PDF**,
and commit the regenerated tex — that manual check is the convention in this repo's
history.

## Repo conventions

PRs target `develop`. `NEWS.md` gets an entry per user-visible change.
