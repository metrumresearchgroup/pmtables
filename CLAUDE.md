# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```sh
make doc        # Regenerate documentation via roxygen2 (devtools::document())
make install    # R CMD INSTALL .
make test       # Install package then run full test suite
make check      # doc + build + R CMD CHECK
make quick      # Fast check without vignettes/tests/manual
make readme     # Render README.Rmd
```

To run a single test file:
```r
testthat::test_file("tests/testthat/test-<name>.R")
```

To run tests matching a pattern:
```r
testthat::test_local(filter = "pattern")
```

## Architecture

pmtables is an R package that generates LaTeX-formatted summary tables for pharmacometrics workflows.

**Data flow:**
1. Raw data → `pt_*()` summarization functions → `pmtable` object (list with `data` slot + metadata)
2. `pmtable` → `st_new()` → `stobject` (an environment collecting table settings)
3. Chain `st_*()` functions to configure styling, spans, notes, alignment, etc.
4. `st_make()` or `stable()` renders the final LaTeX string

**Main public API families:**
- `pt_cont_wide()` / `pt_cont_long()` — continuous variable summaries
- `pt_cat_wide()` / `pt_cat_long()` — categorical/discrete summaries
- `pt_data_inventory()` — observation and individual counts
- `pt_demographics()` — mixed continuous/discrete demographics
- `stable()` / `stable_long()` — render a data frame directly to LaTeX tabular
- `st_*()` functions — pipe-friendly table configuration (alignment, spans, notes, hlines, sizes, panels)

**Key classes** (all S3):
- `pmtable` — result from `pt_*()` functions; holds `data` plus configuration slots
- `stobject` — wraps an environment; built by `st_new()`, mutated by `st_*()` functions
- `digits` — controls numeric formatting
- `new_names` — variable renaming helper

**File naming conventions:**
- `class-*.R` — S3 class definitions
- `table-*.R` — table styling utilities (align, cols, span, sumrow, etc.)
- `pt_*.R` or files named after the summary type — public summarization functions
- `tab_*.R` or `tab_*()` — lower-level table configuration helpers called internally

**Data preparation:** `triage_data()` and `tab_prime()` normalize input before rendering (ungroup, convert factors to character).

**LaTeX sanitization:** `%` and `_` in data values are escaped automatically. Wrap content in `$...$` or prefix with `\\` to prevent sanitization.

**Sample datasets** `pmt_first` and `pmt_obs` (in `/data/`) are used throughout tests and demos. Source in `/data-raw/`, demos in `/inst/`.

**Test naming:** test IDs follow the pattern `[PMT-TEST-####]` at the end of `test_that()` descriptions.
