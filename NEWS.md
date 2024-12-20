# pmtables 0.8.1

## Bugs Fixed

- Fixed a bug in preview helpers (`st_as_image()` and `st2report()`) where 
  a list was getting passed as `.envir` to `glue::glue()` rather than an 
  environment; this was causing `glue::glue()` to fail after changes rolled out 
  in glue version 1.8.0 (#346).

# pmtables 0.8.0

- New functionality to include output file path information in the table 
  annotation (#341).

# pmtables 0.7.0

- New functionality to create table notes from glossary files (#326). 
  - `read_glossary()` reads from `.tex` or `.yaml` formatted files, returning a
    glossary object
  - `select_glossary()` selects specific entries from a glossary object
  - `as_glossary()` creates a glossary object on the fly or from a list
  - `glossary_notes()` creates notes from a glossary object, a list, or the 
    name of a glossary file
  - `st_notes_glo()` creates and and attaches notes to a table in a pipeline
- `stable_save_image()` added to create an image from a table and save to a 
  specific location; this builds on `st_aspdf()` and `st_aspng()`, adding 
  convenient syntax and options (#333). 
- The `maxex` argument to `sig()` can now be set through the `pmtables.maxex` 
  option (#328). 

## Bugs Fixed

- Fixed a bug in `pt_cat_long()` when the `by` argument is used with no all 
  data summary (#330).

# pmtables 0.6.0

- `st_caption()` function added to associate a caption (including short 
  caption and possibly a label) to any table; captions will appear in 
  `st2report()` outputs; captions will always be written to file for 
  `stable_long` outputs and can be optionally written to file for `stable` 
  outputs (#313).

- `yaml_as_df()` will now retain row names in a column named by the new argument
  `row_var` (#317).

- `st_align()`, `st_left()`, `st_right()` and `st_center()` can now be called
  multiple times in a pipeline, supported by new update method for `aligncol`
  objects (#315). 

- `st_clone()` function added to make a copy of `stobject` objects (#314).

- The `id_col` argument to `pt_cont_long()` and `pt_cont_wide()` is 
  deprecated (#305).
  
## Bugs Fixed

- Fixed bug where `sumrows` was not recognized as a valid field in `stobject`
  objects (#312).

# pmtables 0.5.2

- pmtables has been updated to be compatible with stringr 1.5.0, which
  changed the classes of pattern modifiers (#302).

# pmtables 0.5.1

- New function `st_filter()` to filter data item in a pipeline (#298).

- Add `summarize_all` and `all_name_stacked` arguments to 
  `pt_data_inventory()` (#297).

## Bugs Fixed

- Fixed bug where `all_name` was not getting used in `pt_data_inventory()` 
  (#297).

- Fixed bug where detached table notes were getting rendered too close to the 
  main table when building standalone pdf under certain TeX distributions
  (#286).

# pmtables 0.5.0

- New functions `st_as_image()`, `st2pdf()`, and `st2png()` to render tables with 
  TeX to either `pdf` or `png` format; image files may be kept on disk
  or read back for display while knitting; `pdflatex` system dependency for 
  `pdf` images and `latex` + `dvipng` for `png` images; additional Suggested
  packages include `magick` and `pdftools` (#277, #278). 
  
- `sig()` now returns character when integer type is passed (#272). 

- `st_new()` is now generic with dispatch for `data.frame` and objects with 
  class `pmtable`; most pipeline functions can now be used to customize
  tables coming from `pt_cat_*`, `pt_cont_*`, `pt_demographics()` and 
  `pt_data_inventory()` (#274).
  
- New functions `st_notes_detach()`, `st_notes_rm()`, `st_notes_app()`, 
  `st_notes_str()`, `st_notes_sub()` and `st_notes_conf()` to help working 
  with table notes in a pipe context (#274).
  
- `st_span()` and `st_span_split()` accept `align` argument to push column 
  spanner titles to the left, right or center (default); the argument eventually
  gets passed to `colgroup()` (#261).

- `pt_cat_wide()`, `pt_cat_long()` and `pt_demographics()` gain argument 
  `denom` to alter the denominator when calculating percents for categorical
  data summaries (#268).

- `pt_cat_wide()` gains argument `complete` to display missing levels of 
  `by` and `panel` (#268).

- Put stories in yaml format; add script to build validation docs from the
  yaml file (#269, #270).


# pmtables 0.4.1

- `colgroup()` (and `st_span()`) gains an `align` argument to position
  the spanner title on left or right in addition to the center (default)
  #260, #261.

- `rowpanel()` (and `st_panel()`) gains `jut`argument to push non-panel
  table contents to the right relative to the panel header row so that
  contents under the panel header are indented #251, #253. 

- Panel header rows are now modified so that the header row stays
  with the first non-header row for longtable output #252, #253.

- Consistent `BQL` / `BLQ` handling for column titles and table 
  notes for `pt_data_inventory()` #254, #255. 

# pmtables 0.4.0

- Add `cols_omit` option to omit column header data  (#213)

- Add `pt_demographics` function to generate a new table of both continuous and
  categorical data (#186, #249)

- Add `title_side` argument to `colsplit()` so that the title can be taken from
  left or right side of split (#231)

- Add `hline` argument to `rowgroup()` constructor to make the horizontal line
  above the panel data optional (#215)

- Refactor `pt_data_inventory()` to calculate percent BLQ using denominator
  that is the sum of the number of observations BLQ and non-BLQ / non-missing
  (#221, #222)

- `st_asis()` gains a method for `pmtable` objects (#236)

- List names are now escaped when passing a list of tables to `st2report()` 
  and friends (#232)

- Add newline after printing table text using `st_asis()` (#224)

# pmtables 0.3.3

- Fix bug where arguments could not be passed along to `stable_long()` 
  when coercing pmtable output (#203)

- Fix bug where user-specified font size was not getting propagated to 
  long table output with `stable_long()` (#204)
  

# pmtables 0.3.2

- Fix cols renaming and utilization of table argument in pt_cont_wide; add tests
  for column renaming for cat / cont / long / wide #199
- Fix error message when duplicate panels are found #198

# pmtables 0.3.1 

- Add `.list` argument to `st_rename()` allowing user to pass rename information
  as a named list with format `old name = new name` #189
- Fix bug where `drop_miss` wasn't executing correctly #182
- `stable_long()` is now a generic function with methods for `data.frame`, 
  `stobject` and `pmtable` #179
- Expand documentation for data summary functions
- `st_span()` was slightly refactored so that it would dispatch to
  `st_span_split()` when the `split` argument was passed #173
- Fixed a bug where `span_split()` was updating column names, but this was 
  happening before other spans were calculated #172
- The categorical data summary functions now check for missing values (`NA`)
  and will replace them with the string `"NA"` and issue a warning #117
  
# pmtables 0.3.0

- Add landscape mode when previewing tables with `st2report()`, `st2article()`
  or `st2doc()`; see `landscape` argument to `st_wrap()` #129
- Add `lt_cap_short` argument to `stable_long()` to set a short table title 
  that will appear in list of tables #131
- Fix a bug in continuous data summary tables when no observations were found 
  in a particular group; use `na_fill` argument to set placeholder #133
- Add ability to accumulate `hline` indices when set with `st_hline()` #134
- Fix a bug in `cols_bold` that prevented sanitizing text prior to styling as 
  bold #141
- Add ability to introduce row breaks in spanner titles #143
- Add `st2report()` to quickly preview tables in a real-like report document, 
  build directly with `pdflatex`  #148
- Add `st2article()` to quickly preview tables in a latex `article`-like, 
  built directly with `pdflatex` #148
- Add `st_asis()` function and `asis` argument to `st_wrap()`; pipe tables to 
  `st_asis()` or use `asis` argument to render tables in line in an Rmd
  document; in addition to formatting output "as-is", latex dependencies are 
  also invoked #149
- Expand `yaml_as_df()` functionality to accept table columns by position, 
  rather than name #154
- Add `stable()` method for `stobject` #159
- Fix bug where column spanners were not getting displayed in longtable #165

# pmtables 0.2.0
- Add `by` argument to `pt_cont_long` so that, when `panel` is also 
  passed, the table is paneled by `col` and is "by" the `panel` variable #92
- Refactor `pt_cat_long` so that summary numbers are shown underneath column 
  labels #102
- Refactor longtable row and column spacing so that they use `row` and `col` 
  in `tab_size()` with the same behavior as tabular / `stable()` #105
- Add `cols_extra` feature, allowing extra column header information passed
  as a data frame (see `tab_cols()`) #102, #118
- Added documentation for `data` arguments to reinforce that pmtables doesn't
  filter or subset `data` prior to processing or summarizing #120
- Fix bug in `stable_long()` where `stable_file` wasn't getting saved
- Add `n` summary column to `pt_cat_wide()` #80
- Add functions that return table notes for continuous and discrete data 
  summaries and data inventory tables #114
- Make `pt_wrap()` generic; add method for `stable_long` that doesn't wrap 
  with table #121

# pmtables 0.1.0
- Adding NEWS file
- Add `duplicates_ok` argument to rowpanel
- Error if duplicate panel names found and `duplicates_ok` is FALSE
- Refactor how panel groupings are determined - non-repeating values
  rather than unique
- Added debug_data argument to stable; this adds an environment to the output
  with some data that underlies the table; the environment can be accessed with 
  `get_debug_data()`
- Added complete series of `st_*` functions that can be used in a pipeline to set arguments 
  for `stable()`; see `inst/demo-pipe.pdf`
- `st2doc` will render multiple page document when a list of tables is passed in 
  as text
- Changed `stable` argument from `bold_cols` to `col_bold`
- Column names are only rendered with bold font when requested using `col_bold` argument
- The prefixes for `r_file` and `output_file` are only settable with options `r.file.label`
and `output.file.label`, respectively
- Dropped `col_space`, `row_space` and `fontsize` arguments to `stable()`; they should be set 
with a call to `tab_size`
- Add `st_sizes()` to set `sizes` argument to `stable()`
- Refactor `stable()` function in order to reduce cyclomatic complexity and shorten overall function
length in response to brief review and comment by TS representative
- Make stable a generic function; methods for data.frame and pmtable
- All pmtables functions now accept `panel` argument as a rowgroup object
- Add `as.panel()` function for constructing rowgroup objects
- For pmtables functions: when `panel` is passed with an alternate name, 
the name is is used to set the prefix
- Fixed bug where column names were getting sanitized before rename happened #62
- Pick up option `pmtables.dir` to set output directory for `stable_save()`
- Pick up option `mrg.script` to set script name in `st_files()` and `tab_notes()`
- Refactored the way that table cols and units are assembled; column labels are 
split on `...` to create multi-line column names
- Add `header_row` argument to `tab_cols()` to control the spacing between column labels
and between labels and units
- Fix warning message when trying to align a column not in the data set
- Fix warning message when trying to add unit to column not in the data set
- Add several new package options; see `?pt_opts`
- Add `center` argument to `col_fixed`; when `ragged` is `no`, the column can still be 
fixed and centered
- Dots (`...`) added to `st_new()` / `st_data()` to be collected by args
- Add `st_units()` pipeline function
- Add `tex_bold`() function to style data frame contents
- Add `tex_it()` function to style data frame contents
- Add `st_edit()`
- Refactor `st_hline()`; allow to add hline by regexp across whole data frame 
or targeted
- Refactored `st_bold` and `st_it` to target columns or whole data frame
- Add `st_drop()`
- Add data sets `analysis1`, `pmt_pk`, `pmt_first`, `pmt_obs`
- Change argument names for `tab_cols`: all `col_xyz` arguments
are renamed to `cols_xyz`
- Add `find_bq_col` function to allow either `BQL` or `BLQ` as default value 
for `bq_col` argument to `pt_data_inventory`
- Add `pattern` and `cols` arguments to `st_sumrow`; this allows calculation of
`rows` based on a regular expression
- Add `df_grep_rows` and `df_grepl_rows` functions for finding rows where a regular 
expression is matched
- Add `pmtables.escape` option to pass a character vector of items to be 
escaped
- Unit of escape is now individual positions in a vector rather than the 
whole vector itself
- No longer requiring or assuming that `header_row` (`tab_size()`) is a negative
number; enter a negative number if you want to remove space between header rows;
this should only be required when row space is increased


