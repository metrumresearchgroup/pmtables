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


