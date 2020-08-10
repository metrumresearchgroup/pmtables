# pmtables (development version)
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
