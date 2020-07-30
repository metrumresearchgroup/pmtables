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

