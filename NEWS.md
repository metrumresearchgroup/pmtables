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

