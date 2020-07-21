
reps_to_clear <- function(data, clear_reps, panel) {

  if(!is.character(clear_reps)) return(data)

  clear_reps <- new_names(clear_reps)

  if(!panel$null) {
    paneln <- tidyselect::eval_select(panel$col, data = data)
    panelcol <- names(data)[paneln[1]]
    paneldat <- data[[paneln]]
  } else {
    paneldat <- NULL
  }

  ans <- vector("list", length(clear_reps))
  names(ans) <- clear_reps

  for(this_col in clear_reps) {
    run <- chunk_runs(paste(paneldat, as.character(data[[this_col]])))
    dup <- duplicated(run)
    ans[[this_col]] <- list(dup = dup, n = sum(!dup), col = this_col)
  }

  ans
}
