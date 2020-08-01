#' Clear repeated values in a column
#'
#' @inheritParams stable
#' @param clear_reps character vector of column names where duplicate values will
#' be made blank (overwritten with `""`); ; see also [st_clear_reps()]
#' @param ... not used
#' @export
tab_clear_reps <- function(data = NULL, clear_reps = NULL, panel = rowpanel(NULL),...) {
  if(is.null(clear_reps)) return(data)
  assert_that(is.data.frame(data))
  dedup <- reps_to_clear(data, clear_reps, panel)
  for(dd in dedup) {
    if(!is.character(data[[dd$col]])) {
      data[[dd$col]] <- as.character(data[[dd$col]])
    }
    data[[dd$col]][dd$dup] <- rep("", dd$n)
  }
  data
}

reps_to_clear <- function(data, clear_reps, panel) {

  if(!is.character(clear_reps)) return(data)

  clear_reps <- new_names(clear_reps)

  for(col in clear_reps) require_col(data,col)

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
    ans[[this_col]] <- list(dup = dup, n = sum(dup), col = this_col)
  }

  ans
}
