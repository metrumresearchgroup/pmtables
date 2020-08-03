#' Clear repeated values in a column
#'
#' @inheritParams stable
#' @param clear_reps character vector of column names where duplicate values will
#' be made blank (overwritten with `""`); ; see also [st_clear_reps()]
#' @param clear_grouped_reps passed to [deduplicate_values()] as `cols`
#' @param ... not used
#' @export
tab_clear_reps <- function(data = NULL, clear_reps = NULL, panel = rowpanel(NULL),
                           clear_grouped_reps = NULL, ...) {
  if(!is.null(clear_grouped_reps)) {
    xtra <- panel$col
    if(panel$null) xtra <- NULL
    data <- deduplicate_values(
      data,
      cols = clear_grouped_reps,
      extra_groups = xtra
    )
    return(data)
  }
  if(is.null(clear_reps)) return(data)
  assert_that(is.data.frame(data))
  dedup <- reps_to_clear(data, clear_reps, panel)
  for(dd in dedup) {
    data[[dd$col]] <- as.character(data[[dd$col]])
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

#' Clear grouped column values
#'
#' @param data data frame
#' @param cols columns to clear
#' @param extra_groups extra grouping variables
#'
#' @export
clear_grouped_values <- function(data, cols = groups(data), extra_groups = NULL) {
  cols <- new_names(cols)
  gr <- groups(data)
  data <- ungroup(data)
  rm_dup <- function(x) {
    ifelse(duplicated(chunk_runs(x)), "", x)
  }
  for(i in rev(seq_along(cols))) {
    data <- group_by(data, !!!syms(c(extra_groups, cols[seq(i)])))
    tcol <- cols[i]
    data <- mutate(
      data,
      !!sym(tcol) := rm_dup(!!sym(tcol))
    )
  }
  if(length(gr) > 0) {
    data <- group_by(!!!syms(gr))
  } else {
    data <- ungroup(data)
  }
  data
}
#' @rdname clear_grouped_values
#' @export
deduplicate_values <- clear_grouped_values

