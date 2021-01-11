#' Class: pmtable
#'
#' Objects with class `pmtable` are return from the pmtable-provided data
#' summary functions:
#' - [pt_cont_wide()]
#' - [pt_cont_long()]
#' - [pt_cat_wide()]
#' - [pt_cat_long()]
#' - [pt_data_inventory()]
#'
#' The object is a list, with an item called `data`, which is the summarized
#' data set, as well as other items that are arguments for [stable()] or
#' [stable_long()].
#'
#' @examples
#' tab <- pt_cont_long(pmt_first, cols = "WT,AGE,SCR")
#'
#' str(tab)
#'
#' methods(class = "pmtable")
#'
#' @name class-pmtable
#' @rdname class-pmtable
NULL
