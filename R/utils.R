emessage <- function(x) message(paste0("[pmtables] ", x))

cvec_cs <- function(x) {
  if(is.null(x) | length(x)==0) return(character(0))
  if(!is.null(names(x))) return(x)
  x <- unlist(strsplit(as.character(x),",",fixed=TRUE),use.names=FALSE)
  x <- unlist(strsplit(x," ",fixed=TRUE),use.names=FALSE)
  x <- x[x!=""]
  if(length(x)==0) {
    return(character(0))
  } else {
    return(x)
  }
}

#' Format digits
#'
#' Use [sig()] to set the number of significant digits; use [digit1()] to limit
#' to one digit. See examples.
#'
#' @details
#' When `x` is an integer, `x` is returned after coercing to character, without
#' further processing.
#'
#' @param x `numeric`; value to manipulate.
#' @param digits `numeric`; number of significant digits.
#' @param maxex `numeric`; maximum number of significant
#' digits before moving to scientific notation.
#' @param ... other arguments that are not used.
#'
#' @return
#' A character vector of formatted values.
#'
#' @examples
#' sig(1.123455)
#' sig(0.123455)
#' sig(1.123455, digits = 5)
#' sig(1123, maxex = 3)
#' sig(1123, maxex = 4)
#'
#' sig(1L)
#'
#' digit1(1.234)
#' digit1(1321.123)
#'
#' @md
#' @rdname sig
#' @export
sig <- function(x, digits = 3, maxex = getOption("pmtables.maxex", NULL), ...) {

  if(identical(class(x), "integer")) {
    return(as.character(x))
  }

  namez <- names(x)

  x <- as.numeric(x)
  x <- formatC(signif(x,digits=digits), digits=digits, format='g', flag='#')

  if(is.numeric(maxex)) {
    if(digits!=maxex) {
      ex <- "([-]*[0-9]\\.[0-9]+)e([+-][0-9]{2})"
      subit <- grepl(ex,x,perl=TRUE)
      b <- as.numeric(gsub(ex, "\\2", x))
      subit <- subit & abs(b) < maxex
      x <- ifelse(subit,formatC(signif(as.numeric(x),digits=digits),digits=digits, format="fg",flag="#"),x)
    }
  }
  x <- gsub("\\.$", "", x, perl=TRUE)
  names(x) <- namez
  return(x)
}

#' @rdname sig
#' @export
digit1 <- function(x, ...) formatC(x, digits = 1,format = 'f')

#' @rdname sig
#' @export
rnd <- function(x, digits = 0, ...) round(x, digits)

data_total_col <- function(data,all_name="all") {
  if(!exists(".total",data)) {
    data[[".total"]] <- all_name
  }
  return(data)
}

#' Alias to `dplyr::vars`
#'
#' @param ... passed to [dplyr::vars]
#'
#' @export
.cols <- function(...) { # nocov start
  stop("this function is deprecated; use `dplyr::vars` instead")
} # nocov end

combine_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  left[names(right)] <-  right
  left
}

Update_List <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  common <- intersect(names(left), names(right))
  left[common] <-  right[common]
  left
}

is_regex <- function(x) {
  if(!is.character(x)) return(FALSE)
  x <- suppressWarnings(try(grep(x, "abcde"),silent = TRUE))
  !inherits(x, "try-error")
}

is_str_regex <- function(x) {
  if(!is.character(x)) return(FALSE)
  is_regex(x) ||
    (inherits(x, "stringr_fixed") && inherits(x, "stringr_pattern")) ||
    # stringr < v1.5.0
    (inherits(x, "fixed") && inherits(x, "pattern"))
}

as_str_regex <- function(x) {
  if(!is.character(x)) return(fixed(basename(tempfile("invalid-regex-"))))
  if(!is_str_regex(x)) {
    return(fixed(x))
  }
  x
}

repattern_df <- function(data, pattern, warn = TRUE, context = NULL) {
  data <- select(data, intersect(names(data), names(pattern)))
  if(ncol(data)==0) return(data[0,0])
  assertthat::assert_that(ncol(pattern) > 0)
  assertthat::assert_that(nrow(pattern) > 0)
  if(ncol(data) == 0) {
    if(isTRUE(warn)) {
      if(is.character(context)) {
        message("context: ", context)
      }
      warning("could not repattern data frame", call.=FALSE)
    }
    return(data[0,0])
  }
  combined <- bind_rows(slice(pattern,1), data)
  slice(combined, seq(2, nrow(combined)))
}

#' Add parens if not found
#'
#' Opening and closing parens will be added if an opening paren is not the
#' first non-whitespace character.
#'
#' @param x a list or vector
#'
#' @return
#' `x` is returned, possibly modified with parens added.
#'
#' @examples
#' ensure_parens(letters[1:4])
#'
#' ensure_parens(as.list(letters[1:4]))
#'
#' ensure_parens(c("(a)", "b", "(c)"))
#'
#' @export
ensure_parens <- function(x) {
  where <- !sapply(x, grepl, pattern = "^\\s*\\(")
  x[where] <- paste0("(", x[where], ")")
  x
}

#' Format the path to output file
#'
#' @param file the name of the output file.
#' @param dir the directory where the output file is stored.
#' @param path.type a character string indicating how the path to the output
#' file should be formatted; use `"proj"` to have the path expressed relative
#' to an RStudio project file; use `"none"` to format without any directory
#' information (just the output file name); use `"raw"` to print the complete
#' path to the file using `dir` as-is.
#'
#' @details
#' Note that the default value for `path.type` is `"proj"`. This requires
#' that an RStudio project file is able to be found using
#' [rprojroot::find_root()] with the [rprojroot::is_rstudio_project] criterion.
#'
#' Once mrggsave finds a root for a given working directory, it caches the value
#' for the remainder of the R session. An error will be generated if an image is
#' attempted to be saved using `path.type="proj"` but an RStudio project file
#' was not able to be located.
#'
#' @return
#' A string with the formatted image file path.
#'
#' @examples
#' \dontrun{
#' format_table_path("foo.tex", "my/path")
#' format_table_path("foo.tex", "my/path", path.type = "proj")
#' format_table_path("foo.tx", "my/path", path.type = "raw")
#' }
#'
#' @md
#' @export
format_table_path <- function(file, dir = NULL, path.type = c("proj", "none", "raw")) {

  if(dirname(file) != ".") {
    dir <- dirname(file)
    file <- basename(file)
  }

  path.type <- match.arg(path.type)

  if(is.null(dir)) path.type <- "none"

  if (path.type == "proj") {
    root <- find_cached_root()
    if (is.null(root)) {
      stop("No RStudio project root found for ", getwd())
    }

    if (!fs::path_has_parent(dir, root)) {
      stop("dir is not under root\n",
           paste("dir: ", dir, "\n"),
           paste("root:", root))
    }

    return(path(path_rel(dir, start = root), file))
  }

  if(path.type=="raw") {
    return(path(dir, file))
  }

  return(basename(file))
}
