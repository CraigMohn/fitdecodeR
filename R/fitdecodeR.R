#' fitdecodeR: A package for reading garmin .fit files
#'
#' The fitdecodeR package uses reticulate to wrap
#'   the python fitdecode package, which seems to be kept more
#'   current than the native R solutions for processing .fit
#'   files
#'
#' @section read .fit file:
#'   \link{decode_fit_dfs}
#'
#' @import reticulate
#' @import tibble
#'
#' @name fitdecodeR
NULL

###  make the R checker happy
tedious <- utils::globalVariables(c("message_df"))
