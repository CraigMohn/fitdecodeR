
.onLoad <- function(libname, pkgname) {

  #  set flag that we are loaded, not called yet
  op <- options()
  op.fitdecodeR <- list(
    fitdecodeR.conda.checked = FALSE
  )
  toset <- !(names(op.fitdecodeR) %in% names(op))
  if (any(toset)) options(op.fitdecodeR[toset])

  invisible()
}
