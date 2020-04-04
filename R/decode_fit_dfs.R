#' read gps fit data files
#'
#' \code{decode_fit_dfs} reads a gps .fit files using call to python fitdecode
#'  and put into session, data record, and event dataframes
#'
#'
#' @param fitfilename name (including file path) of .fit file
#' @param appendSessionUnits if FALSE, do not append units to variable names
#'   in session dataframe
#' @param frames a vector of frames to return from 
#'   c("records","session","events","hrv")
#' @param checkconda logical, check conda environment for packages
#' @param requiredVars vector of variable names which will be generated as NA
#'   if they are not already in the fit data records
#'
#' @return a list of three dataframes:  \eqn{session},
#'     \eqn{records}and \eqn{events}
#'
#' @export
decode_fit_dfs <- function(fitfilename,
                           appendSessionUnits=TRUE,
                           frames=c("records","session","events"),
                           checkconda=TRUE,
                           requiredVars) {

  if (checkconda & !getOption("fitdecodeR.conda.checked")) {
    set_fitdecode_conda()
    options(list(fitdecodeR.conda.checked = TRUE))
  }
  #reticulate::use_condaenv("r-fitdecode", required = TRUE)
  pyfuncs <- paste(system.file(package = "fitdecodeR"),
                   "decodefitfile.py", sep = "/")
  suppressWarnings(reticulate::source_python(pyfuncs, convert = TRUE))

  if ("session" %in% frames) {
    session <- listofcols_to_tibble(suppressWarnings(
      message_df(fitfilename, outfile = NULL, msgtype = "session",
                 appendunits = appendSessionUnits, fromR = TRUE, missing="keep")),
      tuples="fixed")
    if (nrow(session) != 1) stop(paste0("session dataframe error, nrows=",nrow(session)))
  } else {
    session <- NA
  }
  if ("events" %in% frames) {
    events <- listofcols_to_tibble(suppressWarnings(
      message_df(fitfilename, outfile = NULL, msgtype = "event",
                 appendunits = TRUE, fromR = TRUE)),
      tuples="drop")
  } else {
    events <- NA
  }
  if ("records" %in% frames) {
    records <- listofcols_to_tibble(suppressWarnings(
      message_df(fitfilename, outfile = NULL, 
                 appendunits = TRUE, fromR = TRUE, missing="keep")),
      tuples="fixed")
  } else {
    records <- NA
  }
  if ("hrv" %in% frames) {
    hrv <- expand_tuple_to_tibble(suppressWarnings(
      message_df(fitfilename, outfile = NULL, msgtype = "hrv",
                 addlasttimestamp=TRUE, 
                 appendunits = TRUE, fromR = TRUE)))
  } else {
    hrv <- NA
  }
  
  if (!missing(requiredVars)) records <- addVars(records,requiredVars)

#session <<- session
#records <<- records
#events <<- events
#hrv <<- hrv

  return(list(session = session, records = records, events = events, hrv = hrv))
}

listofcols_to_tibble <- function(listofcolumns, tuples="fixed")  {


  cnames <- names(listofcolumns)
  vvv <- list()
  onames <- numeric(0)
  for (i in seq_along(listofcolumns)) {
    vlist <- listofcolumns[[i]]
    #  vlist should contain a list of python values or tuples
    if (length(unlist(vlist)) > 0) {
      maxTupleLength <- max(c(lengths(vlist),1))
      if (!lubridate::is.POSIXct(vlist) ) 
        vlist[lengths(vlist) == 0] <- list(rep(NA,maxTupleLength))
      
      if (maxTupleLength == 1) {
        vvv[[length(vvv) + 1]] <- unlist(vlist)
        onames <- c(onames,cnames[[i]])
      } else if (tuples == "fixed") {
        if (min(lengths(vlist)) != max(lengths(vlist)))
          stop("nonconstant tuple length ",cnames[i],".  min, max = ",
               min(lengths(vlist)),"  ",max(lengths(vlist)))
        vvec <- unlist(vlist)
        for (j in 1:maxTupleLength) {
          vvv[[length(vvv) + 1]] <- vvec[j + (seq(1,length(lengths(vlist)))-1)*maxTupleLength]
          onames <- c(onames,paste0(cnames[i],"_",j))
        }
      } else if (tuples != "none") {
        stop(paste0("unknown tuple option ",tuples))        
      }
    }
  }
  names(vvv) <- onames
  dfret <- as_tibble(vvv, stringsAsFactors = FALSE)
  #  the key timestamp variable should be POSIXct
  if ("timestamp" %in% cnames) {
    dfret$timestamp.s <- as.POSIXct(dfret$timestamp,
                                    format = "%Y-%m-%dT%H:%M:%OSZ",
                                    tz = "UTC")
  }
  return(dfret)
}

expand_tuple_to_tibble <- function(listofcolumns)  {

  if (length(listofcolumns) == 0) return(NA)
  if (length(listofcolumns) != 2) 
    stop("must have 2 columns when expanding tuples")
  len1 <- max(lengths(listofcolumns[[1]]))*min(lengths(listofcolumns[[1]]))
  len2 <- max(lengths(listofcolumns[[2]]))*min(lengths(listofcolumns[[2]]))
  if (len1 == 1) {
    nontuplecol <- 1
  } else if (len2 == 1) {
    nontuplecol <- 2
  } else  {
    stop("one column must be a non-tuple variable when expanding tuples")
  }
  tuplecol <- 3 - nontuplecol
  
  tuplelist <- listofcolumns[[tuplecol]]
  tuplelengths <- sapply(tuplelist, function(x) sum(!is.na(unlist(x))) )
  tupleexpand <- unlist(tuplelist)
  vvv <- list()
  vvv[[nontuplecol]] <- rep(unlist(listofcolumns[[nontuplecol]]),tuplelengths)
  vvv[[tuplecol]] <- tupleexpand[!is.na(tupleexpand)]
  names(vvv) <- names(listofcolumns)
  return(as_tibble(vvv, stringsAsFactors = FALSE))
}  

addVars <- function(df,varlist)  {
  for (v in varlist) {
    if (! v %in% names(df)) df[[v]] <- NA
  }
  return(df)
}

set_fitdecode_conda <- function()  {
  message("checking conda ")
  reticulate::use_condaenv("r-fitdecode", required = TRUE)
  #reticulate::py_install(c("pandas", "fitdecode"),pip=FALSE)
  #pyfuncs <- paste(system.file(package = "fitdecodeR"),
  #                 "decodefitfile.py", sep = "/")
  #reticulate::source_python(pyfuncs, convert = TRUE)
  message("done checking conda")
  invisible()
}
