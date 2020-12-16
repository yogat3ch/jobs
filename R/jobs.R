#' @title Write a code chunk to file
#' @description Write the \code{code} provided to a file indicated by \code{filename} in the directory indicated by \code{dir}
#' @param code \code{expression} to be written to file. Either one created with \link[rlang]{expr}, or provided wrapped in \code{\{\}}
#' @param filename \code{character} filename
#' @param dir \code{character} vector of directory path(s)
#' @inheritParams rstudioapi::jobRunScript
#' @inheritDotParams rstudioapi::jobRunScript
#' @return \code{character} the path to the file invisibly
#' @importFrom rstudioapi jobRunScript
#' @importFrom rlang enexpr
#' @examples
#' jobscript({
#' # long running code
#' Sys.sleep(10)
#' out <- "I returned"
#' }, filename = "ex.R", exportEnv = "R_GlobalEnv")
#' file.remove("ex.R")
#' @export


jobscript <- function(code, filename, dir = NULL, workingDir = getwd(), ...) {
  # Check to see if it's an expression
  .code <- rlang::enexpr(code)
  if ("{" == deparse(.code[[1]])) code <- .code
  if (!is.null(dir)) {
    stopifnot(is.character(dir))
    if (!dir.exists(dir)) dir.create(dir)
  }
  if (missing(filename)) {
    fp <- tempfile("job", fileext = ".R")
  } else {
    stopifnot(is.character(filename))
    fp <- file.path(ifelse(is.null(dir), "", dir), ifelse(grepl("\\.R$", filename), filename, paste0(filename, ".R")))
  }
  if (!file.exists(fp)) file.create(fp)


  write(deparse(code), fp)
  rstudioapi::jobRunScript(fp, name = basename(fp), workingDir = workingDir, ...)
  invisible(fp)
}

