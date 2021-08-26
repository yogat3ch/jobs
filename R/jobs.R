#' @title Run code as RStudio background task
#' @description Write the code provided to \code{code} or copied to the clipboard to a file indicated by \code{filename} in the directory indicated by \code{dir} and run it as a background task in RStudio
#' @param code \code{expression} to be written to file. Either one created with \link[rlang]{expr}, or provided wrapped in \code{\{\}}. If no code is provided, the clipboard will be read.
#' @param filename \code{character} filename
#' @param dir \code{character} vector of directories
#' @inheritParams rstudioapi::jobRunScript
#' @inheritDotParams rstudioapi::jobRunScript
#' @return \code{character} the path to the file invisibly and once the task completes, any code results (as would be returned if using the background tasks feature directly)
#' @importFrom clipr read_clip
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


jobscript <- function(code, filename, dir = NULL, workingDir = getwd(), exportEnv = .GlobalEnv, importEnv = TRUE, ...) {
  if (missing(code)) {
    code <- clipr::read_clip()
  } else {
    # Check to see if it's an expression
    code <- deparse(rlang::enexpr(code))
    if (code[1] == "{")
      code <- code[-c(1, length(code))]
  }

  if (!is.null(dir)) {
    stopifnot(is.character(dir))
    if (!dir.exists(dir)) dir.create(dir)
  }

  if (missing(filename)) {
    fp <- tempfile("job", fileext = ".R")
  } else {
    stopifnot(is.character(filename))
    fp <- do.call(file.path,
                  list(if (!is.null(dir))
                      dir
                    else
                      workingDir,
                    ifelse(
                      grepl("\\.R$", filename), filename, paste0(filename, ".R")
                    )))

  }

  if (!file.exists(fp)) file.create(fp)

  write(code, fp)
  rstudioapi::jobRunScript(fp, workingDir = workingDir, importEnv = importEnv, exportEnv = exportEnv, ...)
  invisible(fp)
}

#' @title Install all packages on clipboard
#' @description install all packages in the package list rendered by \link[devtools]{install_github} on the clipboard
#' @export

install_all <- function() {
  pkgs <- clipr::read_clip()
  pkgs <- stringr::str_extract(pkgs, "(?<=\\d{1,2}\\:\\s)[\\w\\.]+")
  exp <- rlang::expr(install.packages(!!dput(pkgs)))
  rlang::eval_bare(rlang::call2(jobs::jobscript, !!!list(exp)))
}

