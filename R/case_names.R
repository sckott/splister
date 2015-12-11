#' Change case
#'
#' @param x Input species list, a character vector
#' @return Same as put in, either list, character vector or data.frame
#' @examples
#' x <- c("Salmo eperlanus Linnaeus, 1758", 'Oncorhynchus clarkii', 'Salmo',
#' 'Oncorhynchus clarkii', 'Salvelinus fontinalis', 'Salvelinus confluentus')
#'
#' # capitalize first letter only
#' case_names(x)
#'
#' # all lower case
#' case_names(x, 'lower')
#'
#' # all upper case
#' case_names(x, 'upper')
#'
#' # trim white space
#' x <- c("Salmo eperlanus Linnaeus, 1758  ", 'Oncorhynchus clarkii', 'Salmo ',
#' 'Oncorhynchus clarkii ', '   Salvelinus fontinalis ', 'Salvelinus confluentus  ')
#' case_names(x, 'trim')
#'
case_names <- function(x, what = "onlyfirst") {
  UseMethod("case_names")
}

#' @export
#' @rdname pull_names
case_names.character <- function(x, what = "onlyfirst") {
  switch(what,
    onlyfirst = lets_capwords(x, onlyfirst = TRUE),
    trim = strtrimm(x),
    lower = tolower(x),
    upper = toupper(x)
  )
}

#' @export
#' @rdname pull_names
case_names.splist <- function(x, what = "onlyfirst") {
  switch(what,
    onlyfirst = lets_capwords(x, onlyfirst = TRUE)
  )
}

lets_capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s,1,1)), {
      s <- substring(s,2); if (strict) tolower(s) else s
    }, sep = "", collapse = " " )
  }

  if (!onlyfirst) {
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  } else {
    sapply(s, function(x) {
      xx <- strsplit(substring(x,2), "\\s")[[1]]
      rest <- paste0(na.omit(xx[1:2]), collapse = " ")
      auth <- if (length(xx) > 2) paste0(xx[3:length(xx)], collapse = " ") else ""
      strtrimm(paste(toupper(substring(x,1,1)),
            tolower(rest),
            " ",
            auth,
            sep = "", collapse = " "))
    }, USE.NAMES = FALSE)
  }
}

strtrimm <- function(str) gsub("^\\s+|\\s+$", "", str)
