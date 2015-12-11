#' Fuzzy matcher
#'
#' @export
#' @param x Input species list, a character vector
#' @param ref Reference taxon data.frame, or file path
#' @param against (character) What column to match against in data.frame.
#' Ignored if a vector given
#' @param ... Further args passed on to \code{\link{grep}}
#' @examples
#' x <- c("Salmo eperlanus Linnaeus, 1758", 'Oncorhynchus clarkii', 'Salmo',
#' 'Oncorhynchus clarkii', 'Salvelinus fontinalis', 'Salvelinus confluentus')
#' y <- system.file("examples", "worms_salmo.csv", package = "splister")
#' dat <- unique(read.csv(y, stringsAsFactors = FALSE))
#'
#' # get exact matches
#' res <- match_exact(x, ref = dat, against = "scientificName")
#'
#' # then move on to do fuzzy matching
#' match_fuzzy(res)
#'
#' # pipe, if you like
#' match_exact(x, ref = dat, against = "scientificName") %>% match_fuzzy
#'
#' # read from file
#' x <- system.file("examples", "iucn_dat.csv", package = "splister")
#' x <- read.csv(x, stringsAsFactors = FALSE)[,-1]
#' y <- system.file("examples", "worms_sample.csv", package = "splister")
#' spp <- x$sciname[1:5000L]
#' (res <- match_exact(spp, ref = y, against = "scientificName"))
#' res2 <- match_fuzzy(res)
match_fuzzy <- function(x, ref = NULL, against = NULL, ...) {
  UseMethod("match_fuzzy")
}

#' @export
#' @rdname match_fuzzy
match_fuzzy.character <- function(x, ref = NULL, against = NULL, ...) {
  if (!is(ref, "data.frame")) ref <- readr::read_csv(ref)
  structure(unname(lapply(x, function(z) {
    ex <- ref[ref[[against]] %in% z, against]
    if (length(ex) == 0) {
      structure(list(z, NA), match = "no_exact_match")
    } else {
      structure(list(z, ex), match = "exact_match")
    }
  })), class = "splist", ref = what_ref(ref), against = against)
}

#' @export
#' @rdname match_fuzzy
match_fuzzy.splist <- function(x, ref = NULL, against = NULL, ...) {
  if (is.null(ref)) {
    sha <- attr(x, "refsha")
    if (is.null(sha)) stop("no reference data found", call. = FALSE)
    ref <- getout(sha)
  } else {
    if (!is(ref, "data.frame")) ref <- readr::read_csv(ref)
    sha <- get_sha(ref)
    putin(ref)
  }

  if (is.null(against)) against <- attr(x, "against")

  structure(unname(lapply(x, function(z) {
    if (attr(z, "match") %in% c('exact_match', 'replace')) {
      return(z)
    } else {
      df <- ref[grep(z[[1]], ref[[against]], ...), ]
      if (NROW(df) == 0) {
        structure(list(z[[1]], NA), match = c(attr(z, "match"), "no_fuzzy_match"))
      } else if (NROW(df) > 1) {
        ask(df, z[[1]], against)
      } else {
        structure(list(z[[1]], df[, against]), match = "replace")
      }
    }
  })), class = "splist", ref = what_ref(ref),
  refsha = sha, against = against)
}
