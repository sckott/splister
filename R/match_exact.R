#' Exact matcher
#'
#' @export
#' @param x Input species list, a character vector
#' @param ref Reference taxon data.frame, or file path
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
#' match_fuzzy(x, dat, against = "scientificName")
match_exact <- function(x, ref = NULL, against = NULL) {
  UseMethod("match_exact")
}

#' @export
#' @rdname match_exact
match_exact.character <- function(x, ref = NULL, against = NULL) {
  if (is.null(ref)) {
    sha <- attr(x, "refsha")
    if (is.null(sha)) stop("no reference data found", call. = FALSE)
    ref <- getout(sha)
  } else {
    if (!is(ref, "data.frame")) ref <- readr::read_csv(ref)
    sha <- get_sha(ref)
    putin(ref)
  }

  structure(unname(lapply(x, function(z) {
    ex <- ref[ref[[against]] %in% z, against]
    if (length(ex) == 0) {
      structure(list(z, NA), match = "no_exact_match")
    } else {
      structure(list(z, ex), match = "exact_match")
    }
  })), class = "splist", ref = what_ref(ref),
  refsha = sha, against = against)
}

get_sha <- function(x) digest::digest(x, algo = "sha1")

have_hash <- function(x) {
  sha <- get_sha(x)
  ls(envir = splist_data_env)
}

putin <- function(x) {
  assign(get_sha(x), x, envir = splist_data_env)
}

getout <- function(x) {
  get(x, envir = splist_data_env)
}
