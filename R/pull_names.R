#' Pull out names after matching is done
#'
#' @export
#' @param x Input species list, a character vector
#' @return Characater vector of names, original names for those not matched,
#' and replacment names for those matched
#' @examples \dontrun{
#' x <- system.file("examples", "iucn_dat.csv", package = "splister")
#' x <- read.csv(x, stringsAsFactors = FALSE)[,-1]
#' y <- system.file("examples", "worms_sample.csv", package = "splister")
#' spp <- x$sciname[1:1000L]
#' res <- match_exact(spp, ref = y, against = "scientificName")
#' res2 <- match_fuzzy(res)
#' pull_names(res2)
#' }
pull_names <- function(x) {
  UseMethod("pull_names")
}

#' @export
#' @rdname pull_names
pull_names.splist <- function(x) {
  vapply(x, function(w) {
    if ("exact_match" %in% attr(w, "match")) {
      w[[1]]
    } else if ("replace" %in% attr(w, "match")) {
      w[[2]]
    } else {
      w[[1]]
    }
  }, "")
}
