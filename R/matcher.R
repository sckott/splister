#' Match spp list to reference list
#'
#' @param spplist Input species list, a vector
#' @param ref Reference taxon data.frame
#' @examples
#' x <- ""
#' y <- system.file("examples", "worms_salmo.csv", package = "splister")
#' matcher(x, y)
matcher <- function(spplist, ref) {
  UseMethod("matcher")
}

#' @export
#' @rdname matcher
matcher.data.frame <- function(spplist, ref) {

}
