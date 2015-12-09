#' Match spp list to reference list
#'
#' @param spplist Input species list, a character vector
#' @param ref Reference taxon data.frame, or file path
#' @examples
#' x <- c('Salmo trutta', 'Oncorhynchus clarkii', 'Salmo Linnaeus',
#' 'Oncorhynchus clarkii', 'Salvelinus fontinalis', 'Salvelinus confluentus')
#' y <- system.file("examples", "worms_salmo.csv", package = "splister")
#' dat <- unique(read.csv(y, stringsAsFactors = FALSE))
#' matcher(spplist = x, ref = dat, against = "scientificName")
#'
#' # read from file
#' x <- system.file("examples", "iucn_dat.csv", package = "splister")
#' x <- read.csv(x, stringsAsFactors = FALSE)[,-1]
#' y <- system.file("examples", "worms_sample.csv", package = "splister")
#' spp <- x$sciname[1:20000L]
#' res <- matcher(spplist = spp, ref = y, against = "scientificName")
#' unique(sapply(res, attr, "match"))
#' df <- data.frame(original = spp, taxon = unlist(res),
#'    match = sapply(res, attr, "match"))
#' df[df$match == "replace", ]
matcher <- function(spplist, ref, against = NULL) {
  UseMethod("matcher")
}

#' @export
#' @rdname matcher
matcher.character <- function(spplist, ref, against = NULL) {
  if (!is(ref, "data.frame")) ref <- readr::read_csv(ref)
  unname(lapply(spplist, function(z) {
    ex <- ref[ref[[against]] %in% z, against]
    if (length(ex) == 0) {
      df <- ref[grep(z, ref[[against]]), ]
      if (NROW(df) == 0) {
        structure(NA, match = "not_found")
      } else if (NROW(df) > 1) {
        ask(df, z, against)
      } else {
        structure(df[, against], match = "replace")
      }
    } else {
      structure(ex, match = "exact")
    }
  }))
}

ask <- function(x, z, against) {
  message("\n\nMore than match found for '", z, "'!\n
          Enter rownumber (other inputs will return 'NA'):\n")
  print(x)
  take <- scan(n = 1, quiet = TRUE, what = 'raw')
  if (length(take) == 0) {
    structure(NA, match = "not_found")
  } else {
    if (take %in% seq_len(NROW(x))) {
      take <- as.numeric(take)
      message("Input accepted, took row '", take, "'.\n")
      structure(as.character(x[take, against]), match = "replace")
    } else {
      structure(NA, match = "not_found")
    }
  }
}
