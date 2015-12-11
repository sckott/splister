#' @export
print.splist <- function(x, ...) {
  cat("<species list>", sep = "\n")
  cat(paste0("  Reference: ", make_ref(attr(x, "ref"))), sep = "\n")
  cat(paste0("  Against: ", attr(x, "against")), sep = "\n")
  cat(paste0("  Taxa: ", length(x)), sep = "\n")
  cat(paste0("  Exact Matches: ", how_many(x, "exact_match")), sep = "\n")
  cat(paste0("  Replacements: ", how_many(x, "replace")), sep = "\n")
  cat(paste0("  No Exact Matches: ", how_many(x, "no_exact_match")), sep = "\n")
  cat(paste0("  No Exact or Fuzzy Matches: ", how_many(x, "no_exact_match,no_fuzzy_match")), sep = "\n")
}

how_many <- function(x, y = "exact_match") {
  length(x[pluck_attr(x, "match", "", combine = TRUE) == y])
}

make_ref <- function(x) {
  paste(names(x), unlist(x), sep = ":", collapse = ", ")
}
