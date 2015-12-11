ask <- function(x, z, against) {
  message("\n\nMore than match found for '", z, "'!\n
          Enter rownumber (other inputs will return 'NA'):\n")
  print(x)
  take <- scan(n = 1, quiet = TRUE, what = 'raw')
  if (length(take) == 0) {
    structure(list(z, NA), match = "no_fuzzy_match")
  } else {
    if (take %in% seq_len(NROW(x))) {
      take <- as.numeric(take)
      message("Input accepted, took row '", take, "'.\n")
      structure(list(z, as.character(x[take, against])), match = "replace")
    } else {
      structure(list(z, NA), match = "no_fuzzy_match")
    }
  }
}
