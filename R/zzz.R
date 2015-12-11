cmp <- function(l) Filter(Negate(is.null), l)

pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

pluck_attr <- function(x, name, type, combine = FALSE) {
  if (missing(type)) {
    lapply(x, function(z) {
      if (combine) paste0(attr(z, name), collapse = ",") else attr(z, name)
    })
  } else {
    vapply(x, function(z) {
      if (combine) paste0(attr(z, name), collapse = ",") else attr(z, name)
    }, FUN.VALUE = type)
  }
}

what_ref <- function(x) {
  if (is(x, "data.frame")) {
    list(type = "data.frame", cols = NCOL(x), rows = NROW(x))
  }
}
