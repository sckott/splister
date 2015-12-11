splist_data_env <- NULL
.onLoad <- function(libname, pkgname){
  splist_data_env <<- new.env()
}
