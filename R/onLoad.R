.onLoad <- function(libname, pkgname) {
  opts = c("texr.labelcount" = 0L)
  for (ii in names(opts)) {
    eval(parse(text = paste0("options(", ii, "=", opts[ii], ")")))
  }
}
