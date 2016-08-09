.onLoad <- function(libname, pkgname) {
  opts = list("texr.labelcount" = 0L,
              "texr.params" = list_to_text(.global$default_params),
              "texr.warn.rotating" = TRUE,
              "texr.warn.rotating.toggle" = TRUE)
  for (ii in names(opts)) {
    eval(parse(text = paste0("options(", ii, "=", opts[[ii]], ")")))
  }
}
