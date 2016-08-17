.onLoad <- function(libname, pkgname) {
  opts = list("texr.labelcount" = 0L,
              "texr.params" = object_to_text(.global$default_params),
              "texr.warn.rotating" = TRUE,
              "texr.warn.rotating.toggle" = TRUE)
  for (ii in names(opts)) {
    eval(parse(text = paste0("options(", ii, "=", opts[[ii]], ")")))
  }
  
  #have to declare these AFTER the above runs,
  #  but also can't declare .global from within
  #  the scope of this function
  .global$label_count <- getOption("texr.labelcount") 
  .global$warn_rotate <- getOption("texr.warn.rotating")
}
