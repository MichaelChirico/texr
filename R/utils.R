"%+%" <- function(s1, s2) paste0(s1, s2)

#for eval(parse())-ing a list as text
list_to_text <- function(x) {
  out <- paste(capture.output(dput(x)), collapse = "")
  invisible(out)
}

#Internal globals
.global <- new.env()
setPackageName("texr", .global)
.global$default_params <-
  list(use.dims = TRUE, align = NULL, label = NULL,
       digits = NULL, caption = NULL, hline.after = integer(0L),
       vline.after = integer(0L), na.char = "", file = "", 
       placement = "ht", caption.placement = "above",
       floating.environment = "table", only.body = FALSE,
       line.ends = TRUE)
.global$param_names <- names(.global$default_params)

#Label Counter & Reset Function
#increments by 1 each `tex()` call
reset_label_count <- function(...) .global$label_count <- 0L

get_label_count <- function(...) .global$label_count

#Warning users about the need for rotating package
#  by default, ping the user with a message only the first
#  time tex() is called in a given session;
#  also allow this message to be issued each tex call.
.warn_rotate <- function(...) {
  if (getOption("texr.warn.rotating")) {
    message("Remember to include \\usepackage{rotating} ",
            "in your LaTeX preamble when using sidewaystable.")
    if (getOption("texr.warn.rotating.toggle"))
      options("texr.warn.rotating" = FALSE)
  }
}
