"%+%" <- function(s1, s2) paste0(s1, s2)

#for eval(parse())-ing a list as text
list_to_text <- function(x) {
  out <- paste(capture.output(dput(x)), collapse = "")
  invisible(out)
}

#Internal globals
.global <- new.env()
setPackageName("texr", .global)
.global$label_count <- getOption("texr.labelcount") 
.global$default_params <-
  list(use.dims = TRUE, align = NULL, label = NULL,
       digits = NULL, caption = NULL, hline.after = integer(0L),
       vline.after = integer(0L), na.char = "", file = "", 
       placement = "ht", caption.placement = "above")
.global$param_names <- names(.global$default_params)

#Label Counter & Reset Function
#increments by 1 each `tex()` call
reset_label_count <- function(...) .global$label_count <- 0L

get_label_count <- function(...) .global$label_count
