"%+%" <- function(s1, s2) paste0(s1, s2)

#Label Counter & Reset Function
.global <- new.env()
setPackageName("texr", .global)

#increments by 1 each `tex()` call
.global$label_count <- getOption("texr.labelcount") 

reset_label_count <- function(...) .global$label_count <- 0L
