tex <- function(...) {
  .global$label_count <- .global$label_count + 1L
  UseMethod("tex")
}

tex.matrix <- function(x, options = getOption("texr.params")) {
  opt_names <- names(options)
  if (!is.list(options) || is.null(opt_names) || any(opt_names == ""))
    stop("`options` must be a named `list`")
  if (any(ido <- !opt_names %in% .global$param_names)) 
    warning("Option values not recognized: ", 
            paste(opt_names[ido], collapse = ","))
  #no need to evaluate `getOption` twice if `options` unchanged
  if (!deparse(substitute(options)) == 'getOption("texr.params")') {
    opts <- getOption("texr.params")
    opts[opt_names] <- options
  } else { opts <- options }
  for (oo in .global$param_names) assign(oo, opts[[oo]], envir = environment())
  
  mm <- nrow(x)
  nn <- ncol(x)
  
  use.row <- isTRUE(use.dims) || use.dims == "row"
  if (is.null(rown <- rownames(x))) rown <- 1L:mm
  
  use.col <- isTRUE(use.dims) || use.dims == "col"
  if (is.null(coln <- colnames(x))) coln <- 1L:nn

  #number of (effective) ROWS increases when
  #  use.col (and vice versa)
  NN <- nn + use.row
  MM <- mm + use.col
  
  if (is.null(align)) {
    alignc <- rep("r", NN)
    if (length(vline.after)) {
      if (!all(vline.after %in% (idv <- (0L - use.row):nn)))
        stop("Column", 
             if (length(idx <- which(!vline.after %in% idv)) > 1L) "s",
             " ", paste(idx, collapse = ","), 
             if (length(idx) > 1L) " are" else " is", 
             " outside this table's range (", min(idv), ":", max(idv), ").",
             if (-1 %in% vline.after & !use.row) 
               " Please use `use.dims = TRUE` or `use.dims = \"row\"`")
      alignc <- character(2L * NN + 1L)
      alignc[seq(2L, length(alignc), by = 2L)] <- "r"
      alignc[alignc == ""][which(idv %in% vline.after)] <- "|"
    }
    align <- paste(alignc, collapse = "")
  } else {
    if (nchar(gsub("|", "", align, fixed = TRUE)) != NN)
      stop("You specified ", nchar(align), " alignments for ", NN, " columns.")
    if (nchar(gsub("[lrc|]|p\\{[0-9.a-z]*\\}", "", align)) > 0)
      stop("Invalid `align` specification: ", align)
    if (grepl("|", align, fixed = TRUE) && length(vline.after))
      warning("Attempt to specify vertical lines both with `vline.after` ",
              "and within `align`; ignoring `vline.after`")
  }
  
  if (is.null(caption)) 
    caption <- paste("Matrix:", deparse(substitute(x)))
  
  if (is.null(label)) label <- "tbl:" %+% .global$label_count
  
  if (!all(idx <- (placement_spl <- 
            strsplit(placement, split = "", fixed = TRUE)[[1L]]) %in% 
           c("!", "h", "t", "b", "p")))
    stop("Invalid float placement options: ", 
         paste(unique(placement_spl[!idx]), collapse = ", "))
  
  if (!is.null(digits)) {
    if (is.numeric(x)) x <- round(x, digits = digits)
    else warning("`", deparse(substitute(x)), "` is not numeric; ",
                 "ignoring supplied `digits` argument.")
  }
  
  if (caption.placement %in% c("top", "above")) {
    cap.above <- TRUE
  } else {
    if (caption.placement %in% c("bottom", "below")) {
      cap.above <- FALSE
    } else 
      stop("Invalid `caption.placement`; valid values are `\"top\"`, " %+% 
             "`\"bottom\"`, `\"above\"`, and `\"below\"`.")
  }
  
  #length-7 padding for table environment set-up,
  #  caption/label printing, and environment closing
  out <- character(ll <- MM + length(hline.after) + 7L)
  #begin{table}, begin{tabular}, \centering go to slots:
  tab.c.tabu.ind <- 
    c(1L, 2L:3L + 2L * cap.above, ll - 1L - 2L * (!cap.above), ll)
  #caption{} and label{} go into slots:
  cap.lab.ind <- if (cap.above) 2L:3L else ll - 2L:1L
  
  out[tab.c.tabu.ind] <- 
    c("\\begin{table}[" %+% placement %+% "]",
      "\\centering",
      "\\begin{tabular}{" %+% align %+% "}",
      "\\end{tabular}",
      "\\end{table}")
  out[cap.lab.ind] <- 
    c("\\caption{" %+% caption %+% "}",
      "\\label{" %+% label %+% "}")
  
  x[is.na(x)] <- na.char
  
  mrows <- apply(x, 1L, .tex_row)
  if (use.row) mrows <- rown %+% " & " %+% mrows
  if (use.col) mrows <- c(paste0(if (use.row) " & ", .tex_row(coln)), mrows)
  if (length(hline.after)) {
    if (!all(hline.after %in% (idc <- (0L - use.col):mm)))
      stop("Row", if (length(idx <- which(!hline.after %in% idc)) > 1L) "s",
           " ", paste(idx, collapse = ","), 
           if (length(idx) > 1L) " are " else " is ", 
           "outside this table's range (", min(idc), ":", max(idc), ").",
           if (-1 %in% hline.after & !use.col) 
             " Please use `use.dims = TRUE` or `use.dims = \"col\"`")
    tbody <- character(2L * MM + 1L)
    tbody[seq(1L, length(tbody), by = 2L)
          ][which(idc %in% hline.after)] <- "\\hline"
    tbody[seq(2L, length(tbody), by = 2L)] <- mrows
    tbody <- tbody[tbody != ""]
  } else 
    tbody <- mrows
  out[4L:(ll - 4L) + 2L * cap.above] <- tbody
  cat(out, sep = "\n", file = file)
  invisible(out)
}

.tex_row <- function(x) {
  paste(x, collapse = " & ") %+% " \\\\"
}