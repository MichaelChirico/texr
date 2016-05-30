tex <- function(x, ...) {
  UseMethod("tex")
}

tex.matrix <- function(x, use.dims = TRUE, align = NULL, 
                       label = NULL, digits = NULL,
                       caption = NULL, hline.after = integer(0L),
                       vline.after = integer(0L), na.char = "",
                       file = "", placement = "") {
  use.col <- isTRUE(use.dims) || use.dims == "col"
  if (is.null(coln <- colnames(x))) coln <- 1L:ncol(x)
  use.row <- isTRUE(use.dims) || use.dims == "row"
  if (is.null(rown <- rownames(x))) rown <- 1L:nrow(x)
  if (is.null(align)) {
    alignc <- rep("r", ncol(x) + use.row)
    if (length(vline.after)) {
      if (!all(vline.after %in% (idv <- (0L - use.row):ncol(x))))
        stop("Column", 
             if (length(idx <- which(!vline.after %in% idv)) > 1L) "s",
             paste(idx, collapse = ","), 
             if (length(idx) > 1L) " are" else " is", 
             "outside this table's range (",
             min(idv), ":", max(idv), ").",
             if (-1 %in% vline.after & !use.row) 
               " Please use `use.dims = TRUE` or `use.dims = \"row\"")
      alignc <- character(2L * (ncol(x) + use.row) + 1L)
      alignc[seq(2L, length(alignc), by = 2L)] <- "r"
      alignc[alignc == ""][which( %in%
                                   vline.after)] <- "|"
    }
    align <- paste(alignc, collapse = "")
  } else {
    if (nchar(align) != ncol(x) + use.row)
      stop("You specified ", nchar(align), " alignments for ", ncol(x) + use.row, " columns.")
    if (grepl("|", align, fixed = TRUE) & length(vline.after))
      warning("Attempt to specify vertical lines both with `vline.after` ",
              "and within `align`; ignoring `vline.after`")
  }
  if (is.null(caption)) 
    caption <- paste("Matrix:", as.character(substitute(x)))
  if (is.null(label)) label <- "tbl:x"
  if (!all((placement_spl <- strsplit(placement, split = "")[[1L]]) %in% 
           c("!", "h", "t", "b", "p")))
    stop("Invalid float placement options: ",
         paste(placement_spl[!placement_spl %in% c("!", "h", "t", "b", "p")],
               collapse = ", ")
  
  mm <- nrow(x)
  MM <- nrow(x) + use.col
  #length-7 padding for table environment set-up,
  #  caption/label printing, and environment closing
  out <- character(ll <- MM + length(hline.after) + 7L)
  out[c(1L:5L, ll - 1L, ll)] <- 
    c("\\begin{table}[" %+% placement %+% "]"),
      "\\caption{" %+% caption %+% "}",
      "\\label{" %+% label %+% "}",
      "\\centering",
      "\\begin{tabular}{" %+% align %+% "}",
      "\\end{tabular}",
      "\\end{table}")
  mrows <- apply(x, 1L, .tex_row)
  if (use.row) mrows <- rown %+% " & " %+% mrows
  if (use.col){
    if (use.row) mrows <- c(" & " %+% .tex_row(coln), mrows)
    else mrows <- c(.tex_row(coln), mrows)
  }
  if (length(hline.after)) {
    if (!all(hline.after %in% (idc <- (0L - use.col):mm)))
      stop("Row", if (length(idx <- which(!vline.after %in% idv)) > 1L) "s",
           paste(idx, collapse = ","), 
           if (length(idx) > 1L) " are" else " is", 
           "outside this table's range (",
           min(idc), ":", max(idc), ").",
           if (-1 %in% hline.after & !use.col) 
             " Please use `use.dims = TRUE` or `use.dims = \"col\"")
    tbody <- character(2L * MM + 1L)
    tbody[seq(1L, length(tbody), by = 2L)
          ][which(idc %in%hline.after)] <- "\\hline"
    tbody[seq(2L, length(tbody), by = 2L)] <- mrows
    tbody <- tbody[tbody != ""]
  } else 
    tbody <- mrows
  out[6L:(ll - 2L)] <- tbody
  cat(out, sep = "\n", file = file)
  invisible(out)
}

.tex_row <- function(x) {
  paste(x, collapse = " & ") %+% " \\\\"
}