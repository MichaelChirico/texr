tex <- function(...) {
  .global$label_count <- .global$label_count + 1L
  UseMethod("tex")
}

tex.matrix <- function(x, options = getOption("texr.params"), ...) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(options)) stop("Please use `options` alone or don't use it.")
    
    dot_names <- names(dots)
    opts <- getOption("texr.params")
    if (any(ido <- !dot_names %in% .global$param_names)) 
    warning("Option values not recognized: ", 
            paste(dot_names[ido], collapse = ","))
    opts[dot_names] <- dots
  } else {
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
  }
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
    } else alignc <- rep("r", NN)
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
  
  if (floating.environment == "sidewaystable") .warn_rotate()

  #length-7 padding for table environment set-up,
  #  caption/label printing, and environment closing
  out <- character(ll <- MM + length(hline.after) + 7L*(!only.body))
  if (!only.body) {
    #begin{table}, begin{tabular}, \centering go to slots:
    tab.c.tabu.ind <- 
      c(1L, 2L:3L + 2L * cap.above, ll - 1L - 2L * (!cap.above), ll)
    #caption{} and label{} go into slots:
    cap.lab.ind <- if (cap.above) 2L:3L else ll - 2L:1L
    
    out[tab.c.tabu.ind] <- 
      c("\\begin{" %+% floating.environment %+% "}[" %+% placement %+% "]",
        "\\centering",
        "\\begin{tabular}{" %+% align %+% "}",
        "\\end{tabular}",
        "\\end{" %+% floating.environment %+% "}")
    out[cap.lab.ind] <- 
      c("\\caption{" %+% caption %+% "}",
        "\\label{" %+% label %+% "}")
  }
  
  x[is.na(x)] <- na.char
  
  mrows <- apply(x, 1L, .tex_row) %+% (if (line.ends) " \\\\")
  if (use.row) mrows <- rown %+% " & " %+% mrows
  if (use.col) mrows <- 
    c(paste0(if (use.row) " & ", .tex_row(coln), 
             if (line.ends) " \\\\"), mrows)
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
  if (only.body) out <- tbody else out[4L:(ll - 4L) + 2L * cap.above] <- tbody
  cat(out, sep = line.split, file = file)
  invisible(out)
}

tex.default <- function(x, ...) {
  tex.matrix(as.matrix(x), ...)
}

tex.table <- function(x, options = getOption("texr.params"), ...) {
  ndim <- length(dim(x))
  mcall <- match.call()
  if ("caption" %in% names(mcall)) caption <- list(...)$caption
  else caption <- options$caption
  
  if ("hline.after" %in% names(mcall)) hline.after <- list(...)$hline.afer
  else hline.after <- options$hline.after
  
  if ("vline.after" %in% names(mcall)) vline.after <- list(...)$vline.afer
  else vline.after <- options$vline.after
  
  if (ndim > 2L) stop("Support for 3+ -dimensional tables not implemented.")
  if (ndim == 1L) 
    tex.matrix(t(as.matrix(x)), use.dims = "col", 
               caption = 
                 if (is.null(caption)) "Table: " %+% deparse(substitute(x))
               else caption, 
               hline.after = if (is.null(hline.after)) 0L else hline.after,
               vline.after = if (is.null(vline.after)) 0L else vline.after, ...)
  else
    tex.matrix(as.matrix(x), use.dims = TRUE, 
               caption = 
                 if (is.null(caption)) "Table: " %+% deparse(substitute(x))
               else caption, 
               hline.after = if (is.null(hline.after)) 0L else hline.after,
               vline.after = if (is.null(vline.after)) 0L else vline.after, ...)
}

tex.lm <- function(x, options = getOption("texr.params"), 
                   model.name = "Model", include.se = TRUE, 
                   clean.intercept = TRUE, ...) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(options)) stop("Please use `options` alone or don't use it.")
    
    dot_names <- names(dots)
    opts <- getOption("texr.params")
    if (any(ido <- !dot_names %in% .global$param_names)) 
    warning("Option values not recognized: ", 
            paste(dot_names[ido], collapse = ","))
    opts[dot_names] <- dots
  } else {
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
  }
  for (oo in .global$param_names) assign(oo, opts[[oo]], envir = environment())

  use.row <- isTRUE(use.dims) || use.dims == "row"
  use.col <- (isTRUE(use.dims) || use.dims == "col") && !is.null(model.name)
  
  if (is.null(caption)) 
    opts$caption <- paste("Regression:", deparse(substitute(x)))
  
  if (is.null(label)) opts$label <- "reg:" %+% .global$label_count
 
  lmsum <- summary(x)
  
  if (!is.null(digits)) {
    coefs <- round(lmsum$coefficients[ , "Estimate"], digits)
    if (include.se) ses <- round(lmsum$coefficients[ , "Std. Error"], digits)
    #to prevent attempted rounding by tex.matrix
    opts$digits <- NULL
  } else {
    coefs <- lmsum$coefficients[ , "Estimate"]
    if (include.se) ses <- lmsum$coefficients[ , "Std. Error"]
  }
  
  MM <- (1L + include.se) * length(coefs)
  coef.ind <- seq(1L, by = (1L + include.se), length.out = length(coefs))
  
  if (use.row) {
    rown <- character(MM)
    rown[coef.ind] <- .clean_coef(x, clean.intercept)
  } else rown <- NULL
  
  out <- matrix("", nrow = MM, 
                dimnames = list(rown, if (use.col) model.name))
  out[coef.ind] <- coefs
  if (include.se) out[coef.ind + 1L] <- "(" %+% ses %+% ")"
  
  tex.matrix(out, options = opts)
}

tex.list <- function(ll, options = getOption("texr.params"), ...) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(options)) stop("Please use `options` alone or don't use it.")
    
    dot_names <- names(dots)
    opts <- getOption("texr.params")
    if (any(ido <- !dot_names %in% .global$param_names)) 
    warning("Option values not recognized: ", 
            paste(dot_names[ido], collapse = ","))
    opts[dot_names] <- dots
  } else {
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
  }
  
  inner.opts.names <- c("only.body", "line.ends", "use.dims")
  inner.opts <- opts[inner.opts.names]

  opts[inner.opts.names] <- list(TRUE, FALSE, FALSE)
  
  capture.output(tex.body <- lapply(ll, tex, options = opts))
  
  #Pad unequal-length objects with ""
  #**  (Shouldn't do this. Should match based on row names before
  #**   padding with "").
  if (diff(range(lens <- lengths(tex.body)))) {
    LL = max(lens)
    tex.body[lens != LL] = 
      lapply(tex.body[lens != LL],
             function(ee) c(ee, rep.int("", LL - length(ee))))
  }
  do.call(paste, c(tex.body, sep = " & "))
}

.tex_row <- function(x) paste(x, collapse = " & ")

.clean_coef <- function(x, clean.intercept = TRUE, clean.factors = TRUE) {
  ans <- names(x$coefficients)
  if (clean.intercept) ans <- gsub("(Intercept)", "Intercept", ans, fixed = TRUE)
  ans
}
