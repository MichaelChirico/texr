#logic mostly adopted from test.data.table()

test.texr <- function(...) {
  test_dir = getNamespaceInfo("texr", "path") %+% "/tests/"
  olddir = setwd(test_dir)
  on.exit(setwd(olddir))
  cat("Running tests of `texr`")
  sys.source(file.path(test_dir, "tests.Rraw"), 
             envir = new.env(parent = .GlobalEnv))
}

test <- function(name, x, y, error, warning) {
  cat("\nRunning test: ", name, "\n", sep = "")
  x.catch <- tryCatch(x, error = identity, warning = identity)
  if (inherits(x.catch, "error")) {
    if (missing(error)) {
      cat("`", as.character(substitute(x)),
          "` produced an unanticipated error: '",
          x.catch$message, "'.\n", sep = "")
      return(invisible())
    }
    if (grepl(error, x.catch$message)) return(invisible())
    cat("Expected error matching '", error, 
        "', but returned '", x.catch$message, "'.\n", sep = "")
    return(invisible())
  }
  if (inherits(x.catch, "warning")) {
    if (missing(warning)) {
      cat("`", as.character(substitute(x)),
          "` produced an unanticipated warning: '",
          x.catch$message, "'.\n", sep = "")
      return(invisible())
    }
    if (grepl(warning, x.catch$message)) return(invisible())
    cat("Expected error matching '", error, 
        "', but returned '", x.catch$message, "'.\n", sep = "")
    return(invisible())
  }
  if (identical(x.catch, y)) return(invisible())
  else 
    cat("`", as.character(substitute(x)),
        "` evaluated without errors to:\n", x,
        "\nwhich is not identical to the expected output:\n",
        eval(substitute(y)), "\n", sep = "")
  invisible()
}
