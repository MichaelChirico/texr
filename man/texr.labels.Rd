\name{texr.labels}
\alias{texr.labels}
\alias{reset_label_count}
\alias{get_label_count}
\title{Automatic Table Numbering}
\description{
  For convenience, \code{texr} automatically numbers tables incrementally, so that the first time \code{tex} is run in a session, the label is 1; the second time, it's 2; and so on. This can be overridden with the \code{label} option. See \code{\link{texr.options}}. 

  Here, we have two convenience functions for interacting with the automatic label numbering.
}
\usage{
  reset_label_count(...)
  get_label_count(...)
}
\arguments{
  \item{...}{ Neither function takes and options. }
}
\details{
  As might be expected, \code{reset_label_count} resets the label count, so that the next table is automatically printed with label 1. \code{get_label_count} returns the current label count. 
}
\seealso{ 
  \code{\link{texr.options}}
}
