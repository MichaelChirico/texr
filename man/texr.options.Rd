\name{texr.options}
\alias{texr.options}
\alias{texr-options}
\title{Valid \code{texr} Options}
\description{
  Valid options to be passed to \code{tex} calls.
  
  All methods for \code{tex} support the following arguments. They can either be passed through the \code{options} argument as a \code{list} (this approach facilitates producing a "theme" (or themes) across multiple tables in an analysis) or individually through the catch-all \code{...}. 
}
\details{
  \itemize{
    \item{align}{ Alignment specifications to be passed through to LaTeX's \code{\\align{}}; valid values are \code{l}, \code{r}, \code{c}, and \code{p}. \code{p} must be followed by a valid width specification. Must specify one alignment for each output column (be wary of alignment for row names). Default: \code{NULL}. }
    \item{caption}{ To be provided to LaTeX's \code{\\caption{}}. See also \code{caption.placement} and \code{label}. Default: \code{NULL}. }
    \item{caption.placement}{ Should the caption be included above or below the output? Valid values are \code{"top"}, \code{"above"}, \code{"bottom"}, and \code{"below"}. Default: \code{"above"}. }
    \item{digits}{ For numeric input \code{x}, this number is passed to \code{base::round}'s \code{digits} argument to truncate output as specified. Default: \code{NULL}. }
    \item{file}{ If specified, the output is saved to \code{file}. Default: \code{""} (so printed in \code{stdout}). }
    \item{floating.environment}{ Currently supported environments are \code{table} and \code{sidewaystable}. Default: \code{"table"}. }
    \item{hline.after}{ After which rows of the output should a horizontal line be included (via \code{\\hline})? Must be in the range \code{[-1, nrow(x)]} if \code{use.col} is \code{TRUE} or \code{"col"}, else in \code{[0, nrow(x)]}. See \code{use.dims} and \code{vline.after}. Default: \code{NULL}. }
    \item{label}{ To be provided to LaTeX's \code{\\label{}}. Always included directly below \code{\\caption{}}. Default: \code{NULL}. }
    \item{line.ends}{ \code{logical}; mainly for internal use, for "glue-ing" columns together. If \code{FALSE}, the typical line end (\code{\\}) is excluded; this will lead to unexpected/erroneous output if used improperly. Default: \code{TRUE}. }
    \item{line.split}{ How should lines be separated in the output? Common usage is to just separate lines with a newline (\code{\\n}); this option is for convenience to LyX users, for whom double-newlines facilitate copy-pasting. Default: \code{"\n"}. }
    \item{na.char}{ How should \code{NA} values be printed? Default: \code{""}. }
    \item{only.body}{ \code{logical}; should meta-data (i.e., the \code{\\begin}, \code{\\centering}, \code{\\caption}, etc.) be excluded, leaving only the main body of the table to be outputted? Default: \code{FALSE}. }
    \item{placement}{ Floating environment placement; valid values are \code{!}, \code{h}, \code{t}, \code{b}, and \code{p}. Default: \code{"ht"}. }
    \item{use.dims}{ Which of row names and column names should be included in the output? If \code{use.dims = TRUE}, both are included; if \code{use.dims = "row"}, only row names are included; if \code{use.dims = "col"}, only column names are included; and if \code{use.dims = FALSE}, neither are included. Default: \code{TRUE}. }
    \item{vline.after}{ After which columns of the output should a vertical line be included (via \code{|} in the \code{\\align} specification)? Must be in the range \code{[-1, ncol(x)]} if \code{use.col} is \code{TRUE} or \code{"row"}, else in \code{[0, ncol(x)]}. See \code{use.dims} and \code{hline.after}. Default: \code{NULL}. }
  }
}
\seealso{ 
  \code{\link{texr.options}}
}
