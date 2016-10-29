\name{tex}
\alias{tex}
\alias{tex.matrix}
\alias{tex.table}
\alias{tex.lm}
\alias{tex.list}
\title{TeX-friendly Output Formatting}
\description{
  Convert \R objects into LaTeX-parseable output.
}
\usage{
  tex(...)

  \method{tex}{matrix}(x, options = getOption("texr.params"), \dots)

  \method{tex}{table}(x, options = getOption("texr.params"), ...)

  \method{tex}{lm}(x, options = getOption("texr.params"), 
                   model.name = "Model", include.se = TRUE, 
                   clean.intercept = TRUE, ...)

  \method{tex}{list}(ll, options = getOption("texr.params"), ...)
}
\arguments{
  \item{x}{ An R object. }
  \item{options}{ A named \code{list} of options to be applied. See \code{\link{texr.options}}. }
  \item{\dots}{ Alternative approach for providing options. Anything provided must be a valid option; see \code{\link{texr.options}}. Exactly one of \code{options} and \code{...} can be used in any \code{tex} call. }
  \item{model.name}{ \code{character}; name to be given to the model in output, e.g., \code{"Fixed Effects"}. }
  \item{clean.intercept}{ \code{logical}; should the parentheses produced by \code{lm} surrounding the intercept name be removed? }
  \item{ll}{ A \code{list} of objects, to each of which \code{tex} will be applied. The result is then concatenated for making a multi-column table. }
}
\seealso{ 
  \code{\link{texr.options}}
}
