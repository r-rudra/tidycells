% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_cells.R
\name{validate_cells}
\alias{validate_cells}
\title{Validate cell-DF}
\usage{
validate_cells(dat)
}
\arguments{
\item{dat}{An R object}
}
\value{
Logical scalar. If the value is \code{FALSE}, it will contain attribute named \code{msg}.
}
\description{
Validate cell-DF
}
\details{
It checks for following facts:
\itemize{
\item Whether \code{dat} is a \code{data.frame}
\item Whether \code{dat} conforms to \code{rc_df} format
\item Whether \code{dat} conforms to \code{cell_df} format
If all the checks are passed then it returns \code{TRUE} else it returns \code{FALSE} with \code{msg} attribute, indicating reason for validation failure.
}
}
\examples{
# returns TRUE
validate_cells(tibble::tibble(row = 1, col = 2, data_type = "numeric", value = "1"))

# this is FALSE
chk <- validate_cells(tibble::tibble(row = 1, col = 2, data_type = "numeric"))

# msg
attr(chk, "msg")
}
\keyword{internal}
