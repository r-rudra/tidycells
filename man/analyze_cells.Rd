% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_cells.R
\name{analyze_cells}
\alias{analyze_cells}
\title{Analyze Cells}
\usage{
analyze_cells(d, silent = TRUE)
}
\arguments{
\item{d}{A \code{\link[=cell_df-class]{cell_df}} after \code{\link[=value_attribute_classify]{Value Attribute Classification}} done}

\item{silent}{logical scalar indicating whether to raise a warning if heuristic detection fails. (Default TRUE).}
}
\value{
Detailed analysis of the cell data structure.
Which will be a \code{\link[=cell_analysis-class]{cell_analysis}} class object.
}
\description{
After \code{\link[=value_attribute_classify]{Value Attribute Classification}} done on a \code{\link[=cell_df-class]{cell_df}} next task to do is
analyze it's contents for data block detection, attribute orientation identification etc. The function \code{analyze_cells} (and also \code{analyse_cells})
does the same for you.

\strong{Note}:
\if{html}{If you are not sure about what package functions actually do or how they work together,
please start with \href{../doc/tidycells-intro.html}{\code{vignette("tidycells-intro")}}.}
\if{latex}{If you are not sure about what package functions actually do or how they work together,
please start with \code{vignette("tidycells-intro")}.}
}
\details{
it returns detailed analysis of the data structure including data block detection, attribute orientation detection etc.
The argument \code{silent} is set to \code{TRUE} by default, as the warning will be given whenever the \code{\link[=cell_analysis-class]{cell_analysis}} is printed.

After this step one may like to do :
\itemize{
\item \code{\link[=compose_cells]{compose_cells}}
}

If in an interactive session, following additional functions can be helpful for interactive visualizations:
\itemize{
\item \code{\link[=visual_data_block_inspection]{visual_data_block_inspection}}
\item \code{\link[=visual_orientation_modification]{visual_orientation_modification}}
\item \code{\link[=visual_traceback]{visual_traceback}}
}
}
\examples{
d <- structure(c(
  "block 1", "", "C", "D", "", "block 2", "", "C",
  "D", "", "A", "1", "2", "", "", "A", "10", "20", "", "B", "3",
  "4", "", "", "B", "30", "40"
), .Dim = c(9L, 3L))
d <- as.data.frame(d)
cd <- as_cell_df(d) \%>\% numeric_values_classifier()

# see it
cd \%>\% plot(adaptive_txt_size = FALSE)
ca <- analyze_cells(cd)

# look at the plot for detected directions
plot(ca)
}
\seealso{
\code{\link[=compose_cells]{compose_cells}}, \code{\link[=collate_columns]{collate_columns}}
}
