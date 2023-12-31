% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc_legendEGSL.R
\name{legendEGSL}
\alias{legendEGSL}
\title{Function to create legend for the St. Lawrence base map \code{plotEGSL()}.}
\usage{
legendEGSL(
  range = c(0, 1),
  pal = NULL,
  cexMain = 1,
  cexSub = 0.75,
  mainTitle = NULL,
  subTitle = NULL,
  n = 5
)
}
\arguments{
\item{range}{numeric, vector with minimal and maximal values}

\item{pal}{character, vector of colors, or color palette}

\item{mainTitle}{character, type of legend. Choices are 'continuous', 'binary' or 'categorical'}

\item{subTitle}{character, type of legend. Choices are 'continuous', 'binary' or 'categorical'}

\item{cex.text}{numeric, cex for legend text}

\item{type}{character, type of legend. Choices are 'continuous', 'binary' or 'categorical'}

\item{nTick}{numeric, number of ticks in the legend}
}
\value{
Opens a graphical interface with the plot
}
\description{
Function to create legend for the St. Lawrence base map \code{plotEGSL()}.
}
\examples{
plotEGSL()
legendEGSL(mainTitle = 'Acidification', subTitle = expression(Aragonite~(m^2)))

plotEGSL2('webdriver', driver = 'aquacultureInv')
}
\keyword{lawrence}
\keyword{plot,}
\keyword{st.}
