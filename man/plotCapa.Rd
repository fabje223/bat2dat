% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot0r.R
\name{plotCapa}
\alias{plotCapa}
\title{plot0r}
\usage{
plotCapa(capacity)
}
\arguments{
\item{capacity}{data.frame with galvanostatic cycling information}
}
\value{
returns graphs generated from expeirmental data for report.Rmd
}
\description{
Plotting Script for RMarkdown Report
}
\examples{
\dontrun{
capa <- Arbin.CAP()
pCap <- plotCapa(capa)
}
}
