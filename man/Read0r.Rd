% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Read0r2.R
\name{BCSraw}
\alias{BCSraw}
\alias{VMPraw}
\alias{ARBINrawXLSX}
\alias{ARBINrawACCDB}
\title{Read0r2.R}
\usage{
BCSraw(dir, filename)

VMPraw(dir, filename)

ARBINrawXLSX(dir, f.path)

ARBINrawACCDB(filename)
}
\arguments{
\item{dir}{directory to raw data folder}

\item{filename}{sample name}

\item{f.path}{full file path with ending}
}
\value{
returns data.frame with raw data
}
\description{
Imports experimental data from data folders, depending on data format/instrument.
Experimental data is converted into a uniform format for further processing
}
\details{
read .txt files from raw data directory (for Biologic BCS)

read .txt files from raw data directory (for Biologic VMP)

read .xlsx files from raw data directory

read .accdb (MS Access) files from raw data directory
}
\examples{
\dontrun{
raw <- BCSraw()
}
}
