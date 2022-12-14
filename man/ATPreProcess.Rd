% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AT_Functions.R
\name{ATPreProcess}
\alias{ATPreProcess}
\title{Alpha Testing Pre-Processing}
\usage{
ATPreProcess(data, iname, fname, rname, fftile = 5, win.prob = c(0, 1))
}
\arguments{
\item{data}{dataframe containing id columns, return column and factor value column}

\item{iname}{character column names of identifiers}

\item{fname}{character column name of factor}

\item{rname}{character column name of return}

\item{fftile}{integer number of fractiles to use in splitting data}

\item{winsor}{pair of numeric to bound data in windsorization}
}
\value{
dataframe containing id columns,
"return", "factor.value","factor.q", "factor.z", "return.z"
}
\description{
Pre-Process Data for Alpha Testing
}
\details{
This function pre-processes factor and return data to be
used in AT functions. Inputs raw data and returns \cr
ID columns \cr
factor value \cr
factor quantile \cr
normalized factor value and \cr
normalized returns
}
