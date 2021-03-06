% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/scientificNotation.R
\name{cSpread}
\alias{cSpread}
\title{Calculate the center spread (range) of a sample}
\usage{
cSpread(x, frac = 0.95)
}
\arguments{
\item{x}{A sample of data}

\item{frac}{The fraction of the center part, the default is 0.95}
}
\value{
The range of the center part
}
\description{
Calculate the center spread (range) of a sample
}
\examples{
table1 <- c(7999,7998,7998,7997)
cSpread(table1)

table2<-c(97.62,92.24,100.90,90.39,48.29,42.31,49.98,39.09,75.23,75.16,100.11,74.23,49.69,57.21,80.19,51.09)
table2 <- matrix(table2, nrow=4,byrow=TRUE,dimnames=list(c("North" ,"South","East","West"),c("Q1","Q2","Q3","Q4")))
table2
cSpread(table2, frac = 0.8)

#' UCBAdmissions
cSpread(UCBAdmissions, frac=1)

Titanic
cSpread(UCBAdmissions, frac=0.5)
}
\author{
Wayne Oldford and Xiaomei Yu
}

