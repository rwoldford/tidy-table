% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getLocation.R
\name{getLocation}
\alias{getLocation}
\title{Get the location of a sample of data}
\usage{
getLocation(table, frac = 1)
}
\arguments{
\item{table}{A sample of data, can be a number, a vector or a matrix, an array
or a table.}

\item{frac}{The fraction of the center part of data, used in cSpread().}
}
\value{
The location of the sample.
}
\description{
Get the location of a sample of data
}
\note{
The choice of frac also represents the fraction of the sample we will
 compare. e.g. if frac=0.9, then everytime we will only compare the center
 90% part of the sample data to check if their certain digits are the same.
 We ignore extreme values in this way
}
\examples{
table1 <- c(1111110000,1134516000,1346578900,1234567890)
getLocation(table1)

table2 <- c(.7999, .7998, .7998, .7997)
getLocation(table2)


table3 <- -c(49991234, 49983454, 50013333, 49923333)
getLocation(table3)


table4 <- c(4220,4232,-6332,4578)
getLocation(table4)

HairEyeColor
getLocation(HairEyeColor)
}
\author{
R. Wayne Oldford and Xiaomei Yu
}

