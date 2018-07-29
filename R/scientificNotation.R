## Get the scientific notation elements (exponents and coefficients) of a sample of Data
## Idea: First calculate the exponent (the power of 10),
##       then according to that the exponent is positive or negative
##       to construct a divisor or multiplier. By doing this
##       we avoid dealing with decimal numbers. Then use the
##       number to devide the divisor or time the multiplier
##       to get the coefficient.


#' @title Get the coefficient and exponent of Scientific notation.
#' @param table A sample of data, can be a number, a vector or a matrix, an
#'   array or a table.
#' @param ndigits Number of digits to be shown in the coefficient of the
#'   scientific notation.
#' @param round Logical, the last digit of the coeeficient is obtained by
#'   rounding, otherwise by truncating.
#' @return A list containing the scientific notation coefficient and scientific
#'   notation exponent of the sample.
#' @author R. Wayne Oldford and Xiaomei Yu
#' @examples
#' table1 <- c(7999,7998,7998,7997)
#' SciNotation(table1)
#'
#' table2<-c(97.62,92.24,100.90,90.39,48.29,42.31,49.98,39.09,75.23,75.16,100.11,74.23,49.69,57.21,80.19,51.09)
#' table2 <- matrix(table2, nrow=4,byrow=TRUE,dimnames=list(c("North" ,"South","East","West"),c("Q1","Q2","Q3","Q4")))
#' table2
#' SciNotation(table2, ndigits=2)
#'
#' UCBAdmissions
#' SciNotation(UCBAdmissions, ndigits=1, round=FALSE)
#'
#' Titanic
#' SciNotation(Titanic)


SciNotation <- function(table, ndigits, round = TRUE) {
  exponent <- mapply(function(x){
                      if (x == 0) 0 else floor(log(abs(x), 10))
                      },
                     table)
  multiplier <- rep(1, length(exponent))
  negativeExp <- exponent < 0
  multiplier[negativeExp] <- 10 ^ (- exponent[negativeExp])

  numValues <- table * multiplier

  divisor <- rep(1, length(exponent))
  positiveExp <- exponent > 0
  divisor[positiveExp] <- 10 ^ (exponent[positiveExp])

  numValues <- numValues / divisor

  if (missing(ndigits)){
    coefficient <- table / 10 ^ exponent
  } else {
    coefficient <- if (round) {
      round(numValues, digits = ndigits - 1)} else {
        trunc(numValues, digits = ndigits - 1)
      }
  }

  list(
    coefficient = coefficient,
    exponent = exponent
    )
}

# -----------------------------------------------------------------------
## Get the center spread of a sample
## Idea: by trimming a fraction from
##       each end of the sample


#' @title Calculate the center spread (range) of a sample
#' @param x A sample of data
#' @param frac The fraction of the center part, the default is 0.95
#' @return The range of the center part
#' @author Wayne Oldford and Xiaomei Yu
#' @examples
#' table1 <- c(7999,7998,7998,7997)
#' cSpread(table1)
#'
#' table2<-c(97.62,92.24,100.90,90.39,48.29,42.31,49.98,39.09,75.23,75.16,100.11,74.23,49.69,57.21,80.19,51.09)
#' table2 <- matrix(table2, nrow=4,byrow=TRUE,dimnames=list(c("North" ,"South","East","West"),c("Q1","Q2","Q3","Q4")))
#' table2
#' cSpread(table2, frac = 0.8)
#'
#' #' UCBAdmissions
#' cSpread(UCBAdmissions, frac=1)
#'
#' Titanic
#' cSpread(UCBAdmissions, frac=0.5)

cSpread <- function (x, frac = 0.95) {
  tail <- (1 - frac) / 2
  spread <- diff(quantile(x, c(tail, 1-tail)))
  names(spread) <- NULL
  spread
}


# --------------------------------------------------------------------

#' @title Get the unique values and count the numbers of them in a sample
#' @param x A sample of data
#' @return A two-column matrix, the first column records the unique values
#'         and the second column are the correponding numbers of the unique values
#' @author Wayne Oldford and Xiaomei Yu
#' @examples
#' countUnique(c(1,4,5,2,1,1,4,2))

countUnique <- function (x) {
  vals <-unique(x)
  counts <- mapply(function(v) {length(x[x == v])},
                   vals
                  )
  cbind(vals, counts)
}