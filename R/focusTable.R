## Remove redundant zeros and show most table numbers Idea: Find a base that
## reduces the number of trailing zeros and make sure most numbers are shown in
## the table to avoid non-integers and scientific notations.


#' @title Focus the table on those entries having minimal range in order of
#'   magnitude.
#' @param table A sample of data, can be a number, a vector or a matrix, an
#'   array or a table.
#' @param nSig Number of significant digits to be showed (default is 2).
#' @param expsRange The threshold to allow how many numbers to be shown (default
#'   is nSig-1). If the spread of exponents of the table is smaller than
#'   expsRange, then all the numbers in the table will be shown in the new
#'   table. Otherwise, only those numbers whose exponents are within this
#'   expsRange will be displayed without rounding, others will be rounded
#'   according to the determined base.
#' @return A list containing the focused table, the base number (the exponent of
#'   10) used to construct the units of the table, and a recommended penalty for
#'   "scipen" to be used in options(...) when printing the table.
#' @author R. Wayne Oldford and Xiaomei Yu
#' @examples
#' table1 <- c(97623,92243,100906,90397,48296,42317,49983,39097,75238,75162,100116,74235,49699,57212,80196,51092)
#' table1 <- matrix(table1, nrow=4,byrow=TRUE)
#' focusTable(table1)
#'
#'
#' table2<-array(c(0.000012345,0.002,
#' 3.3,4,
#' 20,34,
#' 20000,300000),
#' c(4,4)
#' )
#' focusTable(table2)
#' focusTable(table2, nSig=3)
#'
#'
#' table3<-array(c(0.12345,2,
#' 3.3,4,
#' 20.3,34.5,
#' 200,3000),
#' c(4,4)
#' )
#' focusTable(table3)
#'
#'
#' table4<-array(c(1,20,
#' 33,40,
#' 203,345,
#' 2000,30000),
#' c(4,4)
#' )
#'
#'
#' focusTable(table4)
#'
#'
#' table5 <- array(c(5*10^5,9*10^5,2*10^5,8*10^5,
#'   5*10^4,9*10^4,2*10^4,8*10^4,
#'   5*10^2,9*10^2,2*10^2,8*10^2,
#'   5*10^3,9*10^3,2*10^3,8*10^3,
#'   7*10^9,4*10^9,2*10^9,5*10^9,
#'   8*10^2,3*10^2,7*10^2,1*10^2,
#'   6*10^3,8*10^3,2*10^3,9*10^3,
#'   4*10^7,9*10^7,3*10^7,1*10^7
#' ), c(4,4,2))
#' dimnames(table5) <- list(c("i1","i2","i3","i4"),c("j1","j2","j3","j4"),c("k1","k2"))
#' focusTable(table5)


focusTable <- function(table, nSig=2, expsRange=nSig-1){
  # helper functions
  # function to check if it's an integer
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  # find the case and sci penalty by range
  baseByRange <- function(table, nSig=2, expsRange=nSig-1){
    results <- SciNotation(table,nSig)
    exps <- results$exponent
    # first check if the whole range of exps is <=(nSig -1)
    if (diff(range(exps)) <= expsRange) {
      # in this case we will use the smallest exps to derive base
      base <- min(exps)-(nSig-1)
      newTable <- results$coefficient*10^(exps-base)

      # when scipen=0 (default in R), can keep 4 zeros, e.g. 10000,
      # more than 4 zeros, will be scientific notation, e.g., 100000 will be 1e+05
      sci <- max(max(exps)-base-4,0)
    } else
    {
      astart <- min(exps)
      amax <- max(exps)
      aend <- amax - expsRange
      maxcount <- 0
      abase<-astart

      for (a in seq(astart, aend, 1)) {
        count <- sum(exps >= a & exps <= a + expsRange)
        if (count > maxcount) {
          maxcount <- count
          abase <- a
        }
      }
      base <- abase - (nSig-1)
      newTable <- results$coefficient*10^(exps-base)
      newTable<-round(newTable)
      sci <- max(amax-base-4,0)
    } # end of else
    list(table=newTable, base=base, scipen=sci)
  }

  #
  # Focus table first by using only the range of orders of magnitude
  baseInfo <- baseByRange(table, nSig=nSig, expsRange = expsRange)
  base <- baseInfo$base
  sci <- baseInfo$sci
  newTable <- baseInfo$table

  # Now increase base to remove trailing zeros
  testTable <- newTable
  for (i in 1:(nSig-1)){
    newCoef <- testTable/10^i
    if(sum(is.wholenumber(newCoef))==length(newCoef)){
      base <- base + 1
      newTable <- newTable/10
      sci <- max(sci-1,0)
    }
  }

  list(table=newTable, base=base, scipen=sci)
}




