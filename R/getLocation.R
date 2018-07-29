## Get the location of a sample
## Idea: Start from the first non-zero digit,
##       if the numbers in the sample share the same
##       digit (the value and exponent of that digit
##       are the same), then record it and continue;
##       stop unitil we find a digit which is not shared
##       by all the figures in the sample. Use those
##       common digits to construct the location.

#'@title Get the location of a sample of data
#'@param table A sample of data, can be a number, a vector or a matrix, an array
#'  or a table.
#'@param frac The fraction of the center part of data, used in cSpread().
#'@return The location of the sample.
#'@author R. Wayne Oldford and Xiaomei Yu
#'@note  The choice of frac also represents the fraction of the sample we will
#'  compare. e.g. if frac=0.9, then everytime we will only compare the center
#'  90% part of the sample data to check if their certain digits are the same.
#'  We ignore extreme values in this way
#'@examples
#' table1 <- c(1111110000,1134516000,1346578900,1234567890)
#' getLocation(table1)
#'
#' table2 <- c(.7999, .7998, .7998, .7997)
#' getLocation(table2)
#'
#'
#' table3 <- -c(49991234, 49983454, 50013333, 49923333)
#' getLocation(table3)
#'
#'
#' table4 <- c(4220,4232,-6332,4578)
#' getLocation(table4)
#'
#' HairEyeColor
#' getLocation(HairEyeColor)






getLocation <- function(table, frac = 1.0){
  # function to check if it's an integer
  is.wholenumber <- function(x, tol = .Machine$double.eps)  abs(x - round(x)) < tol
  # function to count the number of decimal places
  countDecimalDigits <- function(x){
    digits <- 0
    x <- min(abs(x))
    while (abs(x-round(x, digits)) > .Machine$double.eps){
      digits <- digits + 1
    }
    digits
  }

  newTable <- table
  location <- 0
  numDecimals <- countDecimalDigits(table)
  again <- TRUE
  while (again) {
    result <- SciNotation(newTable, 1, round = FALSE)
    nums <- result$coefficient
    exps <- result$exponent
    expSpread <- cSpread(exps, frac)
    if (expSpread >= 2) {again <- FALSE}
    else {
      result <- SciNotation(newTable, 1 + expSpread, round = FALSE)
      exps <- min(result$exponent)
      nums <- result$coefficient * 10 ^ (result$exponent -  exps)
      numSpread <- cSpread(nums, frac)
      if (numSpread <= 2) {
        if (max(nums) > 0){
          delta <- min(nums) * 10 ^ (exps)
        } else {
          delta <- max(nums) * 10 ^ (exps)
          }
        if (abs(delta) > .Machine$double.eps) {
          location <-  location + delta
          newTable <- table - location
        } else {again <- FALSE}
      } else {again <- FALSE}
    }
  }
  # check last digit is zero
  # ... needs to be checked for location < 1

  if (location == 0) return(location)
 #print(location)
  if (is.wholenumber(location)) {
    wholeNum <- TRUE
    } else
    {
      wholeNum <- FALSE
      location <- trunc(location * 10 ^ numDecimals)
    }
  #if (is.wholenumber(location)) {
    # a non-zero "integer"
   if (location > 0) {
    location <- location - (location %% 10)
    } else {
      abslocation <- abs(location) - (abs(location) %% 10)
      location <- - abslocation
    }

   if(!wholeNum) {
     location <- location * 10 ^(- numDecimals)
   }
  return(location)
}