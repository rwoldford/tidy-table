## Generate a tidy table based on initial input table
## Idea: Firstly check if there is a location, if there is, remove location
##       Secondly order within dimensions.
##       Thirdly swap the dimensions in the table if possible.
##       Lastly, round to two significant digits and get a unit to
##       show most numbers in the table and remove rudundant common zeros.
##       The user can choose all or some of the above steps to analyze


#' @title Generate a tidy table.
#' @param table A sample of data with dimensions >=2, can be a matrix, an array
#'   or a table.
#' @param removeLocation Logical, detect and remove the location.
#' @param locTrimFraction The fraction to be trimmed from each end of a sample
#'   to find the location.
#' @param fixedOrderDims A vector of dimensions within which elements
#'   cannot be reordered.
#' @param orderSummary The summary which we use to order the dimension layers.
#' @param orderDecreasing Logical, if the order of dimension elements is
#'   decreasing.
#' @param swapDimensions Logical, reorder dimensions (swap).
#' @param preserveFirst2dim Fix the first 2 dimensions of the table to be a
#'   specific two dimensions of the table.
#' @param swapDecreasing Logical, if swap the dimensions in a way of spread
#'   decreasing.
#' @param chooseFun The summary to represent the variability (spread) of each
#'   dimension/combination of dimensions, can be functions in R or self-defined
#'   functions.
#' @param valueFun The summary to measure the variability (spread) of each layer
#'   of the dimension/combination of dimensions, can be functions in R or
#'   self-defined functions.
#' @param removeCommonZeros Logical, remove common rightmost zeros from all
#'   table entries. (Find a unit.)
#' @param nSig Number of digits showed in the table.
#' @param  expsRange The range of exponents within which
#'         we hope to show most of the values,  default is nSig-1
#' @return A list containing the named components:
#'         tidytable The tidy table,
#'         originalTable The original table,
#'         location The location removed from the original table,
#'         nSig Number of digits shown in the tidy table,
#'         units The units of the entries in the tidy table,
#'         newDimOrder The new dimension order after swapping the table.
#' @author R. Wayne Oldford and Xiaomei Yu
#' @examples
#' table1<-c(97.62,92.24,100.90,90.39,48.29,42.31,49.98,39.09,75.23,75.16,100.11,74.23,49.69,57.21,80.19,51.09)
#' table1 <- matrix(table1, nrow=4,byrow=TRUE,dimnames=list(c("North" ,"South","East","West"),c("Q1","Q2","Q3","Q4")))
#' table1
#' tidytable(table1)
#'
#' table2 <- c(.7999, .7998, .7998, .7997,
#' 3.7999,3.7824,3.7662,3.7223,
#' 0.3,1.2,4.9,145.7,
#' 20.799,20.699,20.899,145.699,
#' 35.3, 34.5,33.6,34.7)
#' table2 <- matrix(table2, ncol=4,byrow=TRUE,dimnames=list(c("A" ,"B","C","D","E"),c("1","2","3","4")))
#' tidytable(table2)
#'
#'
#' UCBAdmissions
#' tidytable(UCBAdmissions)
#'
#' HairEyeColor
#' tidytable(HairEyeColor)
#'
#'
#' Titanic
#' tidytable(Titanic)

tidytable <- function( table,
                       removeLocation = TRUE,
                       locTrimFraction = 0,
                       fixedOrderDims  = NULL,
                       orderSummary = "mean",
                       orderDecreasing = TRUE,
                       swapDimensions = TRUE,
                       preserveFirst2dim = NULL,
                       swapDecreasing = TRUE,
                       chooseFun = "median",
                       valueFun = "mad",
                       removeCommonZeros = TRUE,
                       nSig = 2,
                       expsRange = nSig-1)
{
  # Should we remove a location?
  if(removeLocation)
  {
    location  <- getLocation(table,frac=(1 - 2 * locTrimFraction))
    tidytable <- table - location
  } else
  {
    tidytable <- table
    location <- 0
  }

  # Reorder values within any dimension not appearing in fixedOrderDims
  fixedOrderDims <- unique(fixedOrderDims)
  dims <- 1:length(dim(tidytable))
  dimsToReorder <-  dims [! dims %in%  fixedOrderDims]

  if(length(dimsToReorder) > 0){
    # Need to reorder the dimensions
    # Reorder each dimension in decreasing order (orderDecreasing = TRUE)
    # as determined by the orderSummary function.
    for (i in 1:length(dimsToReorder)){
      tidytable <- orderDim(tidytable,
                            summary = orderSummary,
                            dim = dimsToReorder[i],
                            decreasing = orderDecreasing)
    }
  }

  # Swap dimensions?
  if(swapDimensions){
    swapInfo <- swap(tidytable,
                     chooseFun = chooseFun,
                     valueFun = valueFun,
                     dimDecreasing = swapDecreasing,
                     preserveFirst2dim = preserveFirst2dim)
    tidytable <- swapInfo$swappedTable
    newDimOrder<-swapInfo$newDimOrder
  } else
  {
    newDimOrder <- 1:length(dim(tidytable))
  }

  # Remove as many zeros from the right by
  # determining the common power of 10
  if(removeCommonZeros){
    basetable <- focusTable(tidytable, nSig = nSig, expsRange = expsRange)
    tidytable <- basetable$table
    base <- basetable$base
    scipen <- basetable$scipen
  } else
    {
      base <- 0
      scipen <- NULL
      }

  list(
    table = tidytable,
    units = 10 ^ base,
    location = location,
    nSig = nSig,
    scipen=scipen,
    newDimOrder = newDimOrder,
    originalTable = table
    )
}
