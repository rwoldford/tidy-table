## Reorder within one dimension in a table
## Idea: Order the dimension layers in a certain order type (decreasing or increasing)
##       as determined by the orderSummary function.



#' @title Reorder the layers of one dimension
#' @param table A sample of data with dimensions >=1, can be a matrix, an array
#'   or a table.
#' @param summary A summary based on which we order within a dimension, can be
#'   functions in R or self-defined functions.
#' @param dim Which dimension do we want to order.
#' @param decreasing The order type, decreasing or increasing.
#' @return The reordered table.
#' @author R. Wayne Oldford and Xiaomei Yu
#' @examples
#' table2<-c(97.62,92.24,100.90,90.39,48.29,42.31,49.98,39.09,75.23,75.16,100.11,74.23,49.69,57.21,80.19,51.09)
#' table2 <- matrix(table2, nrow=4,byrow=TRUE,dimnames=list(c("North" ,"South","East","West"),c("Q1","Q2","Q3","Q4")))
#' table2
#' orderDim(table2, dim=1)
#'
#' UCBAdmissions
#' orderDim(UCBAdmissions, dim=2)
#'
#' HairEyeColor
#' orderDim(UCBAdmissions, dim=3)
#'
#' Titanic
#' orderDim(Titanic, dim=3)



orderDim <- function(table, summary = "mean", dim = 1, decreasing = TRUE){
  dims <- dim(table)
  num  <- length(dims)
  ord <- "order(mapply(function(x){do.call(summary, list(plyr::take(table, dim, x)))},
                       1:dims[dim]),
                decreasing = decreasing)"
  if (dim == 1){
    # First dimension, need to avoid generating a zero character
    newTable <-paste('table [',
      ord,
      paste(rep(",", num-1), collapse = ""),
      ']',
      sep = '')
  }
  else if (dim == num){
    # Last dimension, alsp need to avoid generating a zero character
    newTable <- paste('table [',
      paste(rep(",", num-1), collapse = ""),
      ord,
      ']',
      sep = '')
  }
  else{
    # All middle dimensions work the same
    newTable <- paste('table [',
      paste(rep(",", dim-1), collapse = ""),
      ord,
      paste(rep(",", num-dim), collapse = ""),
      ']',
      sep = '')
  }
  eval(parse(text = newTable))
}

