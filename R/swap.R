## Swap dimensions of a table
## Idea: Swap the dimensions of a table based on the variability (spread)
##       of dimensions and combinations of dimensions.
##       Here, we first find the two-dimension combination
##       with the smallest varaibility from all possible
##       two-dimension combinations, then those two dimensions are
##       the first two dimensions. Then for the third dimension,
##       we combine the first two dimensions with each of the left
##       dimensions to construct three-dimension combinations, then also
##       find the three-dimension combination with the smallest variability
##       The third dimension is just the third dimension except for the
##       first two dimensions of that three-dimension combination...
##       We continue to decide all the left dimensions' order in this way
##       until the last dimension. After we decide the general order of all dimensions,
##       we come back to decide within the first two dimensions that which one is the
##       rows and which one is columns, by compare the variability of combination of each of
##       the first two dimensions with the left n-2 dimensions, i.e. compare variability of
##       two combinations of n-1 dimensions


#' @title Swap dimensions of a table.
#' @param table A sample of data with dimensions >=2, can be a matrix, an array
#'   or a table.
#' @param chooseFun The summary to represent the variability (spread) of each
#'   dimension/combination of dimensions, can be functions in R or self-defined
#'   functions.
#' @param valueFun The summary to measure the variability (spread) of each layer
#'   of the dimension/combination of dimensions, can be functions in R or
#'   self-defined functions.
#' @param dimDecreasing Logical, if swap the dimensions in a way of spread
#'   decreasing.
#' @param preserveFirst2dim Fix the first 2 dimensions of the table to be a
#'   specific two dimensions of the table.
#' @return A list of the swapped table and the new dimension order after
#'   swapping
#' @author R. Wayne Oldford and Xiaomei Yu
#' @examples
#' table1<-c(97.62,92.24,100.90,90.39,48.29,42.31,49.98,39.09,75.23,75.16,100.11,74.23,49.69,57.21,80.19,51.09)
#' table1 <- matrix(table1, nrow=4,byrow=TRUE,dimnames=list(c("North" ,"South","East","West"),c("Q1","Q2","Q3","Q4")))
#' table1
#' swap(table1)
#'
#'
#' table2 <- c(.7999, .7998, .7998, .7997,
#' 3.7999,3.7824,3.7662,3.7223,
#' 0.3,1.2,4.9,145.7,
#' 20.799,20.699,20.899,145.699,
#' 35.3, 34.5,33.6,34.7)
#' table2 <- matrix(table2, ncol=4,byrow=TRUE,dimnames=list(c("A" ,"B","C","D","E"),c("1","2","3","4")))
#' swap(table2)
#'
#
#' UCBAdmissions
#' swap(UCBAdmissions)
#'
#' HairEyeColor
#' swap(HairEyeColor)
#'
#'
#' Titanic
#' swap(Titanic)







swap<-function(table,
               chooseFun = "median",
               valueFun = "mad",
               dimDecreasing = TRUE,
               preserveFirst2dim = NULL)
{
  dim <- length(dim(table)) # number of dimensions
  alldim <- 1:length(dim(table)) # all the dimensions
  # for two-way table
  if (dim == 2){
    # generate one-dimension combination in order to use findCombination() function
    # use findCombination() function to find the dimension combination w.r.t
    # the median value of the mad spread
    combs <- combn(length(dim(table)), 1)
    finalord <- findCombination(table, combs = combs,
                    chooseFun = chooseFun, valueFun = valueFun,
                    dimDecreasing = dimDecreasing)
    #set the dimension with smaller median value as the rows
    finalord <- c(alldim [! alldim %in% finalord], finalord)
  }
  # for multiway table, >2
  else{
    if(is.null(preserveFirst2dim) == TRUE){
      # generate two-dimension combination
      # in order to use findCombination() function
      combs <- combn(length(dim(table)), 2)
      finalord <- findCombination(table, combs = combs,
                                  chooseFun = chooseFun,valueFun = valueFun,
                                  dimDecreasing = dimDecreasing)
    }
    else{
      finalord <- preserveFirst2dim
    }
    dimleft <- dim - 2 # number of left dimensions
    # After choosing the first 2 dimensions, each time pick one dimension as
    # the following dimension, i.e., the 3rd one, then the 4th one,...
    # by choosing the dimension combination with the largest median value.
    while(dimleft > 1){
      ldim <- alldim [! alldim %in% finalord] # left dimensions
      # all n-dimension combinations including first n-1 dimensions
      combs <- mapply(function(i){
                       c(finalord, ldim[i])
                      },
                     1:length(ldim)
                    )
      finalord <- findCombination(table, combs = combs,
                                  chooseFun = chooseFun, valueFun = valueFun,
                                  dimDecreasing = dimDecreasing)
      dimleft <- dimleft-1
    }
    finalord <- c(finalord, alldim [! alldim %in% finalord]) # the left dimension as the last dimension
    # decide the order of the first two dimension
    # first calculate the spread of columns spread: colSpread
    coldim <- c(finalord[2:length(finalord)]) # all dimensions except for the row
    # automatically generate the dimensions' elements combination in order to use expand.grid() function
    excom <- mapply(function(i){
                     paste('1:dim(table)[coldim[',i,']]')
                    },
                   1:length(coldim)
                  )
    # use expand.grid() to generrate the dimensions' elements combination
    com <- eval(parse(text = paste('expand.grid(', paste(excom,collapse = ","), ')', sep = '')))
    coldim <- matrix(coldim, nrow = length(coldim)) # transform to matrix in for the count convenience
    # use choose value function to calculate the column spread, i.e. the median value of the mad spread
    colSpread <- chooseValue(table, chooseFun = chooseFun,
                             valueFun = valueFun, comleft = coldim,
                             com = com, column = "")
    # calculate the spread of rows spread: rowSpread
    rowdim <- c(finalord[1], finalord[3:length(finalord)])
    excom <- mapply(function(i){
                      paste('1:dim(table)[rowdim[', i, ']]')
                    },
                   1:length(rowdim)
                  )
    com <- eval(parse(text = paste('expand.grid(', paste(excom, collapse = ","), ')', sep = '')))
    rowdim <- matrix(rowdim, nrow = length(rowdim))
    rowSpread <- chooseValue(table, chooseFun = chooseFun,
                             valueFun = valueFun, comleft = rowdim,
                             com = com, column = "")
    # compare the row and column spread, choose the smaller one as the row in the swapped table
    if (rowSpread < colSpread) {
      finalord <- c(finalord[2], finalord[1], finalord[3:length(finalord)])
    }
  }

  list(
    swappedTable = aperm(table,finalord),
    newDimOrder = finalord
    )
}