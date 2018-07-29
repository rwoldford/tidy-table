## Find a combination of several dimensions
## Idea: Order combinations of dimensions in a decreasing
##       (or increasing) order as determined by the values
##       get form chooseValue() function. Then find
##       the combination which is ordered first


#' @title Find a dimension/combination of dimensions with certain conditions.
#' @param table A sample of data with dimensions >=2, can be a matrix, an array
#'   or a table.
#' @param combs A matrix of dimension combinations, each column stores the
#'   dimensions of a combination.
#' @param chooseFun The summary to represent the variability (spread) of each
#'   dimension/combination of dimensions, can be functions in R or self-defined
#'   functions.
#' @param valueFun The summary to measure the variability (spread) of each layer
#'   of the dimension/combination of dimensions, can be functions in R or
#'   self-defined functions.
#' @param dimDecreasing Logical, if order the dimensions/combinations in a
#'   decreasing order.
#' @return The dimension/combination of dimensions which is ordered first.
#' @author R. Wayne Oldford and Xiaomei Yu
#' @note Here, the default of chooseFun is "median" and the default of valueFun
#'   is "mad" since in our tidytable package we define the varaibility of a
#'   dimension (combination of dimensions) as the median of mad values get from
#'   each layer of that dimension (combination).
#' @examples
#' table <- Titanic
#' coms <- combn(length(dim(table)), 2) # all the combinations of two dimensions
#' findCombination(table, coms, chooseFun = "median", valueFun = "mad", dimDecreasing = TRUE)
#' # so the combination of first two dimensions has the largest variability
#' # among all combinations of two dimensions in Titanic data



findCombination <- function(table,
                            combs = "NULL",
                            chooseFun = "median",
                            valueFun = "mad",
                            dimDecreasing = TRUE)
{
  if (is.null(combs)) stop("combs must be passed a matrix of combinations of n dimensions")
  alldim <- 1:length(dim(table)) # all the dimensions of the table
  comleft <- mapply(function(i){ alldim [! alldim %in% combs[,i]]},
                    1:dim(combs)[2]
                   ) # the remaining dimensions combinations corresponding to each n-dimension combination
  comleft <- matrix(comleft, ncol = dim(combs)[2]) # for the count convienience
  ordleft <- order(mapply(function(num){
                              # get all the layer combinations of each remaining dimensions combination,
                              # in order to use expand.grid() function
                              excom <- mapply(function(i){
                                               paste('1:dim(table)[comleft[', i, ',num]]')
                                               },
                                              1:dim(comleft)[1]
                                             )
                              # use expand.grid() to get all the layer combinations
                              # of each combination of remaining dimensions
                              com <- eval(parse(text = paste('expand.grid(',
                                                          paste(excom, collapse = ","),
                                                          ')',
                                                          sep = ''
                                                          )
                                              )
                                        )
                               # use chooseValue function to calculate the choosed value
                               # of each combination of remaining dimensions
                               chooseValue(table, chooseFun = chooseFun,
                                           valueFun = valueFun, comleft = comleft,
                                           com = com, column = num)
                               },
                               1:dim(comleft)[2]
                          ),
                   decreasing = dimDecreasing # order these dimension combinations
                   )
  finalord <- combs[ , ordleft[1]]
  finalord
}
