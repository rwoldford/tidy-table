## Get a dimension summary of those  layer summaries get from each layer of a dimension
## Idea: Apply valueFun to each layer of the dimension
##       (or combination of several dimensions), then use chooseFun
##       to get a summary value form those values get from each layer


#' @title Get a summary of a dimension/combination of dimensions.
#' @param table A sample of data with dimensions >=2, can be a matrix, an array
#'   or a table.
#' @param chooseFun The summary to represent the variability (spread) of each
#'   dimension/combination of dimensions, can be functions in R or self-defined
#'   functions.
#' @param valueFun The summary to measure the variability (spread) of each layer of the
#'   dimension/combination of dimensions, can be functions in R or
#'   self-defined functions.
#' @param comleft A matrix of combinations of dimensions, each column stores the
#'   dimensions of a combination.
#' @param com A matrix of all the layer combinations of the dimensions in the combination
#'   combination. Each column stores a combination of layers e.g. The dimensions in a
#'   combination is (2,4,1), if the first column of com is (1,2,4), this means
#'   the layer combination is the first layer of the second dimension and
#'   the second layer of the fourth dimension and the fourth layer of the first
#'   dimension.
#' @param column The column of comleft which we want to apply our chooseValue()
#'   to, the default is 1.
#' @return The summary of the dimension/combination of dimensions
#' @author R. Wayne Oldford and Xiaomei Yu
#' @note Here, the default of chooseFun is "median" and the default of valueFun
#'   is "mad" since in our tidytable package we define the varaibility of a dimension
#'   (combination of dimensions) as the median of mad values get from each layer
#'   of that dimension (combination).
#' @examples
#' table <- Titanic
#' comleft <- combn(length(dim(table)), 2) # all the combinations of two dimensions
#' comleft <- matrix(comleft, ncol = dim(comleft)[2])
#' # Get the summary of the second combination (second column of comleft), i.e. (1,3)
#' # use expand.grid() to get all the layer combinations
#  # of each combination of remaining dimensions
#' excom <- mapply(function(i){
#' paste('1:dim(table)[comleft[', i, ',2]]')
#' },
#' 1:dim(comleft)[1]
#' )
#' com <- eval(parse(text = paste('expand.grid(',
#' paste(excom, collapse = ","),
#' ')',
#' sep = ''
#' )
#' )
#' )
#' chooseValue(table, chooseFun = "median", valueFun="mad", comleft, com, column=2)
#'




chooseValue <- function(table, chooseFun = "median", valueFun="mad",
                        comleft=NULL, com=NULL, column=1)
{
  if (is.null(comleft)) stop("comleft must be passed a matrix of combinations of several dimensions")
  if (is.null(com)) stop("com must be passed a matrix of all possible combinations of dimensions' layers")
  do.call(chooseFun, # apply the choose function to all the combinations of those dimensions.
    args = list((mapply(function(i){
                          # automatically generate the components inside the take() function
                          itake<-mapply(function(j){
                                        paste(# indices for the jth dimension
                                              'comleft', '[', j, ',', column, ']',
                                              ',',
                                              # indices for which layer to choose of the jth dimension
                                              'com', '[i,', j, ']'
                                             )
                                        },
                                       1:dim(comleft)[1] # number of dimensions in each combination
                                      )
                           n<-dim(comleft)[1]
                           # apply the value function to a combination of several dimensions' layers
                           do.call(valueFun,
                                   args = list(eval(parse(
                                                      text = paste(paste(rep("plyr::take(",n), collapse=""),
                                                      # automatically generate enough take function
                                                      # in order to extract that combination of layers
                                                      'table,',
                                                      paste(itake, collapse = "),"),
                                                      ')',
                                                      sep = '')
                                                     )
                                                    )
                                              )
                                 )
                          },
                         1:dim(com)[1] # total number of combinations of dimensions' layers
                        )
                  )
                )
      )
}

