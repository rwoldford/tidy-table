## Extract each block's name and dimension of a high-dimension table
## Idea: Use table dimensions information to construct the block names as showed
## in console, in order to show them in laTex. Extract the dimension information of each block
## in order to call it in showtidytable().



#' @title Blocks information of tables with dimension >= 3.
#' @param table A sample of data with dimensions >= 3, can be a matrix, an array
#'   or a table.
#' @return a list of block names and dimension infomation to call each block.
#' @author R. Wayne Oldford and Xiaomei Yu
#' @examples
#' Titanic
#' arrayBlocks(Titanic)
#' Titanic[,,1,1] # the first row of $blockTablecom correponds to the first block
#' Titanic[,,1,2] # the third row of $blockTablecom correponds to the third block


arrayBlocks <- function(table){
  # to calculate blockTablecom automatically for any dimensions
  excom <- mapply(function(i){
                   paste('1:dim(table)[', i, ']')
                  },
                 3:length(dim(table))
                )
  # record the blocks' dimensions of the multi-way table (with n dimensions)
  # each row has three n-2 numbers, e.g. n=5, each row has 3 number,
  # say a,b,c, then use[,,a,b,c] we can call the corresponding block of the multi-way table.
  blockTablecom <- eval(parse(text = paste('expand.grid(',
                                            paste(excom, collapse=","),
                                            ')',
                                            sep=''
                                          )
                             )
                       )
  # record the block names
  blockNames <- mapply(function(i){
                  paste(',', ',', paste(mapply(function(j){
                                        if(is.null(names(dimnames(table)[2+j])) || is.na(names(dimnames(table)[2+j]))){
                                          if(is.null(dimnames(table)[2+j][[1]][blockTablecom[i,j]]) ||
                                            is.na(dimnames(table)[2+j][[1]][blockTablecom[i,j]]))
                                            {
                                            paste(blockTablecom[i,j])
                                            } else {
                                              paste(dimnames(table)[2+j][[1]][blockTablecom[i,j]])
                                              }
                                        } else {
                                          blockElements <- eval(parse(text=paste('dimnames(table)[2+',
                                                                                  j,
                                                                                 ']$',
                                                                                  names(dimnames(table)[2+j]),
                                                                                 '[',
                                                                                 eval(parse(text=paste('blockTablecom[',
                                                                                                       i,
                                                                                                       ',',
                                                                                                       j,
                                                                                                       ']',
                                                                                                       sep=''
                                                                                                      )
                                                                                            )
                                                                                     ),
                                                                                 ']',
                                                                                 sep=''
                                                                                )
                                                                      )
                                                               )
                                          if(is.null(blockElements) || is.na(blockElements)){
                                            paste(names(dimnames(table)[2+j]), '=')
                                          } else {
                                            paste(names(dimnames(table)[2+j]),
                                                  '=',
                                                  blockElements
                                                 )
                                          }
                                         }
                                       },
                                       1:dim(blockTablecom)[2]
                                      ),
                              collapse=","
                             )
                       )
                      },
                     1:dim(blockTablecom)[1]
                    )

  list(
    blockNames = blockNames,
    blockTablecom = blockTablecom)
}
