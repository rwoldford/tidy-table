## Produce a tidy table for tables with any possible dimensions,
## and display it in console or LaTex
## Use tidytable() to get a general tidy table first, Then according to
## the user's preference of order, swap and remove common zeros within
## each block of the tidy table or not, and show tidy tale in console or laTex .

#' @title Show (Print) tidy table in console or LaTex.
#' @param table A sample of data with dimensions >= 2, can be a matrix, an array
#'   or a table.
#' @param display To show the table in console or laTex.
#' @param tableCaption The caption of the table.
#' @param file If NULL, then the latex code will be shown in console. Otherwise,
#'   the code will be written to file (as in the latex command from the Hmisc
#'   package).
#' @param append Logical, append latex output to an existing file.
#' @param removeLocation Logical, detect and remove the location.
#' @param printLocation  Logical, print the location.
#' @param locationType Whether to remove and print the location within each
#'   two-way block.
#' @param locTrimFraction The fraction to be trimmed from each end of a sample
#'   to find the location.
#' @param fixedOrderDims A vector of those dimensions within which the layers
#'   cannot be reordered.
#' @param orderSummary The summary which we use to order the dimension layerss.
#' @param orderDecreasing Logical, if the order of dimension elements is
#'   decreasing.
#' @param orderType Whether to order the rows and columns within each two-way
#'   block.
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
#' @param swapType Whether to swap the rows and columns within each two-way
#'   block.
#' @param removeCommonZeros Logical, remove common rightmost zeros from all
#'   table entries.(Find a unit)
#' @param printUnit Logical, whether to print the location.
#' @param unitType Whether to remove common zeros and print unit within each
#'   two-way block.
#' @param nSig Number of digits showed in the table.
#' @param expsRange The threshold to allow how many numbers to be showed. If the
#'   spread of exponents of the table is smaller than expsRange, then all the
#'   numbers in the table will be showed in the new table. Otherwise, only those
#'   numbers whose exponents are within this expsRange will be displayed and we
#'   will try to display most of the numbers.
#' @return The tidy table in console or laTex format.
#' @author R. Wayne Oldford and Xiaomei Yu
#' @examples
#' table <- array(c(5*10^5,9*10^5,2*10^5,8*10^5,
#' 5*10^4,9*10^4,2*10^4,8*10^4,
#' 5*10^2,9*10^2,2*10^2,8*10^2,
#' 5*10^3,9*10^3,2*10^3,8*10^3,
#' 7*10^9,4*10^9,2*10^9,5*10^9,
#' 8*10^2,3*10^2,7*10^2,1*10^2,
#' 6*10^3,8*10^3,2*10^3,9*10^3,
#' 4*10^7,9*10^7,3*10^7,1*10^7
#' ), c(2,2,2,2,2))
#' dimnames(table) <- list(c("i1","i2"),c("j1","j2"),c("k1","k2"),c("l1","l2"),c("m1","m2"))
#' table
#' tidytable(table)
#' showtidytable(table,display="console",tableCaption="tidy table",
#' locationType="withinEachTwoWay", orderType="withinEachTwoWay",
#' swapType="withinEachTwoWay", unitType="withinEachTwoWay")
#'
#' showtidytable(table,display="latex",tableCaption="tidy table",
#' locationType="withinEachTwoWay", orderType="withinEachTwoWay",
#' swapType="withinEachTwoWay", unitType="withinEachTwoWay")
#'
#'
#' Titanic+264200
#' showtidytable(Titanic+264200,display="console",
#' tableCaption="Titanic tidy table", locationType="withinEachTwoWay",
#' orderType="withinEachTwoWay",swapType="withinEachTwoWay", unitType="withinEachTwoWay")
#'
#' showtidytable(Titanic+264200,display="latex",tableCaption="Titanic tidy table",
#'  locationType="withinEachTwoWay", orderType="withinEachTwoWay",
#'  swapType="withinEachTwoWay", unitType="withinEachTwoWay")




showtidytable <- function(table,
                          display = c("console", "latex"),
                          tableCaption = NULL,
                          file = NULL,
                          append=FALSE,
                          removeLocation = TRUE,
                          printLocation = TRUE,
                          locationType = c("common", "withinEachTwoWay"),
                          locTrimFraction = 0,
                          fixedOrderDims  = NULL,
                          orderSummary = "mean",
                          orderDecreasing = TRUE,
                          orderType = c("common", "withinEachTwoWay"),
                          swapDimensions = TRUE,
                          preserveFirst2dim = NULL,
                          swapDecreasing = TRUE,
                          chooseFun = "median",
                          valueFun = "mad",
                          swapType = c("common", "withinEachTwoWay"),
                          removeCommonZeros = TRUE,
                          printUnit = TRUE,
                          unitType = c("common", "withinEachTwoWay"),
                          nSig = 2,
                          expsRange = nSig - 2)
{
  display <- match.arg(display)
  locationType <- match.arg(locationType)
  orderType <- match.arg(orderType)
  swapType <- match.arg(swapType)
  unitType <- match.arg(unitType)

  tidytableInfo <- tidytable(table = table, removeLocation = removeLocation,
                             locTrimFraction = locTrimFraction, fixedOrderDims = fixedOrderDims,
                             orderSummary = orderSummary, orderDecreasing = orderDecreasing,
                             swapDimensions = swapDimensions, preserveFirst2dim = preserveFirst2dim,
                             swapDecreasing = swapDecreasing, chooseFun = chooseFun,
                             valueFun = valueFun, removeCommonZeros = removeCommonZeros,
                             nSig = nSig, expsRange = expsRange)

  dim <- length(dim(table)) # number of dimensions

  if (dim == 2 || (display == "console" && orderType == "common" && swapType == "common"
                   && locationType == "common" && unitType == "common"))
  {
    printTidy2way(tidytableInfo, display = display, tableCaption = tableCaption,
                  printUnit = printUnit, printLocation = printLocation,
                  file = file, append = append)
  } else {
    theoreticalTable <- tidytableInfo$table
    newDimOrder <- tidytableInfo$newDimOrder

    fixed2wayOrderDims <- NULL
    for (i in 1:2){
      if(newDimOrder[i]%in% fixedOrderDims){
        fixed2wayOrderDims <- c(fixed2wayOrderDims, i)
      }
    }

    if(orderType == "common"){
      fixed2wayOrderDims <- c(1,2)
    }

    if(swapType == "withinEachTwoWay"){
      swapBlocks <- TRUE
    } else {
      swapBlocks <- FALSE
    }

    if(locationType == "withinEachTwoWay"){
      removeBlockLocation <- TRUE
    } else {
      removeBlockLocation <- FALSE
    }

    if(unitType == "withinEachTwoWay"){
      removeBlockZeros <- TRUE
    } else {
      removeBlockZeros <- FALSE
    }

    arrayBlocksInfo <- arrayBlocks(theoreticalTable)
    blockTablecom <- arrayBlocksInfo$blockTablecom
    blockNames <- arrayBlocksInfo$blockNames


    overallInfo <- NULL
    tidyBlockInfo <- mapply(function(i){
    eval(parse(text=paste('tidytable(
                            theoreticalTable[,,', paste(blockTablecom[i, ], collapse=','), '],',
                            'removeLocation = removeBlockLocation, locTrimFraction = locTrimFraction,
                            fixedOrderDims = fixed2wayOrderDims, orderSummary = orderSummary,
                            orderDecreasing = orderDecreasing, swapDimensions = swapBlocks,
                            swapDecreasing = swapDecreasing, chooseFun = chooseFun,
                            valueFun = valueFun, removeCommonZeros = removeBlockZeros,
                            nSig = nSig, expsRange = expsRange',
                          ')'
                        )
                )
           )
      },
      1:dim(blockTablecom)[1]
    )

      if (printUnit) {
        overallUnit <- tidytableInfo$units
        if(unitType == "common"){
          overallInfo <- paste("Unit: ", overallUnit)
          printBlockUnit <- FALSE
        } else {
          unitInfo <- tidyBlockInfo["units",]

          if (length(unique(unitInfo)) == 1) {
            overallUnit <- overallUnit * unitInfo[[1]]
            overallInfo <- paste(overallInfo, "Unit: ", overallUnit)
            printBlockUnit <- FALSE
          } else {
            unitInfo <- mapply(function(i){unitInfo[[i]] * overallUnit},
                               1:length(unitInfo))
            tidyBlockInfo["units",] <- unitInfo
            printBlockUnit <- TRUE
          }
        }
      }

    if (printLocation) {
      overallLocation <- tidytableInfo$location
      if(locationType == "common"){
        overallInfo <- paste(overallInfo, "Location: ", overallLocation)
        printBlockLocation <- FALSE
      } else {
        locationInfo <- tidyBlockInfo["location",]

        if (length(unique(locationInfo)) == 1) {
          overallLocation <- overallLocation + locationInfo[[1]]*tidytableInfo$units
          overallInfo <- paste(overallInfo, "Location: ", overallLocation)
          printBlockLocation <- FALSE
        } else {
          locationInfo <- mapply(function(i){
            locationInfo[[i]] * tidytableInfo$units + overallLocation
          },
            1:length(locationInfo))
          tidyBlockInfo["location", ] <- locationInfo
          printBlockLocation <- TRUE
        }
      }
    }

      if (display == "console"){
        if(is.null(tableCaption)==FALSE){
          cat(tableCaption,"\n")

        }
        if(is.null(overallInfo)==FALSE){
          cat(overallInfo,"\n")
          cat("\n")
        }
      } else {
        if(is.null(tableCaption)==FALSE){
          if (is.null(file)==FALSE){
            sink(file=file)
          }
          cat(paste("\\begin{center}"), "\n")
          cat(tableCaption,"\n")
          cat(paste("\\end{center}"), "\n")
          if (is.null(file)==FALSE){
            sink()
          }
        }
        if(is.null(overallInfo)==FALSE){
          if (is.null(file)==FALSE){
            sink(file=file)
          }
          cat(overallInfo,"\n")
          if (is.null(file)==FALSE){
            sink()
          }
        }
        }


       for (i in 1:dim(blockTablecom)[1]) {
        eval(parse(text=paste('printTidy2way(
                                   tidyBlockInfo [ ,', i, '],',
                                   'display = display, tableCaption = blockNames[i],
                                    printUnit = printBlockUnit, printLocation = printBlockLocation,
                                    file = file, append = append',
                               ')'
                             )
                  )
           )
         if (display == "console"){
         cat("\n")
         }
         }
  }
}





