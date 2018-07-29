## Print the tidytable information in console or latex format;
## if console format is choosen, it could print high-dimensional
## tidytable information, otherwise it could only deal with
## two-dimensional table due to the limit of Hmisc::latex() function.

#' @title Print multi-way tidytable information in console or
#'   two-way tidytable information in latex format.
#' @param tidytableInfo A sample of data with dimensions >= 2, can be a matrix,
#'   an array or a table. If display choosen to be "latex", then dimension of
#'   the data must be = 2.
#' @param display To show the table in console or laTex.
#' @param tableCaption The caption of the table.
#' @param printUnit Logical, Print the unit of the tidytable.
#' @param printLocation Logical, Print the location of the tidytable.
#' @param file If NULL, then the latex code will be shown in console. Otherwise,
#'   the code will be written to file (as in the latex command from the Hmisc
#'   package).
#' @param append Logical, append latex output to an existing file.
#' @note printTidy2way() could print tidy table for multi-way tables in R
#'   console, but it can only print tidy table for two-way tables in latex code
#'   due to the limit of Hmisc::latex() function we used.
#' @author R. Wayne Oldford and Xiaomei Yu
#' @seealso \code{\link[Hmisc]{latex}} for print two dimensional table in latex
#'   format.
#' @examples
#' table1<-c(97.62,92.24,100.90,90.39,48.29,42.31,49.98,39.09,75.23,75.16,100.11,74.23,49.69,57.21,80.19,51.09)
#' table1 <- matrix(table1, nrow=4,byrow=TRUE,dimnames=list(c("North" ,"South","East","West"),c("Q1","Q2","Q3","Q4")))
#' table1
#' printTidy2way(tidytable(table1), display="console", tableCaption="Sales data")
#' printTidy2way(tidytable(table1), display="latex", tableCaption="Sales data")
#'
#'
#' table2 <- c(.7999, .7998, .7998, .7997,
#' 3.7999,3.7824,3.7662,3.7223,
#' 0.3,1.2,4.9,145.7,
#' 20.799,20.699,20.899,145.699,
#' 35.3, 34.5,33.6,34.7)
#' table2 <- matrix(table2, ncol=4,byrow=TRUE,dimnames=list(c("A" ,"B","C","D","E"),c("1","2","3","4")))
#' printTidy2way(tidytable(table2), display="console")
#' printTidy2way(tidytable(table2), display="latex")
#'
#'
#' UCBAdmissions
#' printTidy2way(tidytable(UCBAdmissions),display="console")
#'
#' HairEyeColor
#' printTidy2way(tidytable(HairEyeColor),display="console")
#'
#'
#' Titanic
#' printTidy2way(tidytable(Titanic),display="console")



printTidy2way <- function( tidytableInfo,
                           display = c("console", "latex"),
                           tableCaption = NULL,
                           printUnit = TRUE,
                           printLocation = TRUE,
                           file = NULL,
                           append=FALSE
                           )
{
  display <- match.arg(display)

  op <- options()
  if (is.null(tidytableInfo$scipen)==FALSE){
    options(scipen=tidytableInfo$scipen)
  }

  switch(display,
    console = {if(is.null(tableCaption)==FALSE){
                  cat(tableCaption,"\n")
                  }
              if(printUnit && printLocation == TRUE){
                  cat("Unit: ", tidytableInfo$units,
                      " ",
                      "Location: ", tidytableInfo$location,
                      "\n")
              } else {
                if (printUnit) {
                  cat("Unit: ", tidytableInfo$units, "\n")
                } else if (printLocation) {
                  cat("Location: ", tidytableInfo$location, "\n")
                }
              }

              print(tidytableInfo$table)
    },
    latex = {if (printUnit && printLocation == TRUE) {
               insertTop <- paste("Unit: ", tidytableInfo$units,
                                   " ",
                                   "Location: ", tidytableInfo$location)
             } else {
               if (printUnit) {
                   insertTop <- paste("Unit: ", tidytableInfo$units)
                 } else if (printLocation) {
                   insertTop <- paste("Location: ", tidytableInfo$location)
                 } else {
                   insertTop <- NULL
                 }
               }

             if(is.null(file)){
                 file = ''
               }

             if(is.null(names(dimnames(tidytableInfo$tidytable)[1]))){
                 rowlabel <- ''
               } else {
                 rowlabel <- names(dimnames(tidytableInfo$tidytable)[1])
               }

             Hmisc::latex(tidytableInfo$table, file = file, rowlabel = rowlabel,
                   cgroup = names(dimnames(tidytableInfo$tidytable)[2]),
                   append = append, caption = tableCaption,
                   where = "hbtp",insert.top = insertTop)
    },
    stop("Wrong 'display'")
  )
  options(op)
}
