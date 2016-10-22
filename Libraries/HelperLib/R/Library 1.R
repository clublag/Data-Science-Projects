#Helper functions

#' Import CSV
#'
#' This function allows you to import a CSV.
#' Default input is comma separated value, and first row as headers. 
#' @param filename
#' @keywords 
#' @export
#' @examples
#' import.csv()
import.csv <- function(filename) {
  if (!require("utils",character.only = TRUE))
  {
    install.packages("utils",dep=TRUE)
    if(!require("utils",character.only = TRUE)) stop("Package not found")
  }
  return(read.csv(filename, sep = ",", header = TRUE))
}

#' Import XLS
#'
#' This function allows you to import a XLS.
#' Defeault input is that data is in first sheet.
#' @param filename
#' @keywords 
#' @export
#' @examples
#' import.xls()
import.xls <- function(filename) {
  if (!require("xlsReadWrite",character.only = TRUE))
  {
    install.packages("xlsReadWrite",dep=TRUE)
    if(!require("xlsReadWrite",character.only = TRUE)) stop("Package not found")
  }  
  return(read.xls(filename, sheet=1))
}