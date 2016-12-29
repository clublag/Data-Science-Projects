#Helper functions

#' CheckPackageANDInstall
#'
#' Check whether package exists, otherwise install.
#' @param packageName
#' @keywords 
#' @export
#' @examples
#' ChkPkgANDInstall()
ChkPkgANDInstall <- function(packageName) {
  if (!require(packageName,character.only = TRUE)) {
      install.packages(packageName,dep=TRUE)
      if(!require(packageName,character.only = TRUE)) stop("Package not found")
  }
}

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
  ChkPkgANDInstall("utils")
  return(read.csv(filename, sep = ",", header = TRUE))
}

#' Import XLS (TEMPORARILY NOT WORKING)
#'
#' This function allows you to import a XLS.
#' Defeault input is that data is in first sheet.
#' @param filename
#' @keywords 
#' @export
#' @examples
#' import.xls()
import.xls <- function(filename) {
  ChkPkgANDInstall("xlsx")
  return(read.xlsx(filename, sheet=1))
}

#' Describe Dataset
#'
#' This function describes the columns in the dataset. Dataframe is sent as the input
#' 
#' @param df
#' @keywords 
#' @export
#' @examples
#' describeData()
describeData <- function(df) {
  cat(sprintf("This dataset has %i Rows %i Attributes \n",nrow(df),ncol(df)))
  cat('real valued attributes \n')
  cat('---------------------- \n')
  
  #Extract only numeric columns from df
  NumDF = df[,sapply(df,is.numeric)]
  
  #Extract missing values for all columns
  MissingCount = data.frame(count=sapply(NumDF, function(x) sum(is.na(x))+length(which(x==""))))[,1]
  
  #Extract summary metrics
  MeanVector = data.frame(sapply(NumDF, function(x) sprintf("%.2f",mean(x,na.rm=TRUE))))[,1]
  if(class(MeanVector)=="factor") {
    MeanVector = as.numeric(as.character(MeanVector))
  }
  MedianVector = data.frame(sapply(NumDF, function(x) sprintf("%.2f",median(x,na.rm=TRUE))))[,1]
  if(class(MedianVector)=="factor") {
    MedianVector = as.numeric(as.character(MedianVector))
  }  
  SDVector = data.frame(sapply(NumDF, function(x) sprintf("%.2f",sd(x,na.rm=TRUE))))[,1]
  if(class(SDVector)=="factor") {
    SDVector = as.numeric(as.character(SDVector))
  }  
  MinVector = data.frame(sapply(NumDF, function(x) sprintf("%.2f",min(x,na.rm=TRUE))))[,1]
  if(class(MinVector)=="factor") {
    MinVector = as.numeric(as.character(MinVector))
  }  
  MaxVector = data.frame(sapply(NumDF, function(x) sprintf("%.2f",max(x,na.rm=TRUE))))[,1]
  if(class(MaxVector)=="factor") {
    MaxVector = as.numeric(as.character(MaxVector))
  }  
  
  # summarizing real-valued attributes in a data frame
  print(data.frame(
                    Attribute_name=colnames(NumDF),
                    Missing=MissingCount,
                    Mean=MeanVector,
                    Median=MedianVector,
                    Sdev=SDVector,
                    Min=MinVector,
                    Max=MaxVector             
  ))  
  cat('\n symbolic attributes \n')
  cat('---------------------- \n')

  #Extract only symbolic columns from df
  SymbolicColumnVector = data.frame(sapply(df,function(x) !is.numeric(x)))
  SymbolicIndices = which(SymbolicColumnVector[1:nrow(SymbolicColumnVector),]=="TRUE")
  SymbolicIndicesNames = sapply(SymbolicIndices, function(x) colnames(df)[x])
  
  #Converting result into a data framem and fixing column names
  CharDF = data.frame(df[,SymbolicIndices])
  colnames(CharDF) = SymbolicIndicesNames

  #extract missing count
  MissingCount = data.frame(count=sapply(CharDF, function(x) sum(is.na(x))+length(which(x==""))))[,1]
  
  #extract Arity
  ArityCount = data.frame(count=sapply(CharDF, function(x) length(levels(x))))[,1]
      
  # summarizing symbolic attributes in a data frame
  return(data.frame(
                      Attribute_name=colnames(CharDF),
                      Missing=MissingCount,
                      Arity=ArityCount
  ))

}

