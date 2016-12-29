#Project 1 - House Rent value prediction
setwd("H:/Workstuff/Data Science - Johns Hopkins/All Things Data Science/Data-Science-Projects/Projects/Project 1 - House Rent Prediction")
filename = "house_no_missing.csv"


MainDF = import.csv(filename)

##### Rough

SymbolicColumnVector = data.frame(sapply(MainDF,function(x) !is.numeric(x)))
SymbolicIndices = which(SymbolicColumnVector[1:nrow(SymbolicColumnVector),]=="TRUE")
SymbolicIndicesNames = sapply(SymbolicIndices, function(x) colnames(MainDF)[x])
  
CharDF = data.frame(MainDF[,SymbolicIndices])
colnames(CharDF) = SymbolicIndicesNames


sapply(CharDF, function(x) table(x))

##### Rough 