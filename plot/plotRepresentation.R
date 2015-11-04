# 
# Plots the data representation. This function chooses the appropriate function by looking at dataRep$method.
# 
plotRepresentation = function(dataRep, labels=NULL, category=NULL, ...) {
  if (dataRep$method == "mds") {
    
    source("plot//plotMDS.R")
    plotMDS(dataRep$data, labels, category, ...)
  }
  else if (dataRep$method == "hclust") {
    
    source("plot//plotDendrogram.R")
    plotDendrogram(dataRep$data, labels, category, ...)
  }
}