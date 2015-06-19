#
# Plots MDS timeseries
#
source("plot//plotTimeseries.R")
source("plot//plotDendrogram.R")

plotDendrogramTimeseries = function(hclustSeries, labels=NULL, category=NULL, 
                                    timeLabels=NULL, timeLabelOffset=1, maxHeight=1,
                                    ...) {
  
  # TODO: implement show scales for only one plot
  
  plotTimeseries(plotDendrogram, hclustSeries, labels, category,
                 timeLabels, timeLabelOffset, maxHeight=maxHeight,
                 ...)
}