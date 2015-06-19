#
# Plots MDS timeseries
#
source("plot//plotTimeseries.R")
source("plot//plotMDS.R")

plotMDSTimeseries = function(mdsSeries, labels=NULL, category=NULL, 
                             timeLabels=NULL, timeLabelOffset=1,
                             pointScale = 0.5, textAdjust = c(.5, .5),
                             ...) {
  
  plotTimeseries(plotMDS, mdsSeries, labels=labels, category=category, 
                 timeLabels, timeLabelOffset,
                 pointScale=pointScale, textAdjust = textAdjust,
                 boxPlots=c(0, 0), # force boxPlots off
                 ...)
}