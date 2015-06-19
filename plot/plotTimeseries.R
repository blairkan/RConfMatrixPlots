#
# Abstract worker function for plotting time-series data
#
# Args:
#   plotFun: requires function with prototype plotFun = function(dataRep, labels, category=NULL)
#   seriesData: a list of data representations for the plot.
#   labels: a list of label variables. possible variables are 
#     labels$names: the text labels of the elements.
#     labels$colors: the color for the text labels.
#     labels$images: the images for the elements.
#     labels$category: the category for the elements.
#   category: a list of category related labels
#     category$names: the text labels of the categories.
#     category$colors: the color for the categories.
#   timeLabels: a list of labels for each time series plot.
#   timeLabels.offset: adjusts the horizontal position of the time label.
#
# Returns:
#   Nothing.
#
plotTimeseries = function(plotFun, seriesData, labels=NULL, category=NULL, timeLabels=NULL, timeLabels.offset=1,  
                          ...)
{
  nPlots = length(seriesData) # get number of datums
  layout(t(1:nPlots))
  
  # TODO: implement show scales/axis for only one plot
  
  for (iPlot in 1:nPlots) {
    # plot
    plotFun(seriesData[[iPlot]], labels, category, 
            ...)
    
    # time title
    if (!is.null(timeLabels))
      title(sub=timeLabels[iPlot], line=timeLabels.offset)
  }
}