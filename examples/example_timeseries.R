#
# In this example we will prepare time series data and plot it to show a sequence.
#

# First, load the functions we need.
source("distance//getDistances.R")
source("process//calculateMDS.R")
source("process//calculateHclust.R")
source("plot//plotMDSTimeseries.R")
source("plot//plotDendrogramTimeseries.R")

#
# Let's start by creating some matrices. For the example here, we will directly create distance matrices.
#

# this is a function that creates a distance object
createDistObject = function(i) {
  distMatrix = matrix(log10(i+1)*abs(runif(n^2)), n, n)
  distMatrix = (distMatrix+t(distMatrix))/2 # make symmetric
  diag(distMatrix) = 0 # distance to self is 0
  return(as.dist(distMatrix))
}

n = 12 # number of entries
nPlots = 9 # number of time series

distList = lapply(1:nPlots, createDistObject) # create a list of dist objects

# create color labels, too.
library(RColorBrewer)
labels = list(colors=brewer.pal(n, "Paired")) # add colors to the pointLabels

## Part 1. MDS plots ##
#
# The plots require a list of data representations. 
# The lapply function comes handy in these cases and calculateMDS works well with lapply.
#
mdsList = lapply(distList, calculateMDS, 4)
plotMDSTimeseries(mdsList, labels) # spread the dots around

#
# Grouping by categories also works. However, we cannot use boxPlots for time series
# due to the problems with the layout function and par(mfrow).
#
nGroups = 4
labels$category = (1:n)%%nGroups + 1 # split the points into three groups
category = list(colors=brewer.pal(nGroups, "Set1"))

plotMDSTimeseries(mdsList, labels, category)

#
# We can use most parameters for plotMDS and apply it to all the plots. 
# Let's plot MDS dimensions 3 and 4.
#
plotMDSTimeseries(mdsList, labels,
                  dims=c(3,4))

#
# We can also add time labels.
#
timeLabels = sapply(1:nPlots, function(i){return(paste("Interval", i))})

plotMDSTimeseries(mdsList, labels, 
                  timeLabels = timeLabels,
                  dims=c(3,4)) # hmm...

#
# The layout isn't to nice...
# Let's hide the axis text and move the time labels
#
plotMDSTimeseries(mdsList, labels, 
                  timeLabels = timeLabels, timeLabelOffset=0.5,
                  dims=c(3,4), showAxisTitle=F)

#
# Adding label names.
#
labels$names = 1:n
plotMDSTimeseries(mdsList, labels, 
                  timeLabels = timeLabels, timeLabelOffset=0.5,
                  dims=c(3,4), showAxisTitle=F, 
                  pointScale=0, textAdjust=c(.5, .5))



## Part 2. Dendrograms ##
#
# Dendrograms follow the same flow.
#
denList = lapply(distList, calculateHclust, method="average")
plotDendrogramTimeseries(denList, labels)

#
# By default the dendrograms are all matched to have a maximum height of 1.
# We can set this to a different value if needed.
#
plotDendrogramTimeseries(denList, labels,
                         maxHeight=1.5)

#
# We can make each dendrogram have a different scale
#
plotDendrogramTimeseries(denList, labels,
                         maxHeight=NULL, axisSide=1)

#
# And parameters for plotDendrogram also works. Some stylizing...
#
plotDendrogramTimeseries(denList, labels,
                         type="triangle")