#
# This is a simple example for plotting a confusion matrix 
# to an MDS plot and dendrogram.
#
# Here we look at a simple example where we create a sample confusion matrix and some labels for the entries, 
# then use this sample matrix to get MDS and dendrogram plots with default settings.
# The basic work flow is: 
# 0. load and format your data into a confusion matrix
# 1. transform the confusion matrix into a dist object
# 2. transform the dist object into a data representation for the specific plot (MDS, dendrogram)
# 3. plot the data representation
#
# For more detailed use, see example_MDS.R, example_dendrogram.R
#

# Load functions to use
source("distance//normalizeMatrix.R")
source("distance//symmetrizeMatrix.R")
source("distance//getDistances.R")
source("process//calculateMDS.R")
source("process//calculateHclust.R")
source("plot//plotMDS.R")
source("plot//plotDendrogram.R")

# 0. Create example confusion matrix and labels
n = 6
confMatrix = matrix(abs(runif(n^2)), n, n) # square matrix with uniform distribution
# confMatrix = confMatrix/rowSums(confMatrix) # normalize rows

labels = list(names = 1:n,
              colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#bcbd22")) # pretty colors!


## 1. Get distance matrix from confusion matrix
normMatrix = normalizeMatrix(confMatrix) # normalize
symmMatrix = symmetrizeMatrix(normMatrix) # symmetrize
dstMatrix = getDistances(symmMatrix) # make distance


## Plot MDS
# 2. Get MDS representation
mdsRep = calculateMDS(dstMatrix)
# 3. Plot MDS
plotMDS(mdsRep, labels)


## Plot Dendrogram
# 2. Get hierarchical clustering representation
denRep = calculateHclust(dstMatrix)
# 3. Plot dendrogram
plotDendrogram(denRep, labels)