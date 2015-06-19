#
# In this example we will start with the default MDS plot, 
# then gradually add more structure to the data (images, categories)
# and make use of the advance functions of plotDendrogram
#

# First, load the functions we need.
source("distance//getDistances.R")
source("process//calculateHclust.R")
source("plot//plotDendrogram.R")

## Part 0. Setup ##
#
# Let's start by creating an example confusion matrix.
#
n = 12
confMatrix = matrix(abs(rnorm(n^2)), n, n) # square matrix with normal distribution
confMatrix = sweep(confMatrix, 2, rowSums(confMatrix), `/`) # normalize rows
# image(confMatrix, xaxt="n", yaxt="n") # a very confused matrix

#
# Next, let's transform this into a distance matrix,
# then to a dendrogram(Hclust) representation.
#
dst = getDistances(confMatrix)
# image(as.matrix(dst), xaxt="n", yaxt="n") # a symmetrically distant matrix
denRep = calculateHclust(dst, method="ward") # calculate using Ward's method for linkage

#
# And let's plot the data
#
plotDendrogram(denRep) # tree with branches



## Part 1. Color/Text ##
#
# In this part, let's look at ways of stylizing the dendrogram plot.
# First, let's add labels to each point. 
# To do this, we need to create a list object that contains the name of the points.
#
pointNames = 1:n
pointLabels = list(names=pointNames)
plotDendrogram(denRep, pointLabels) # four, one, six, ...

#
# Next, to add color to the points, we add an array of colors to pointLabels.
# We will use the RColorBrewer package for a nice palette.
#
# install.packages("RColorBrewer") # run this line if you don't have RColorBrewer installed.
library(RColorBrewer)
pointLabels$colors = brewer.pal(n, "Paired") # add colors to the pointLabels
plotDendrogram(denRep, pointLabels) # wow much colors, very satisfaction!

#
# We can show the axis scale to put things into perspective.
# 
plotDendrogram(denRep, pointLabels, 
               axisSide=1) # on this side

plotDendrogram(denRep, pointLabels, 
               axisSide=-1) # or that

#
# And we can scale the text labels and the axis using textScale and axisScale
#
plotDendrogram(denRep, pointLabels, 
               axisSide=-1, 
               axisScale=0.75, textScale=1.25)

#
# Finally, we can set the maximum height to a fixed value.
# This comes handy when comparing different confusion matrices.
#
plotDendrogram(denRep, pointLabels, 
               maxHeight=1.5,
               axisSide=-1, 
               axisScale=0.75, textScale=1.25)



## Part 1. Text Stylizing ##
#
# We can move the text to go along the branches. This is nice for longer node names.
#
pointLabels$names = sapply(1:n, function(i){return(paste("Node", i))}) # longer node names
plotDendrogram(denRep, pointLabels) # Wha?

plotDendrogram(denRep, pointLabels,
               textRotate=T, textTop=T) # Oooh

#
# Or we could align the text labels to the end using textAdjust.
#
plotDendrogram(denRep, pointLabels,
               textRotate=T, textAdjust=c(1.5, 0.95))

#
# The dendrogram can go from left to right.
#
plotDendrogram(denRep, pointLabels,
               plotHorizontal=T,
               textRotate=T, textAdjust=c(1.5, 0.95)) # <- this way is up 

#
# And we can reverse the order of the nodes, too.
#
plotDendrogram(denRep, pointLabels,
               plotHorizontal=T, reverseOrder=T,
               textRotate=T, textAdjust=c(1.5, 0.95)) # pu si yaw siht ->



## Part 2. Images ##
#
# It is also possible to place images in the plots.
# We do this by adding an array of filenames to pointLabels.
# For this example, the images are named "stimulus##.png" where ## denotes a number.
# Currently, only PNG files are used for images. 
#
imageDir = "examples/images/"
pointLabels$images = sapply(1:n, function(i){return(paste(imageDir,"stimulus",i,".png", sep=""))}) # create list of filenames

plotDendrogram(denRep, pointLabels, 
               axisSide=-1)

#
# The images are placed int two rows to maximize the size without overlapping the branches.
# Notice the scale doesn't start from 0. 
# With images, by default the minimum height is adjusted to the lowest branching.
# To disable this, set adjustMin to FALSE.
#
plotDendrogram(denRep, pointLabels, 
               adjustMin=F, 
               textScale=0, # hide text
               axisSide=-1)

# 
# Let's add text to the top of the branches for better legibility.
#
plotDendrogram(denRep, pointLabels, type="triangle",
               adjustMin=F, 
               textTop=T, textRotate=T,
               axisSide=-1) # Sweet!

# 
# Let's add text to the top of the branches for better legibility.
#
plotDendrogram(denRep, pointLabels,
               adjustMin=F, 
               textTop=T, textRotate=T,
               axisSide=-1) # Sweet!

#
# On, and one more thing...
# Set the type parameter to "triangle" and see what happens.
#
plotDendrogram(denRep, pointLabels, type="triangle",
               adjustMin=F, 
               textScale=0)