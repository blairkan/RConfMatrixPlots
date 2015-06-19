#
# In this example we will start with the default MDS plot, 
# then gradually add more structure to the data (images, categories)
# and make use of the advance functions of plotMDS
#

# First, load the functions we need.
source("distance/getDistances.R")
source("process//calculateMDS.R")
source("plot//plotMDS.R")

## Part 0. Setup ##
#
# Let's start by creating an example confusion matrix.
#
n = 12
confMatrix = matrix(abs(runif(n^2)), n, n) # square matrix with uniform distribution
confMatrix = sweep(confMatrix, 2, rowSums(confMatrix), `/`) # normalize rows
# image(confMatrix, xaxt="n", yaxt="n") # a very confused matrix

#
# Next, let's transform this into a distance matrix,
# then to an MDS representation.
#
dst = getDistances(confMatrix)
# image(as.matrix(dst), xaxt="n", yaxt="n") # a symmetrically distant matrix
mdsRep = calculateMDS(dst, ndim=6) # calculate up to 6th dimension

#
# And let's plot the data
#
plotMDS(mdsRep) # a very monotonous plot



## Part 1. Color/Text ##
#
# In this part, let's look at ways of stylizing the MDS plot.
# First, let's add labels to each point. 
# To do this, we need to create a list object that contains the name of the points.
#
pointNames = 1:n
pointLabels = list(names=pointNames)

plotMDS(mdsRep, pointLabels) # one, two, three...

#
# Next, to add color to the points, we add an array of colors to pointLabels.
# We will use the RColorBrewer package for a nice palette.
#
# install.packages("RColorBrewer") # run this line if you don't have RColorBrewer installed.
library(RColorBrewer)
pointLabels$colors = brewer.pal(n, "Paired") # add colors to the pointLabels

plotMDS(mdsRep, pointLabels) # pretty colors!

#
# We can view other dimensions of the MDS plot by the dims parameter
#
plotMDS(mdsRep, pointLabels,
        dims=c(3, 4)) # pretty colors!

#
# We can move the text from top of the points to the bottom using textAdjust.
# The positive direction for textAdjust is to the left and bottom.
#
plotMDS(mdsRep, pointLabels,
        textAdjust=c(.5, 2.5)) # how low can you go?

#
# We can plot the MDS with text only by scaling the points to zero,
# and centering the text with textAdjust.
# We can also hide the axis labels using showAxisTitle
#
plotMDS(mdsRep, pointLabels,
        textAdjust=c(.5, .5), pointScale = 0, showAxisTitle=F) # numbers numbers



## Part 2. Images
#
# It is also possible to place images in the plots.
# We do this by adding an array of filenames to pointLabels.
# For this example, the images are named "stimulus##.png" where ## denotes a number.
# Currently, only PNG files are used for images. 
#
imageDir = "examples/images/"
pointLabels$images = sapply(1:n, function(i){return(paste(imageDir,"stimulus",i,".png", sep=""))}) # create list of filenames

plotMDS(mdsRep, pointLabels) # small images...

#
# The images can be scaled using the imageScale parameter.
# Let's also hide the text labels by moving them to the center. 
#
plotMDS(mdsRep, pointLabels,
        imageScale=2, textScale=0) # that's better



## Part 3. Categories
#
# We can group the points into categories. 
# To do this, first each element needs a category to be assigned in pointLabels$category.
# Next, we need to specify the colors and the optionally names for the categories in a list.
# Note, the category colors will override the colors assigned in pointLabels$colors.
#
nGroups = 3
pointLabels$category = (1:n)%%nGroups + 1 # split the points into three groups
pointCategories = list(colors=brewer.pal(nGroups, "Set1"))

plotMDS(mdsRep, pointLabels, pointCategories,
        imageScale=2, textScale=0)

#
# We can look at the residual distributions using boxPlots
#
plotMDS(mdsRep, pointLabels, pointCategories,
        boxPlots=c(1, 1),
        imageScale=2, textScale=0)

#
# -1 for boxPlots will plot the boxplot on the opposite side
#
plotMDS(mdsRep, pointLabels, pointCategories,
        boxPlots=c(-1,1),
        imageScale=2, textScale=0)
