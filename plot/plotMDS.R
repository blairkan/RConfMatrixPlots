#
# Plots the MDS coordinates with the options specified in labels
#
# Arguments:
#   mds: the output of cmdscale. (a matrix with the MDS coordinates)
#   labels: a list of label variables. possible variables are 
#     labels$names: the text labels of the elements.
#     labels$colors: the color for the text labels.
#     labels$images: the images for the elements.
#     labels$category: the category for the elements.
#   category: a list of category related labels
#     category$names: the text labels of the categories.
#     category$colors: the color for the categories.
#   dims: an array with the dimensions to use for the x, y axes. Default is c(1,2)
#   xRange: the range for the x axis. the x axis range will be from -xRange to xRange
#   flipAxes: flips the axes. Default is c(FALSE, FALSE)
#   showAxisTitle: show/hide the axis title
#   boxPlots: Shows residual boxplots for each dimension. c(1,1) show boxplots on the top and right of the MDS plot. c(-1,-1) shows boxplots Default is c(0, 0)
#   pointScale: scales the point on the plot. Default is 1.
#   imageScale: scales the images (only works when labels$images is not NULL). Default is 1. 
#   axisScale: scales the text of the axes. Default is 1.5.
#   textScale: scales the text labels. Default is 1.
#   textAdjust: adjust the position of the text. this changes the text anchor. c(0,0) is the bottom left corner. Default is c(.5, -1.75)
# 
# Returns:
#   Nothing.
# 
plotMDS = function(mds, labels=NULL, category=NULL, 
                   dims=c(1,2), xRange=0.6, 
                   flipAxes=c(F,F), showAxisTitle=T,
                   boxPlots=c(0, 0), 
                   pointScale=1, imageScale=1, axisScale=1.5,
                   textScale=1, textAdjust=c(.5, -1.75),
                   pch=15, outcex=1.5, outlwd=2, mar=c(2,2,2,2), mgp=c(0.5,0,0)) {
  
  n = dim(mds)[1] # get number of elements
  
  # get x/y coordinates
  x = mds[,dims[1]]
  y = mds[,dims[2]]
  
  # flip axes if needed
  if (flipAxes[1]) 
    x = -x;
  if (flipAxes[2]) 
    y = -y;
  
  # set x limits
  xlm = c(-xRange, xRange)
  
  # check the labels and configure plot
  # TODO: if labels are missing, create default labels
  
  # set colors
  colors = "black"
  labelOptions = names(labels)
  if ("category" %in% labelOptions) { # if there is color for the categories, use those colors
    
    catOptions = names(category)
    if ("colors" %in% catOptions) {
      colors = category$colors[labels$category]
    } else if ("colors" %in% labelOptions) {
      colors = labels$colors
    }
  } else if ("colors" %in% labelOptions) {
    colors = labels$colors
  }
  
  
  # configure mds plot position
  if (boxPlots[1] != 0 || boxPlots[2] != 0) { # configure for boxplots
    
    fig = c(0, 0.8, 0, 0.8)
    marSide = 4
    pmar = c(marSide,marSide,0,0)
    
    if (boxPlots[1] < 0) { # x-axis boxplot at bottom
      fig[3:4] = c(0.2, 1)
      pmar[c(1,3)] = pmar[c(3,1)]
    }
    if (boxPlots[2] < 0) { # y-axis boxplot on left
      fig[1:2] = c(0.2, 1)
      pmar[c(2,4)] = pmar[c(4,2)]
    }
    
    # mds layout
    par(fig=fig, new=F, mar=pmar, mgp=mgp) 
  } else { # only MDS plot (no boxplots)
    par(mar=mar, mgp=mgp)
  }
  
  # create axis labels
  xlabel = ""
  ylabel = ""
  if (showAxisTitle) {
    xlabel = paste('Dimension', dims[1])
    ylabel = paste('Dimension', dims[2])
  }
  
  
  # plot MDS points
  plot(x, y, pch = pch, cex = 5*pointScale,
       col = colors,
       xlim = xlm, ylim = xlm,
       xaxt = 'n', yaxt = 'n',
       xlab = xlabel, ylab = ylabel,
       cex.lab = axisScale
       )
  
  if (boxPlots[1] < 0) # x-axis boxplot at bottom
    mtext(paste('Dimension', dims[1]), 3, cex=axisScale, padj=-axisScale)
  if (boxPlots[2] < 0) # y-axis boxplot on left
    mtext(paste('Dimension', dims[2]), 4, cex=axisScale, padj=axisScale)
  
  # add name text if it exists
  if (("names" %in% labelOptions) && (textScale > 0))
    text(x, y, labels$names, 
         adj=textAdjust, cex=1.25*textScale, col=colors)
  
  # add images if an image list exists
  if ("images" %in% labelOptions) {
    library(png)
    
    # draw images
    for (i in 1:n) {
      img = readPNG(labels$images[i])
      sz = xRange/10*imageScale # control size of stimuli images
      
      rasterImage(img, x[i]-sz/2, y[i]-sz/2, x[i]+sz/2, y[i]+sz/2)
    }
  }
  
  # boxplots
  # create data frame for boxplot
  if (boxPlots[1] != 0 || boxPlots[2] != 0) {
    if ("category" %in% labelOptions) {
      df = data.frame(cat=labels$category)
    } else {
        df = data.frame(cat=rep(1,n))
    }
    df[paste('dim', as.character(dims[1]), sep="")] = x
    df[paste('dim', as.character(dims[2]), sep="")] = y
       
    # TODO: check category$colors
    
    # draw x axis box plot 
    if (boxPlots[1] != 0) { 
      # configure boxplot position
      fig = c(0, 0.8, 0.8, 1)
      mar = c(0, marSide, 1, 0)
      
      if (boxPlots[1] < 0) { # at bottom
        fig[3:4] = c(0, 0.2)
        mar[c(1,3)] = mar[c(3,1)]
      }
      if (boxPlots[2] < 0) { # on left
        fig[1:2] = c(0.2, 1)
        mar[c(2,4)] = mar[c(4,2)]
      }
      
      par(fig=fig, new=T, mar=mar)
      fml = paste('dim', dims[1], ' ~ cat', sep="")
      boxplot(as.formula(fml), df, 
              ylim=xlm, 
              pch='+', cex = 5,
              col=category$colors, outcol=category$colors, boxcol=category$colors,
              outcex = outcex, whisklwd = outlwd,
              yaxt='n', xaxt='n', 
              frame.plot=T, horizontal=T)
    }
    
    # draw y axis box plot
    if (boxPlots[2] != 0) {
      # configure boxplot position
      fig = c(0.8, 1, 0, 0.8)
      mar = c(marSide, 0, 0, 1)
      
      if (boxPlots[2] < 0) { # on left
        fig[1:2] = c(0, 0.2)
        mar[c(2,4)] = mar[c(4,2)]
      }
      if (boxPlots[1] < 0) { # at bottom
        fig[3:4] = c(0.2, 1)
        mar[c(1,3)] = mar[c(3,1)]
      }
      
      par(fig=fig, new=T, mar=mar)
      fml = paste('dim', dims[2], ' ~ cat', sep="")
      boxplot(as.formula(fml), df, 
              ylim=xlm, 
              pch='+', col=category$colors, outcol=category$colors, boxcol=category$colors,
              outcex = outcex, whisklwd = outlwd,
              yaxt='n', xaxt='n', 
              frame.plot=T)
    }
    
    # restore figure parameter to normal
    par(fig=c(0, 1, 0, 1), mar=c(5, 4, 4, 2) + 0.1, mgp=c(3, 1, 0), new=F)
  }
  
}
  