#
# Plots a dendrogram with the options specified in labels
#
# Args:
#   cluster: the output of hclust.
#   labels: a list of label variables. possible variables are 
#     labels$names: the text labels of the elements.
#     labels$colors: the color for the text labels.
#     labels$images: the images for the elements.
#   category: a list of category related labels
#     category$names: the text labels of the categories.
#     category$colors: the color for the categories.
#   type: the dendrogram type. {rectangle, triangle} default is "rectangle"
#   plotHorizontal: if TRUE, dendrogram goes from left to right. Default is FALSE.
#   reverseOrder: flips the order of the dendrogram leaf nodes.
#   adjustMin:  adjusts the baseline to the minimum cluster height (only works when labels$images is not NULL).
#   minOffset: offsets the minimum height of the dendrogram. (only works when adjustMin is FALSE)
#   maxHeight: sets the maximum height of the dendrogram. automatically scales the dendrogram if set to NULL. Default is NULL.
#   textScale: scales the text size.
#   textAdjust: adjust the position of the text. this changes the text anchor. c(0,0) is the bottom left corner. Default is c(.5, .5).
#   textOffset: moves the text position up/down the dendrogram.
#   textTop: if TRUE, positions the text to the branching height of the leaf node. if FALSE, places text at the leaf node. Default is FALSE.
#   textRotate: rotates the text.
#   axisSide: select the side to show the axis. values are {-1,0,1}. 0 hides the axis
#   axisScale: scales the axis text
#
# Returns:
#   Nothing.
# 
plotDendrogram = function(cluster, labels=NULL, category=NULL, 
                          type="rectangle", plotHorizontal=F, reverseOrder=F, 
                          adjustMin=T, minOffset=0, maxHeight=NULL, 
                          textScale=1, textAdjust=c(0.5, 1.0), textOffset=0, textTop=F, textRotate=F, 
                          axisSide=0, axisScale=1.5, 
                          asp=1, lwd=1, mar=c(2, 1.5, 2, 1.5), mgp=c(1,0.5,0)) {
  
  n = length(cluster$order) # this is a hacky way of finding the number of elements
  
  # check the labels and configure plot
  labelOptions = names(labels)
  
  # set colors
  colors = "black"
  if ("category" %in% labelOptions) { # if there is color for the categories, use those colors
    catOptions = names(category)
    if ("colors" %in% catOptions)
      colors = category$colors[labels$category]
    else if ("colors" %in% labelOptions)
      colors = labels$colors
  }
  else if ("colors" %in% labelOptions)
    colors = labels$colors
  
  # check reverse flag
  den = as.dendrogram(cluster)
  order = cluster$order
  if (reverseOrder) {
    den = rev(den)
    order = rev(order)
  } 
  
  # get height limits
  maxY = max(cluster$height)
  if (!is.null(maxHeight))
    maxY = maxHeight
  minY = minOffset;
  minH = min(cluster$height);
  
  # adjust heights for images
  if ("images" %in% labelOptions) { 
    
    if (adjustMin) # automatically set the min height 
      minY = minH # TODO: fix code so that adjustMin works without images
    
    imageH = (maxY - minY)/(n*0.5)*asp # image height
    minY = minY - 2.3*imageH
  }
  
  
  ## PLOT
  par(mar=mar, mgp=mgp, lwd=lwd)
  if (plotHorizontal) { # dendrogram goes left to right
    plot.xlim = c(maxY, minY)
    plot.ylim = NULL
  }
  else { # dendrogram goes top to bottom
    plot.xlim = NULL
    plot.ylim = c(minY, maxY)
  }

  # plot dendrogram
  plot(den, type=type, 
       xlab="", xaxt = "n", ylab="", yaxt = "n", 
       leaflab="none", lwd = lwd, 
       xlim = plot.xlim, 
       ylim = plot.ylim,
       horiz = plotHorizontal)

  # show axis if needed
  if (axisSide > 0) 
    axis(ifelse(plotHorizontal, 1, 2), 
         cex.axis = axisScale, lwd = lwd)
  else if (axisSide < 0) 
    axis(ifelse(plotHorizontal, 3, 4), 
         cex.axis = axisScale, lwd = lwd)
  
  
  ## TEXT
  # add name text if it exists
  if (("names" %in% labelOptions) && textScale > 0) {
    
    # get heights for text labels
    h = rep_len(textOffset, n) # put text at branch end
    if (textTop) # put text on branch height
      h = getLabelHeights(cluster) + textOffset

    # get text settings
    if (plotHorizontal) { # dendrogram goes left to right
      text.x = h[order]
      text.y = 1:n + 0.01
    }
    else { # dendrogram goes top to bottom
      text.x = 1:n + 0.01
      text.y = h[order]
    }

    if (textRotate) { # text goes along branch
      text.adj = c(-.5, -1.05) + textAdjust # default text anchor is left bottom (not center)
      text.srt = ifelse(plotHorizontal, 0, -90)
    }
    else {
      text.adj = textAdjust
      text.srt = ifelse(plotHorizontal, 90, 0)
    }
    
    # display text
    text(x = text.x, y = text.y,
         labels=labels$names[order], 
         col=colors[order], 
         cex=textScale, 
         adj=text.adj, 
         srt=text.srt)
  }
  
  ## IMAGES
  # add images if an image list exists
  if ("images" %in% labelOptions) {
    require(png)
    
    imageW = .67 # image half width
    marginH = imageH*0.1
    
    py = 0
    if (adjustMin) # automatically set the min height 
      py = minH - marginH
    
    if(plotHorizontal) { # dendrogram goes left to right
      
      if (adjustMin) { # hide dendrogram line for every other image
        for (px in 1:n) {
          top = minH - marginH - (px %% 2)*1.1*imageH
          bottom = minY-0.2
          
          rect(xleft = top, ybottom = px-imageW, 
               xright = bottom, ytop = px+imageW, 
               col="white", lty="blank")
        }
      }
      else { # extend line to image
        for ( px in 1:n ) {
          lines(x=c(0, -imageH*(1+2*(px %% 2))/2), 
                y=c(px, px), 
                lwd = lwd)
        }
      }
      
      # draw images
      for (px in 1:n) {
        i = order[px]
        
        img = readPNG(labels$images[i])
        yOffset = (px %% 2) * 1.1 * imageH
        rasterImage(img, 
                    py-yOffset, px-imageW, 
                    py-yOffset-imageH, px+imageW)
      }
    } 
    else { # dendrogram goes top to bottom
      
      if (adjustMin) { # hide dendrogram line for every other image
        for (px in 1:n) {
          top = minH - marginH - (px %% 2)*1.1*imageH
          bottom = minY-0.2
          
          rect(xleft = px-imageW, ybottom = top, 
               xright = px+imageW, ytop = bottom, 
               col="white", lty="blank")
        }
      }
      else { # extend line to image
        for (px in 1:n) {
          lines(x=c(px, px), 
                y=c(0, -imageH*(1+2*(px %% 2))/2), 
                lwd = lwd)
        }
      }
      
      # draw images
      for (px in 1:n) {
        i = order[px]
        
        img = readPNG(labels$images[i])
        yOffset = (px %% 2) * 1.1 * imageH
        rasterImage(img, 
                    px-imageW, py-yOffset, 
                    px+imageW, py-yOffset-imageH)
      }
    }
  }
}

# convenience function for getting heights of each branch from hclust results
getLabelHeights = function(cluster) {
  
  n = length(cluster$order) # this is a hacky way of finding the number of elements
  
  h = rep_len(0, n)
  for (i in 1:(n-1)) {
    for (c in 1:2) {
      r = cluster$merge[i,c]
      if (r < 0) {
        idx = -r
        h[idx] = cluster$height[i]
      }
    }
  }
  
  return(h)
}