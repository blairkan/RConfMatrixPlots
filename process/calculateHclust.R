#
# Calculates the heirarchical clustering with the distances.
# 
# Args:
#   distances: a dist object
#   method: a string naming the cluster method. ("ward", "single", "complete", "average", "mcquitty", "median", "centroid") 
#          see hclust for details. default is "ward", Ward's method.
#   reorder: a boolean. if true, the clustering tries to keep the original order of the data.
#
# Returns: 
#   an hclust object
#
calculateHclust = function(distances, method = "ward.D", reorder=T) {
  cluster = hclust(distances, method = method)
  
  if (reorder) {
    n = dim(as.matrix(distances))[1]
    cluster = as.hclust(reorder(as.dendrogram(cluster), 1:n, mean)) # reorder
  }
  
  return(cluster)
}