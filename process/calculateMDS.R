#
# Calculates the MDS coordinates from the distances.
#
# Args:
#   distances - a dist object
#   ndim - the number of dimensions to use for MDS. Default is 2.
#
# Returns:
#   a matrix with rows containing the coordinates of the points chosen to represent the distances
#
calculateMDS = function(distances, ndim = 2)
{
  # TODO: do a range check on ndim
#   n = dim(as.matrix(distances))[1] # get number of elements
  
  mds = cmdscale(distances, ndim)
  
  return(mds)
}