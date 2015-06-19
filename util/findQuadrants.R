#
# finds the quadrants for each entry
#
findQuadrants = function(x, y) {
  # find quarter for idx
  cx = mean(range(x))
  cy = mean(range(y))
  qx = unlist(lapply(x, lambda <- function(d) d > cx))
  qy = unlist(lapply(y, lambda <- function(d) d > cy))
  
  return(list("x" = qx, "y" = qy))
}