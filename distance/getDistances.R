#
# Transforms a confusion matrix into a symmetric distance object
#
# Args:
#   probMatrix: a square matrix. not necessarily symmetric
#   sim.method: the method for transforming the probability matrix into a similarity matrix. {average, diagonal, geometric} Default is "average".
#               "average" takes the average of the confusion matrix and its transpose. 
#               "diagonal" normalizes each row by the diagonal component then takes the average of the normlized confusion matrix and its transpose. 
#               "geometric" normalizes each row by the diagonal component then takes the geometric mean of the normlized confusion matrix and its transpose. 
#   dist.method: the method for transforming the similarity matrix into a distance matrix. {linear, power, logarithmic}
#               "linear" 1 - similarity
#               "power" 1 - similarity^dist.power
#               "logarithmic" 1 - log_{dist.power + 1}(dist.power * similarity + 1)
#   dist.power: the power coefficient. (only works for dist.method = "power" || "logarithmic". )
#
# Returns: 
#   A dist object containing the distances.
#
# see STIMULUS AND RESPONSE GENERALIZATION: DEDUCTION OF THE GENERALIZATION (Shepard 1958)
#
getDistances = function(cm, sim.method="average", dist.method="linear", dist.power = 1) {
  
  # calculate similarity from confusion matrix
  if (sim.method == "average") {
    sim = ( cm + t(cm) )/2 # make symmetric
  } else if (sim.method == "diagonal") { 
    sim = cm/diag(cm)
    sim = ( sim + t(sim) )/2 # arithmetic mean
  } else if (sim.method == "geometric") {
    sim = cm/diag(cm)
    sim = sqrt(sim * t(sim)) # geometric mean
  }
  
  # transform similarity to distances
  if (dist.method == "linear") {
    dst = 1 - sim
  } else if (dist.method == "power") {
    dst = 1 - sim^dist.power
  } else if (dist.method == "log") {
    dst = 1 - log2((dist.power*sim + 1)) / log2(dist.power + 1)
  }
  
  return( as.dist( dst ) )
}