# 
# 
# 
calculateRepresentation = function(m, method='mds', ...) {
  if (method == "mds") {
    
    source("process//calculateMDS.R")
    data = calculateMDS(m, ...)
  }
  else if (method == "hclust") {
    
    source("process//calculateHclust.R")
    data = calculateHclust(m, ...)
  }
  
  return( list(method = method, data = data) );
}