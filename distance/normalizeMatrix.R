#
# Normalizes a matrix
# 
# Arguments:
# m - The square matrix to normalize
# method - The normalization method. This should be one of "r.diagonal", "r.sum", "c.diagonal", "c.sum". Default is "r.diagonal".
#
# Details:
# TODO: explain normalization methods
#
normalizeMatrix = function(m, method='r.diagonal') {
  
  if (method == "r.diagonal") { # divide each row by main diagonal element
    
    nrm = m / diag(m)
    
  } else if (method == "r.sum") { # divide each row by the sum of the row
    
    nrm = m / rowSums(m)
    
  } else if (method == "c.diagonal") { # divide each column by main diagonal element
    
    nrm = t( t(m) / diag(m) )
    
  } else if (method == "c.sum") { # divide each column by the sum of the column
    
    cs = colSums(m)
    nrm = t( t(m) / cs )
  } 
  
  return(nrm)
}