#
# Symmetrizes a matrix
# 
# Arguments:
# m - The square matrix to symmetrize
# method - The symmetrizing method. This should be one of "average", "geometric", "harmonic", "norm". Default is "average".
#
# Details:
# TODO: explain symmetrizing methods
#
symmetrizeMatrix = function(m, method = 'average'){
  # TODO: check if matrix is square
  
  if (method == "average") {
    
    sim = ( m + t(m) )/2 # arithmetic mean
    
  } else if (method == "geometric") { 
    
    sim = sqrt( m * t(m) ) # geometric mean
    
  } else if (method == "harmonic") { 
    
    sim = 1/( 1/m + 1/t(m) )
    
  } else if (method == "norm") { 
    # b_ij = a_ij * a_ji / (a_ii * a_jj) = (a_ij/a_ii) * (a_ji/a_jj)
    ra = m / diag(m)
    sim = ra * t(ra)
  }  
  
  return(m)
}