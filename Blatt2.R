library(matlib)
i2 <- function() {
  vec <- c()
  for (i in 1:25) {
    vec <- append(vec, i)
    
  }
  return(vec)
}


ii2 <- function(vec) {
  mat <- matrix(vec,nrow=5,ncol=5,byrow=TRUE)
  return(mat)
}

iii2 <- function(mat) {
  for (i in 1:5) {
    mat[i,i] <- 25
  }
  return(mat)
}

iv2 <- function(mat) {
  for(row in 1:nrow(mat)) {
    for(column in 1:ncol(mat)) {
      if (mat[row,column]%%2 == 0) {
        mat[row,column] <- 0
      }
    }
  }
  return(mat)
}

# naiv, no optimization
v2 <- function(mat) {
  oldmat <- mat
  for(row in 1:nrow(mat)) {
    for(column in 1:ncol(mat)) {
      matval <- 0
      for(len in 1:nrow(mat)) {
        matval <- matval + oldmat[row,len]*oldmat[len,column]
      }
      mat[row, column] <- matval
    }
  }
  return(mat)
}

vi2 <- function(mat) {
  if (det(mat) <= 10^(-15)) {
    return("Matrix is not invertible")
  } else {
    matInverted <- inv(mat)
    return(matInverted)
  }
  
}


