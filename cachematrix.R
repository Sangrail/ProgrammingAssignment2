## This function returns a list of functions that:
## 1. Set the value of the matrix
## 2. Get the value of the input matrix
## 3. Set the inverse of the input matrix
## 4. Get the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL # this is where the result of inversion is stored
  
  ##Create accessor functions (set/get)
  setMatrix <- function(y) { 
    x <<- y  
    xInv <<- NULL 
  }
  getMatrix <- function() { 
    x 
  } 
  setInv <- function(inv) { 
    xInv <<- inv 
  }
  getInv <- function() { 
    xInv 
  }

  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInv = setInv, 
       getInv = getInv)
}

## The cachesolve function calculates (or retrieves a previously calulated) inverse 
## matrix from x.
##
## If it exists (i.e. m is not null) then the cached inverse is returned.
## If it does not exist then the inoverse matrix is solved for and the result 
## is cached for later retreival and the calulated value is returned.
cacheSolve <- function(x, ...) {
  inv <- x$getInv() 
  
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv) ##return the cached inverse
  }

  data <- x$getMatrix()
  invMat <- solve(data, ...)
  x$setInv(invMat) 
  
  return(inv) # return the calculated inverse (if it exists)
}
