## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function sets the values of makeCacheMatrix
## specifically the matrix and the cached inverse
makeCacheMatrix <- function(mat = matrix()) {
  cachedInverse <- NULL
  
  setMatrix <- function(newMatrix) {
    mat <<- newMatrix
    cachedInverse <<- NULL
  }
  
  getMatrix <- function() {
    mat
  }
  
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  getInverse <- function() {
    cachedInverse
  }
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## solves the inverse of the matrix and caches
cacheSolve <- function(cachedMatrix, ...) {
  cachedInverse <- cachedMatrix$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached inverse")
    cachedInverse
  } else {
    mat <- cachedMatrix$getMatrix()
    inverse <- solve(mat)
    cachedMatrix$setInverse(inverse)
    inverse
  }
}
