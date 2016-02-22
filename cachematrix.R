## Assignment: Caching the Inverse of a Matrix
## To cache the computed inverse of a square matrix.
## Matrix inversion is usually a costly computation and could benefit
## from caching the inverse of a matrix rather than compute it repeatedly.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(theMatrix = matrix()) {
  cacheInv <- NULL
  
  # set the value of the matrix, @param - y- the matrix which inverse has to be calculated 
  set <- function(y) {
    theMatrix <<- y
    cacheInv <<- NULL
  }
  
  # get the value of the matrix, which inverse has to be calculated
  get <- function() theMatrix
  
  # setinverse(inverse), @param inverse - sets the cacheMatrix's inverse
  setinverse <- function(inverse) cacheInv <<- inverse
  
  # getinverse()- inverse return inverse matrix - from the cacheInv.
  getinverse <- function() cacheInv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve- This function computes the inverse of the "thematrix" returned by makeCacheMatrix.
cacheSolve <- function(theMatrix, ...) {
  cacheInv <- theMatrix$getinverse()
  if(!is.null(cacheInv)) {
    message("getting cached data.")
    return(cacheInv)
  }
  data <- theMatrix$get()
  
  #assume that the matrix supplied is always invertible.
  cacheInv <- solve(data)
  theMatrix$setinverse(cacheInv)
  cacheInv
}