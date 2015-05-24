## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

## This function creates a special "matrix" object
## that can cache its inverse.
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  setInverse the value of the matrix inverse
## 4.  getInverse the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  # cache
  inv <- NULL
  
  # matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  
  # inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  
  # available functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## 
## Computing the inverse of a square matrix can be done with the `solve`
## function in R. For example, if `X` is a square invertible matrix, then
## `solve(X)` returns its inverse.
## 
## For this assignment, assume that the matrix supplied is always
## invertible.
cacheSolve <- function(x, ...) {
  # get cached inverse
  inverse <- x$getInverse()
  if(is.null(inverse)) {
    # not yet solved
    data <- x$get()
    
    # these extras are meaningful?
    inverse <- solve(data, ...)
    x$setInverse(inverse)
  }
  
  # single workflow solution to return inverse
  inverse
}
