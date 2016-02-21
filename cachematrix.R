## First create a matrix to be inverted (for example mat<-matrix(c(5,6,7,8))
## Run by feeding variable of stored result from the first into the second:
## eg out<-makeCacheMatrix(d) 
## cacheSolve(out)
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     IM <- NULL
     set <- function(y) {
     x <<- y
     IM <<- NULL 
     }
     get <- function() x 
     setIM <- function(invert) IM <<- invert
     getIM <- function() IM
          list(set = set, get = get,
          setIM = setIM,
          getIM = getIM)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## it retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
  IM <- x$getIM() 
    if(!is.null(IM)) { 
    message("getting cached data")
    return(IM) 
  }
  data <- x$get() 
  IM <- solve(data) 
  x$setIM(IM) 
  IM 
}
