## This function creates a special "matrix" object that can cache its inverse
## This special matrix has an internal cache "i" to do this.
## There are three "methods" in this matrix:
## set(value) to set the value
## get() to retrive the value
## setinverse(value) to fill the cache
## getinverse() to get data from cache
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse<- function(i) i <<-i
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.
## Otherwise the inverse function (solve) is used to invert the matrix.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("found cached inverse matrix")
    return(i)
  } else {
    i <- solve(x$get())
    x$setinverse(i)
    return(i)
  }
}
