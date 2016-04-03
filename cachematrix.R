# A pair of functions that cache the inverse of a matrix in order to enhance computation
# effectiveness

## makeCache creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function (x = matrix()) {
  inverse <- NULL
  set <- function (y){
    x <<- y
    inverse <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set=set, get=get, setinverse= setinverse, getinverse= getinverse)
}

## cacheSolve computes the inverse of special matrix object (output of makeCacheMatrix).
## If  inverse of matrix has already been calculated (and matrix is unchanged)
## cacheMatrix retrieves the inverse from the cache
cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <-solve(data,...)
  x$setinverse(inverse)
  inverse
}
