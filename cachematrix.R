## These functions allow the user to compute the inverse of an invertible matrix and cache the result.

## The function makeCacheMatrix creates an special matrix object from a given matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()){
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The function cacheSolve computes the inverse of a square matrix, or obtains the cached inverse if it exists.

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
        ## Return a matrix that is the inverse of 'x'
}
