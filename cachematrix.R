## Author: Pranav Annapareddy
## Created: 2 April, 2020
## Modified: 2 April, 2020
## Description: Caching the inverse of a matrix


# Function: makeCacheMatrix
# This function creates the matrix and return a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}


# Function: cacheSolve
# This function computes the invese of the matrix. 
# If it is required to create the inverse, it will compute the inverse 
#   and then cache the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## End of Script ##