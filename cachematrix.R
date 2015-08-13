## These two functions are used to create a special object that stores a 
## matrix and caches its inverse matrix.

##  makeCacheMatrix creates a special "matrix" as a list containing functions
# 1) set the values in the matrix, 2) get the values of the matrix 
# 3) set the inverse matrix, 4) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix"
# This function checks to see if the inverse matrix of a certain matrix had been calculated.
# If so, the function skips computations and returns the inverse matrix.
# Otherwise it caculates the inverse matrix and sets the inverse matrix in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
