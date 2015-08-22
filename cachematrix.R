# makeCacheMatrix and cacheSolve are two functions
# that demonstrate how to store calculation results
# in a cache to speed up programming.

# makeCacheMatrix creates a special "matrix" that is
# really just a list of functions used to 
# set and get the value of the matrix and
# set and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list( set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}



# cacheSolve calculates the inverse of the special "matrix"
# created with the makeCacheMatrix function. It checks
# to see if the inverse has already been calculated.
# If the inverse is already calculated, then it gets
# the inverse from the cache, ortherwise it calculates
# and caches the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
