## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates an object that can cache the inverse of a matrix
# @param: x an inversable matrix
# @return a list of four functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # a function to reset the matrix and it's inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # a function to get the matrix
  get <- function() x
  # a function to reset the matrix's inverse in cache
  setinverse <- function(inverse) inv <<- inverse
  # a function to get the matrix's inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function retrieves a matrix's inverse from cache
# if the inverse doesn't exist, recalculates it and set it to cache
# @param: x a list of four functions returned by makeCacheMatrix
# @return the inverse of the matrix
cacheSolve <- function(x, ...) {
  # read the inverse from cache
  inv <- x$getinverse()
  # if the inverse exists in cache, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if the inverse doesn't exist in cache
  # recalculate it and put the value to cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
