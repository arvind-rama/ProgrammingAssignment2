# Creates a object that provides functions to store and retrieve a matrix and its inverse
# get() and set()  functions to retrieve and store the matrix that has to be inversed
# getinverse() and setinverse() to retrieve and store the inversed matrix
#
# Args:
#   x: A Matrix that is inversable. Default is empty matrix.
#
# Returns:
#   A makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solvevalue) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Calculates the inverse matrix of the provided makeCacheMatrix's matrix
# Stores the inverse into the makeCacheMatrix instance by calling setinverse()
# Returns the cached inverse matrix of the makeCacheMatrix if available.
# 
# Args:
#   x: Instance of makeCacheMatrix , whose matrix must be inversed
#
# Returns:
#   The inverse matrix of the provided makeCacheMatrix's matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}