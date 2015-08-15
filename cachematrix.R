## This file contains 2 functions:

## makeCacheMatrix: This function creates a special "matrix" object
##that can cache its inverse.

## cacheSolve: This function computes the inverse of the special
#matrix returned by makeCacheMatrix above. If the inverse has
#already been calculated (and the matrix has not changed), then
#cacheSolve should retrieve the inverse from the cache.


## This function creates a special matrix that can cache it's 
## inverse
makeCacheMatrix <- function(cachedMatrix = matrix()) {
  #initialize the cached inverse matrix to null
  cachedInverseMatrix <- NULL
  #set the matrix and reset the inverse
  set <- function(y) {
    cachedMatrix <<- y
    cachedInverseMatrix <<- NULL
  }
  #get the matrix
  get <- function() cachedMatrix
  #set the inverse of the matrix
  setinverse <- function(inverseMatrix) cachedInverseMatrix <<- inverseMatrix
  #get the inverse of the matrix
  getinverse <- function() cachedInverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Return the inverse of the matrix, if it is already cached
## return the cached value, otherwise compute the inverse,
## cache the inverse and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check to see if the inverse needs to be computed
  inverse <- x$getinverse()
  if(is.null(inverse)) {
    ## compute the inverse and cache it
    message("Computing inverse")
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
  }

  # return the inverse
  inverse
}
