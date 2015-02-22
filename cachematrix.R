## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly
## This pair of functions cache the inverse of a matrix

## The first function creates a special "matrix" object that
## can cache its inverse. It's actually a list of functions

makeCacheMatrix <- function(x = matrix()) {
  
  MatrixInv <- NULL # to store the result of matrix inversion
  
  # Set a matrix to object created by makeCacheMatrix
  set <- function(y){ 
    x <<- y
    MatrixInv <<- NULL
  }
  
  get <- function()x # get the input matrix
  setInv <- function(inv) MatrixInv <<- inv # set the inversed matrix
  getInv <- function() MatrixInv # get the inversed matrix
  
  # create a list that contains above functions
  list(set = set, get = get,
       setInv = setInv, 
       getInv = getInv)
}


## The second function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  MatrixInv <- x$getInv()
  
  # if the inversion result is there, return the 
  # pre-calculated inversion
  if(!is.null(MatrixInv)) { 
    message("getting cached data")
    return(MatrixInv) 
  }
  data <- x$get() # if not, use x$get to get the matrix object
  MatrixInv <- solve(data) #calculate the inversion
  x$setInv(MatrixInv) # then set it to the object
  MatrixInv # return the sloved result
}
