## General Desciption of Code ##

#This pair of functions calculates and caches the inverse of a matrix (if the inverse exists).

# The first function named makeCacheMatrix creates a special "matrix" object that can cach
# the inverse of a given matrix.  

#The second function named cacheSolve calculates the inverse matrix
# of the "matrix" object created with the makeCacheMatrix function (or pulls it from the cache if it
# has previously been calculated.

######################################################################################################
## Detail description and code ##

## This function creates a special "matrix" object that can cache its inverse.
# It is really a list containing a function to:
    #1. define the matrix
    #2. get the matrix as defined
    #3. define the inverse of the original matrix
    #4. get the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inverse <- function(solve) inv <<- solve
  get.inverse <- function() inv
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get.inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set.inverse(inv)
  inv
}

## Test random example of a matrix that does have an inverse.  
m = matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3)
cacheSolve(makeCacheMatrix(m))


