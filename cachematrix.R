## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment


## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## @x: a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # stores the cached value, initialize to NULL
  cache <- NULL     
  
  # set the matrix in the working environment
  set <- function(y) { 
    x <<- y  # caches the inputted matrix so that cacheSolve can check whether it has changed
    cache <<- NULL  # sets the value of cache (the matrix inverse if used cacheSolve) to NULL
  }
    
  # get the value of the matrix
  get <- function() x
  
  # invert the matrix and store in cache
  setInverse <- function(inverse) cache <<- inverse
  
  # get the inverted matrix from cache
  getInverse <- function() cache
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
  
  ## attempt to get the inverse of the matrix stored in cache
  cache <- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(cache)) {
    message("getting cached data")
    
    # display matrix in console
    return(cache)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
  
  # set and return inverse of matrix
  cache <- solve(matrix, ...)
  
  # set inverted matrix in cache
  x$setInverse(cache)
  
  # display matrix in console
  return (cache)
}