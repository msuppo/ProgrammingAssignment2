## Given a square invertible matrix, those 2 functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## Returns a list of functions used by cacheSolve to get or set the inverted matrix in cache
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
    x <<- y  
    cache <<- NULL  
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


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## it retrieves the inverse from the cache.
## @x: output of makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  ## attempt to get the inverse of the matrix stored in cache
  cache <- x$getInverse()

  # if the inverse has already been calculated
  if (!is.null(cache)) {
    # it is get from the cache and skips the computation 
    message("getting cached data")
    return(cache)
  }
  
  # otherwise, calculates the inverse 
  matrix <- x$get()
  
  # set and return inverse of matrix
  cache <- solve(matrix, ...)
  
  # sets the value of the inverse in the cache via the setInverse function
  x$setInverse(cache)
  
  return (cache)
}
