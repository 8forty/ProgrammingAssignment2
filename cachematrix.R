## functions that manage a matrix and cache its inverse after first calculation 
## to save the cost of re-calculating the inverse repeatedly

## makes a special "matrix" that caches its solve() inverse on first calculation

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse = NULL
  
  # function to set a new value for the matrix and clear the cache
  setMatrix <- function(m) {
    x <<- m
    cachedInverse <<- NULL
  }
  
  # function to get the current value of the matrix
  getMatrix <- function() x
  
  # function to set a new value for the cached inverse
  setInverse <- function(inverse) cachedInverse <<- inverse

  # function to get the current value of the cached inverse
  getInverse <- function() cachedInverse
  
  # return this special "matrix" as a list
  return (list(setMatrix = setMatrix, getMatrix = getMatrix, 
              setInverse = setInverse, getInverse = getInverse))
}


## function that operates on a special "matrix" returned by makeCacheMatrix
## to ensuring proper caching of its solve() inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # check if inverse is cached
  inverse = x$getInverse()
  if (!is.null(inverse)) {
    message("returning cached inverse")
    return(inverse)
  }
  
  # inverse wasn't cached, so compute it, cache it, and return it
  matrix <- x$getMatrix()
  inverse = solve(matrix)
  x$setInverse(inverse)
  
  return (inverse)
}
