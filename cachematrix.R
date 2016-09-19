# The 'makeCacheMatrix' and ''cacheSolve' functions calculates inverse of a matrix 
# provided as the argument to the function. It saves the inverse matrix
# to the cache and the next time if a user gives the same matrix values
# then the saved cache value will be returned without any calcultion

# Below function will create a matrix which will cache it's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  y < NULL
  setMatrix <- function(y) {
    # Assigning th Variable y t variable x in parent environment
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  
  # Assigning or setting the cache value m to inverse of mtrix provided by user as x 
  setInverse <- function(inverseVal) m <<- inverseVal
  
  # It will return cached inverse of input x
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The 'cacheSolve' function calculates the inverse of the matrix
# provided by the user. This functions also checks if the matrix
# is already in cache. It it's there then it will return the previously
# calculated inverse. Incase the matrix is new then the inverse is
# calculated and returned

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  # Check if the matrix is already there in cache
  if(!is.null(m)) {
    if(x$setMatrix() == x$getMatrix())
      message("getting cached data")
    return(m)
  }
  y <- x$getMatrix()
  x$setMatrix(y)
  m <- solve(y, ...)
  x$setInverse(m)
  m
}
