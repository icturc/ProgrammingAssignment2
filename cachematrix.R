# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix, especially if it has to be computed
# repeatedly (e.g. in a loop). If the contents of a matrix are not changing, it
# may make sense to cache the value of the inverse so that when we need it again,
# it can be looked up in the cache rather than recomputed
# Example of usage:
# specialMatrix<-makeCacheMatrix(matrix(rnorm(9), 3, 3))
# cacheSolve(specialMatrix)

# The makeCacheMatrix function creates a special "matrixr", which is really a
# list containing a function to:
#   * set the value of the matrix (set)
#   * get the value of the matrix (get)
#   * set the value of the inverse matrix (setInverse)
#   * get the value of the inverse matrix (getInverse)
makeCacheMatrix <- function(originalMatrix = matrix())
{
  # stores the inverse
  inverseMatrix <- NULL
  
  # setter for the original matrix
  set <- function(y)
  {
    originalMatrix <<- y
    inverseMatrix <<- NULL
  }
  
  # getter for the original matrix
  get <- function() originalMatrix
  
  # setter for the inverse of the stored matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  # getter for the inverse of the stored matrix; returns NULL if it was not set yet
  getInverse <- function() inverseMatrix
  
  # construct and return the special object/matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The following function calculates the inverse of the special "matrix" created
# with the above makeCacheMatrix function. However, it first checks to see if
# the inverse has already been calculated. If so, it gets it from the cache and
# skips the computation. Otherwise, it calculates the inverse of the matrix and
# sets the value in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  # see if there's a inverse stored
  inv <- x$getInverse()
  # if not NULL, we have it in cache
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  # not in cache? get the original matrix...
  data <- x$get()
  # ...compute the inverse...
  inv <- solve(data, ...)
  # ...put it in cache...
  x$setInverse(inv)
  # ...and return it...
  inv
}
