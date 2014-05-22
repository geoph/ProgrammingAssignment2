

## It creates the special "matrix" and provides accesses
## to retreive and set both the original matrix and its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {   # set a new matrix
    x <<- y
    inverse <<- NULL     # When the matrix is set, the (cached) inverse is set to NULL
  }
  get <- function() x    # get matrix
  setInverse <- function(inv) inverse <<- inv ##Inverse function
  getInverse <- function() inverse
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## It calculates the inverse of the matrix provided by makeCacheMatrix


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$setInverse()                # get the cached inverse
  if(!is.null(inv)) {                  # checks if the CacheMatrix has a cached inverse
    message("getting cached inverse")
    return(inv)                        # return the cached inverse and exit the function
  }
  mat <- x$get()                       # retrive the original matrix 
  mat.inv <- solve(mat, ...)           # invert the original matrix
  x$setInverse(mat.inv)                # put the inverse matrix calculated in the cache 
  mat.inv                              # return the inverse of the input matrix
}
