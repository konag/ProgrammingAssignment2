## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
      my_inv <- NULL
      ## Here we set the matrix.
      set <- function(y) {
            x <<- y
            my_inv <<- NULL
      }
      ## Here we get the matrix.
      get <- function() x
      ## Here we set the inverse of the matrix.
      setinv <- function(inversion) my_inv <<- inversion
      ## Here we get the inverse of the matrix.
      getinv <- function() my_inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Here we check if the inversion has been made
      ## and if so, we get the inversion from the cache.
      my_inv <- x$getinv()
      if(!is.null(my_inv)) {
            message("getting cached data")
            return(my_inv)
      }
      ## Here we get the matrix.
      data <- x$get()
      ## Here we calculate the inverse of the matrix.
      my_inv <- solve(data)
      ## Here we set the inverse to the object.
      x$setinv(my_inv)
      # Here we return the inversed matrix.
      my_inv
}