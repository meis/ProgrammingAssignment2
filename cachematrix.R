# This code allows to work with a special "matrix" object wich stores
# the matrix itself and its inverse.
# This allows faster processing when the inverse is needen subsequent times.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Store the inverse variable in a cacheable environtment
    inverse <- NULL

    get <- function() x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Functions to get and set the inverse matrix in the cached environment.
    getinverse <- function() inverse
    setinverse <- function(i) inverse <<- i
    
    # Return a list with all the functions
    list(
      get        = get,
      set        = set,
      getinverse = getinverse,
      setinverse = setinverse
    )

}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Get the cached inverse matrix for the "matrix" object.
    inverse <- x$getinverse()
    
    # Return the cached inverse matrix if exists.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    # If the inversed matrix is not cached, calculate and cache it.
    original_matrix <- x$get()
    inverse <- solve(original_matrix, ...)
    x$setinverse(inverse)

    # Return the calculated inverse matrix.
    inverse
}
