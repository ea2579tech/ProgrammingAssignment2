
## This function creates the inverse of "matrix" object and then caches that inverse
##   It also returns a list of functions 

makeCacheMatrix <- function(x = matrix()) {
    cm <- NULL
    set <- function(y) {
      x <<- y
      cm <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) cm <<- solve  # Create and cache the inverse of passed matrix
    getInverse <- function() cm                 # Return the Inverse of passed matrix
    list(set = set, get = get,                  # makeCacheMatrix returns a list of functions
        setInverse = setinverse,                ## that can be applied to matrix `x`
        getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    cm <- x$getInverse()            ## Retrieve a matrix that is the inverse of 'x'
    if(!is.null(cm)) {
      message("getting cached Inverse of x")
      return(cm)
    }               
    ## if no cached value for Inverse of `x` exists, 
    ##   then create one, cache it and return it
    dtm <- x$get()              # obtain the matrix to be inversed and cached
    cm <- solve(dtm, ...)
    x$setInverse(cm)
    cm
}
