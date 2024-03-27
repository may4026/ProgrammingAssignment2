## This function creates a special "matrix" object that can cache its inverse.
## It takes an argument 'x' which is a matrix. If 'x' is not provided,
## an empty matrix is used.
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the cached inverse matrix as NULL
    cachedInverse <- NULL
    
    ## Define the function set to set the matrix
    set <- function(y) {
        ## Assign 'y' to 'x'
        x <<- y
        ## Reset the cached inverse to NULL since the matrix has changed
        cachedInverse <<- NULL
    }
    
    ## Define the function get to get the matrix
    get <- function() {
        ## Return the current value of 'x'
        x
    }
    
    ## Define the function setInverse to set the cached inverse
    setInverse <- function(inverse) {
        ## Assign 'inverse' to 'cachedInverse'
        cachedInverse <<- inverse
    }
    
    ## Define the function getInverse to get the cached inverse
    getInverse <- function() {
        ## Return the current value of 'cachedInverse'
        cachedInverse
    }
    
    ## Return a list containing the set, get, setInverse, and getInverse functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then
## it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Get the cached inverse from 'x' using the getInverse function
    inverse <- x$getInverse()
    
    ## If the cached inverse exists, return it
    if (!is.null(inverse)) {
        message("Getting cached inverse")
        return(inverse)
    }
    
    ## If the cached inverse does not exist, compute it using solve
    data <- x$get()
    inverse <- solve(data, ...)
    
    ## Set the computed inverse in the cache using setInverse function
    x$setInverse(inverse)
    
    ## Return the computed inverse
    inverse
}
