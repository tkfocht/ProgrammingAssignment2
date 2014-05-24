## The functions create a matrix with helper functions that
## calculate and cache the matrix's inverse in order to reduce
## computation time

## makeCacheMatrix wraps a matrix with helper methods that
## cache the matrix's inverse when it is calculated

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve takes a wrapped matrix created by createCacheMatrix
## and computes its inverse. The function then stores the result in
## the wrapped matrix's cache so that it doesn't have to be computed
## again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
