## Functions to create a special Matrix which is able to cache its inverse computation

## This function creates the special Matrix and exposes functions
## to manipulate it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the sepcial Matrix. If
## a cached value for the inverse is found in the special Matrix, then
## its value is returned, saving some computation. Otherwise the inverse
## is caclulated and cached in the special Matrix for future calls.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
