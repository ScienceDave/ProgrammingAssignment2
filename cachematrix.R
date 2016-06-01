## This is the R programming Assigment for Week 3 - Cached Matrix
## Written by David Maxwell
##
## im - inverse of matrix
##
## Assumptions: The input matrix must be square

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) im <<- inverse
    getinv <- function() im
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function computes the inverse of the special matrix object that was
## returned by calling the makeCacheMatrix function. If the inverse was
## already calculated, then that is utilized, otherwise the inverse is
## calculated via the solve function

cacheSolve <- function(x, ...) {
    im <- x$getinv()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinv(im)
    im
}
