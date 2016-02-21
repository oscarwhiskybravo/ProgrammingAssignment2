## The functions below make it possible to cache result of matrix inversion computations, which may be costly computations
## Therefore, caching of the result of this computation may save resources and/or computing time

## the makeCacheMatrix() function creates a 'special' type of matrix which makes it possible to store cached results of the computed inversion
## Input of the function is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list( set=set, get=get, setinv=setinv, getinv=getinv )
}


## the cacheSolve() function computes the inverse of a matrix if not already done.
## The function checks if the 'special' matrix object created with makeCacheMatrix() contains a cached inverse of the matrix
## If so, it will return the cached inverse matrix
## If not, it will compute the inverse matrix, store it, and return it as well

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message('getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
