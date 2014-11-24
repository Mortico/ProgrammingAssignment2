## Put comments here that give an overall description of what your
## functions do

## Create a matrix that can have its inverse cached.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

## Return the inverse of that matrix using the "general inverse function- ginv() of the MASS package.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- ginv(data, ...) # Calculates the Moore Penrose inverse.
    x$setinv(m)
    m
}