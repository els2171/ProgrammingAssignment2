## The following two functions will cache the inverse of a matrix, or solve
## and return the inverse if not already cached.

## makeCacheMatrix will create an object that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invmat) inv <<- invmat
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve will return the inverse if cached in makeCacheMatrix, or 
## calculate and return the inverse on its own.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
