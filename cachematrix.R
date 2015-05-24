## Catching the Inverse of a Matrix


## This function creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of a special "matrix"
## (returned by makeCacheMatrix), or returns the inverse from the
## cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)  
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
