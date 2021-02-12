## This R file implements a function that computes the inverse of the input
## matrix. The result is cached inside the special object so that subsquent
## calls returns the cached inverse without the need to repeat the operation

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inversematrix) im <<- inversematrix
    getinversematrix <- function() im
    list(
        set = set,
        get = get,
        setinversematrix = setinversematrix,
        getinversematrix = getinversematrix
    )
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated then inverse
## from the cache is returned.

cacheSolve <- function(x, ...) {
    im <- x$getinversematrix()
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinversematrix(im)
    im
}
