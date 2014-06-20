## Put comments here that give an overall description of what your
## functions do
## These functions implement caching for matrix inversion so that
## we dont compute those multiple times.

## Write a short comment describing this function
## This function defines a class for corresponding to
## a matrix which has functions to retrieve data as well
## as cached inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(inv) s <<- inv
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
## This function implements inversion and caching for above defined class.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if (!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s
}
