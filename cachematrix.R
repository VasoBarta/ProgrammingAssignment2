## Functions enable to cache a calculated value - in this case an inverse 
## of a matrix. I.e. when the inverse is calculated for the first time, it is 
## cached and when it is requested later, it will not be calculated again but 
## taken from the cache.

## makeCacheMatrix serves for initializing (constructing) the matrix instance
## and for storing the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    setdata <- function(y) {
        x <<- y
        s <<- NULL
    }
    getdata <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(setdata = setdata, getdata = getdata,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve does the actual calculation of the inverse of matrix on the instance
## created in previous step and stores the value in cache, using the makeCacheMatrix
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$getdata()
    s <- solve(data , ...)
    x$setSolve(s)
    s
}
