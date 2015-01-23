## The aim of these functions 'makeCacheMatrix' and 'cacheSolve' are to take a matrix, inverse it, and cache it
## This way, if a matrix is very large, we can save up on computational time and resources, rather than re-calculating
## the inverse every time.

## This special function can take a matrix inverse it, and then cache it
makeCacheMatrix <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## get returns the matrix
    get <- function() x
    ## setInverse applies the solve function to the matrix (we were asked to assume that it is invertible)
    setInverse <- function(solve) i <<- solve
    ## getInverse returns the inverted matrix
    getInverse <- function() i
    ## return list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}

## cacheSolve checks to see if inverted matrix has been cached, otherwise it calls the other function to inverse, then cache
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    ## check if inverted matrix is already cached, if so let the user know and return (exit function)
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## otherwise get the stored matrix
    data <- x$get()
    ## inverse the matrix using solve()
    i <- solve(data, ...)
    ## cache the inverse matrix
    x$setInverse(i)
    ## return the inverse matrix
    i
}
