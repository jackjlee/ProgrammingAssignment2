## These functions work to create a "matrix" that is able to
## cache its inverse


## This function returns a "matrix" that is actually a list of 
## functions (set, get, setInverse, and getInverse). The input is
## an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    # this will store the inverse matrix
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i    
    
    # this list of functions is returned
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the cached inverse of our "matrix"
## if possible; if a cached matrix doesn't exist, the inverse 
## is calculated and then cached.

cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    if(!is.null(i)) {
        message("Getting cached inverse matrix")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
