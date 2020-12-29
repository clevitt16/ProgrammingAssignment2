## Put comments here that give an overall description of what your
## functions do


# this function stores a matrix and its inverse, 
# and it returns functions for getting and setting
# the matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    getMatrix <- function() {
        x
    }
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getInverse <- function() {
        inv
    }
    setInverse <- function(z) {
        inv <<- z
    }
    list(getMatrix = getMatrix, setMatrix = setMatrix, 
         getInverse = getInverse, setInverse = setInverse)
}



# given the list of get and set functions for the matrix cached
# in makeCacheMatrix, this function returns the inverse of the matrix,
# calculating and storing it in the cache if it's not already there
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (is.null(inv)) {
        mtrx = x$getMatrix()
        inv <- solve(mtrx, ...)
        x$setInverse(inv)
    }
    inv
}
