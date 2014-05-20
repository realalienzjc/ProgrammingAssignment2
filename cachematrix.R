## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(matrix) m <<- matrix
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Calculate the inverse of the matrix, skip if already computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        } 
        data <- x$get()
        if ( nrow(data) == ncol(data)){
                m <- solve(data)
                x$setInverse(m)
        } else {
                error("Not a square matrix")
        }
}
