## These functions enable the inverse of a matrix to be cached in a special
## "matrix" object in order to avoid the expense of recomputing it
## if the matrix has not changed. 
## It is assumed that the input matrix is invertible.
## These functions are based on the example code for caching the mean of a vector
## provided in the README file containing the assignment instructions.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        x.inverse <- NULL
        set <- function(y) {
                x <<- y
                x.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x.inverse <<- inverse
        getinverse <- function() x.inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by the `makeCacheMatrix` function. If the inverse has
## already been calculated (and the matrix has not changed), 
## then this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
