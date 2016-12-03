## this function will compute the inverse of a matrix:
## first, creat a invertible matrix. 
## second, prepare the matrix using makeCacheMatrix function.
## last, get the inverse of the matrix using cacheSolve function.

## this function is to create a special "matrix", which is really a list
## containing a function to get/set the original/inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
            inverse_m <- NULL
            set <- function(y) {
                    x <<- y
                    inverse_m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inverse_m <<- inverse
            getinverse <- function() inverse_m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## the following function calculating the inverse of a inversible martrix,

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inverse_m <- x$getinverse()
            if(!is.null(inverse_m)) {
                    message("getting cached data")
                    return(inverse_m)
            }
            data <- x$get()
            inverse_m <- solve(data)
            x$setinverse(inverse_m)
            inverse_m
}
