## This code creates two functions that cache the inverse of a matrix
##
## This function creates a matrix that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
        i <- NULL
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        get <- function() {
                m
        }
        setInverse <- function(inverse) {
                i <<- inverse
        }
        getInverse <- function() {
                i
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## This creates a function that returns the inverse of the matrix
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## Now use Matrix Multiplication to calculate the inverse
        m <- solve(data) %*% data
        x$setInverse(m)
        ## Return the matrix m
        m
}
