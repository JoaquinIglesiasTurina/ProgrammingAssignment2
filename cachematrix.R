## We are trying to cache a matrix and its inverse. We are doing so with 2 
##functions.

## This function caches the matrix. We set our matrix in the set and get functions,
##then the inverse is stored in the setInverse variable, and the inverse is returned
##by getInverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function inverts the matrix and returns the inverse of the matrix.
## We do so by calling "get" from the previous function, storing it in the data
##variable and then takin the inverse with "solve."

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
