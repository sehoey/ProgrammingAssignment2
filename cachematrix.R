## The functions below is a set of functions that cache the inverse of a matrix 
## so that it does not have to be computed repeatedly.  To do this, the inverse 
## calculated once and cashed and only updated if the matrix is changed.

# makeCacheMatrix is a function that stores a set of functions.  The functions
# are set, get, setinverse and getinverse.  It sets the value of the matrix, 
# gets the value of the matrix, sets the value of the inverse of the matrix
# and gets the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The cacheSolve function calculates the inverse of the matrix from the function above.  
# It will first check to see if the inverse has already been calculated.
# If so, it will get the inverse from the cache and not calculate it.
# If not, it will calculate the inverse and set the value of the inverse in the cache 
# using the setinverse function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
