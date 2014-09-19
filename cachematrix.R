## Calculate the inverse of a matrix and store the result in a cache
## so that no re-calculation is required when the inverse of the
## same matrix is needed.

## makeCacheMatrix is a function that accepts a matrix object and
## creates a 'special matrix' object which provides a list of 4 functions:
## 'set' function: set a new matrix to the 'special matrix' object
## and reset the previously cached result to NULL.
## 'get' function: return the matrix set
## 'setInverse' function: set the result of the inverse of the matrix,
## this function is used by the cacheSolve function to store the result
## into the cache.
## 'getInverse' function: get the result of the inverse of the matrix
## if exists.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## The cacheSolve function acts like the Solve function but accept the
## 'special matrix' as input, instead of a normal matix object
## The function will check if there is already a previously calculated result
## exists in the cache of the 'special matrix' object.
## If that exists, just return the previously calculated result,
## if not, call the solve function to calculate the inverse of the matrix
## then store the result into the cache by calling the setInverse function 
## of the 'special matrix' object and return the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
    
}
