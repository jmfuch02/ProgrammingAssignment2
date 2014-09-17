## These functions allow the inverse of a matrix to be cached (stored) for later use


## makeCacheMatrix accepts a matrix as input
## and returns a list of functions (get, set, getinverse, setinverse)
##
## get: get the value of the matrix
## set: change the matrix to something else
## getinverse: get the inverse of the matrix (only after it has been cached)
## setinverse: set the inverse of the matrix (NEVER call this on its own!)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve accepts a 'makeCacheMatrix' object (list) as input
## and returns a matrix that is the inverse of that list

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv 
}