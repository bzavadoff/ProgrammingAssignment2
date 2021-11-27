## The functions below create a matrix, compute its inverse, and 
## caches the inverse to avoid repeating the computation

## Creates a matrix object that can cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Computes and returns a matrix that is the inverse of 'x'

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
