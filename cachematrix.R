## These functions pair together to cache the result of a inverse matrix computation and 
## allow for easy/quick recall of the result. his avoids redundant inverse matrix computations
## when working with the same matrix. All matrices are assumed to be square.

## This function creates a list of functions ('setters' and 'getters'). These functions and 
## their associated objects ('x' and 'inv') are saved in a S3 object for the specific matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function prints the inverse matrix computation for a specific matrix. Step 1: checks
## if the computation has already been computed and recalls/prints it if available. Step 2: 
## computes the inverse matrix computation if unavailable and prints it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
