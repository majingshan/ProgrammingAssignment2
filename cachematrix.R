## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# set the initial inverse to be NULL
    inv <- NULL
    
    # create setter and getter of the matrix
    set <- function(y) {
        x <<- y
        # each time reset the matrix, set the inverse to be NULL
        inv <<- NULL
    }
    get <- function() x
    
    # create setter and getter of the inverse
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    # return a list with setters/getters of matrix and inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # if the inverse is not NULL, return the cached inverse
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("get cached inverse")
        return(inv)
    }
    
    # calculate the inverse and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
