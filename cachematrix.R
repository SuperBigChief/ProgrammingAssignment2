# This function creates a special "matrix" object that can cache its inverse
# sets the value of the matrix
# gets the value of the matrix
# sets the value of the inverse matrix
# gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
# initializes the inverse matrix value
        inv <- NULL
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the value of the inverse matrix
        set_inverse <- function(inv_input) inv <<- inv_input
        # get the value of the inverse matrix
        get_inverse <- function() inv
        # return a list of all the functions listed above
        list(set = set, get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse)
}

## The following function calculates the inverse of the special
## "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache via
## the setinv function.

cacheSolve <- function(x, ...) {
# check to see if the inverse is already cached,
# if so, we get the inverse from the cache directly
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        # else, we first get the matrix
        data <- x$get()
        # and calculate the inverse
        inv <- solve(data, ...)
        # next, cache the inverse of the matrix
        x$set_inverse(inv)
        # and finally, return the result
        inv
}
