## This file contains two functions: makeCacheMatrix and cacheSolve.
## These functions are useful when repeatedly calculating
## the inverse of large matrices. We can cache the inverse so we
## don't need to repeatedly perform the same calculations.


## This function takes a matrix x as input, and outputs a "special"
## matrix, which is a list of 4 functions. One to get the value of 
## x, one to set the value of x, one to set the inverse of x, and 
## one to get the inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(solved) {
                inverse <<- solved
        }
        get_inverse <- function() inverse
        list(set = set, get = get,
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
}


## This function computes the inverse of special matrix x (the output
## of the above function.)
## If the inverse has been previously calculated and cached, 
## the function outputs the cached matrix instead of recomputing it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if (!is.null(inverse)){
                message('Retrieving cached inverse')
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$set_inverse(inverse)
        inverse
}
