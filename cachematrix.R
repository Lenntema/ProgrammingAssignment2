## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This file comes up with a pair of functions
## that cache the inverse of a matrix

## The following function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set = function(z) {
                x <<- z
                inv <<- NULL
        }
        
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function inv
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if (!is.null(inv) {
                message("retrieving cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$set_inverse(inv)
        inv
}
