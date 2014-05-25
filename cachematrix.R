### The 2 functions here operate to take a matrix, either by definition
## using the MakeCacheMatrix function, or by setting it
## by using the "set" function in the MakeCacheMatrix list. The inverse
## matrix can be found by using an in-built R function "solve"
## which is calculated within the "cacheSolve" function below. This
## latter operation can be called by entering matrix data directly into
## "cacheSolve" function.

#################################################################################
## This first function stores 4 items (functions in fact) in a list, 
## they are "get", "set", "getsolve", and "setsolve". 
## The "set"function resets the stored inverse matrix to "NULL" and thus resets
## the scope. By defining a scope of data and its calculated attributes
## this way, by saving them together in a list, avoids making needless
## recalculation of "recycled" calculated values.
## What follows is a simple editing of a structure of a program flow
## adapted from an example provided, i.e., "makeCacheMean".

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

########################################################################################
## This is the function that finds the inverse of a matrix that is
## already defined or set using the MakeCacheMatrix list. It first
## checks to see if the inverse has already been calculated and
## stored (i.e., "is not NULL"). If it is NULL then it
## calculates and caches the inverse to the matrix that was lastly
## set. This procedure is necessary to ensure that the matrix has
## not changed since the last time the function was performed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting inverse matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
