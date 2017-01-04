## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly 
## This R file contains a pair of functions that cache the inverse of a matrix.



## The first function, makeCacheMatrix, uses x (a square invertible matrix)
## and returns a list containing functions to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                     # set the matrix inverse to NULL as a placeholder for a future value
    set <- function(y) {                            # define a function to set the matrix (x) to a new matrix (y)
            x <<- y                                 # set the matrix (x) to a new matrix (y)
            inv <<- NULL                            # reset inv to NULL
    }
    get <- function() x                             # return matrix (x)
    setInverse <- function(inverse) inv <<- inverse # sets the inverse (inv) to inverse
    getInverse <- function() inv                    # returns the inverse (inv)
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)                   # return list of functions
}


## The second function, cacheSolve, uses x from makeCacheMatrix()
## and returns the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()                           # try to retrieve matrix inverse from makeCacheMatrix()
    if(!is.null(inv)) {                             # check if the inverse has been computed
      message("getting cached matrix inverse")      # if the inverse has been computed, retrieve matrix inverse
      return(inv)                                   # display inverse matrix
    }
    data <- x$get()                                 # when matrix inverse does not exist, get original matrix (x)
    inv <- solve(data, ...)                         # solve matrix inverse
    x$setInverse(inv)                               # set inverted matrix in cache
    inv                                             # display inverse matrix
}

