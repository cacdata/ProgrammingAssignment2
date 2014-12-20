## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can be read and set and 
## whose inverse can be computed

makeCacheMatrix <- function(x = matrix()) {
        ## reset inverse to null if making new matrx
        inv <- NULL
        ## create a set function so I can update the matrix object later if I need to
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get returns the original matrix
        get <- function() x
        ## setinverse/getinverse solve the inverse matrix and returns the inverse
        ## note if setinverse has not been called on the object than get inverse will return NULL
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        ## and the list of methods available to the calling function
        ## and how it maps to the internal names
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return the inverse of the passed matrix object
## that was created by makeCacheMatrix
## by either calculating the inverse or returning the previously 
## cached inverse if it has calculated it before for that object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## check to see if it already known
        inv <- x$getinverse()
        if(!is.null(inv)) {
                ## not null means we have a cached value
                ## so go ahead and return that
                message("getting cached data")
                return(inv)
        }
        ## if we got here, then have not previously cached
        data <- x$get()
        ## so use solve to get the inverse
        inv <- solve(data, ...)
        ## cache it
        x$setinverse(inv)
        ## return the newly calc'ed inverse
        inv
}

