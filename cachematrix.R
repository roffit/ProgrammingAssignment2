## This program makes a special 'matrix' object that can cache its inverse
## and then has the ability to return the inverse of the matrix without 
## calculating if it has been calculated prior, it can also calculate the
## inverse if it has not yet been cached

## This function creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(invert) i <<- invert
        getInverse <- function() i
        list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function returns the inverse of a matrix. It will return the cached
## value if the inverse has ever been calculated and it hasn't been changed.
## It will calculate the inverse using makeCacheMatrix otherwise.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
                message('getting chached data')
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}

