## Overall remarks:
##
## These functions calculate the inverse of a matrix.
## The first function caches that inverse.
## The second function calls on parts of the first to either
## calculate the inverse or, if the inverse has been calculated
## already, to use the cached value the first function generated.

## makeCacheMatrix comments:
##
## This function creates a list containing a function that sets the value of a matrix,
## gets the value of that matrix,
## calculates and sets the inverse of that matrix,
## and gets that inverse matrix value.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    
    set <- function (y) {
        x <<- y
        m <<- NULL
    }

    get <-function() x

    setinverse<-function(solve) m <<-solve
    
    getinverse<-function() m
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
    
}

## cacheSolve comments:
##
## This function returns a matrix that is the inverse of 'x'
## If the inverse has already been cached, it simply returns the cached value instead

cacheSolve <- function(x, ...) {
    
    m<-x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data<-x$get()

    m<-solve(data,...)
    x$setinverse(m)
    
    m
    
    
}
