## Put comments here that give an overall description of what your
## functions do

## Create a list that represents a matrix 'x' jointly
## with the inverse of 'x'.


makeCacheMatrix <- function(x = matrix()) {
 	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of the "CacheMatrix" 'x' created
## by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinverse()
        if(!is.null(inv)) {
                #message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
