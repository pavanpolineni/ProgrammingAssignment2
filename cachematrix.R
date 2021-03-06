## Put comments here that give an overall description of what your

## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {            ##set function
                x <<- y
                m <<- NULL
        }
        get <- function() x             ##get function
        setinverse <- function(solve) m <<- solve       ##set the inverse of the matrix
        getinverse <- function() m                      ##get the inverse of the matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                
        if(!is.null(m)) {           #if there is a cache
                message("getting cached data") 
        return(m)                ##just return the cache, no computation needed
        }
        data <- x$get()             #if there's no cache
        m <- solve(data, ...)           #compute the inverse here
        x$setinverse(m)                 #save the result back to x's cache  
        m        ## m Returns a matrix that is the inverse of 'x'
}
