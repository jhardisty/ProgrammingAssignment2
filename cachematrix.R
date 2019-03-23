## Function cacheSolve takes a matrix, finds its inverse, and caches that
## inverse. It checks to see if the the matrix inverse already exists.
## makeCacheMatrix creates the necessary functions to get and set matrix and
## matrix inverse

## Creates list that holds functions to get and set matrix and matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        #m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'. Checks for existing inverse of
## 'x' in global environment with variable 'm'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
