makeCacheMatrix <- function(x = matrix()) {
        
        #inv is cache value for inverted matrix
        inv <<- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
                #print(x)
        }

        get <- function() x
        setInverse <- function(inverse)  inv <<- inverse
        getInverse <- function() inv
        #print(inv)
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        #print(x)
}



cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        message ("hello")
        x$setInverse(inv)
        inv
}