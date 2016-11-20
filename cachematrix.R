## these functions find the inverse of a matrix while storing it in a cache to
## save processing time it the inverse is required again

## makeCacheMatrix takes a matrix as input and creates a list of functions
## that allow us to store and fetch a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve allows us to find the inverse to a matrix created with the  
## above function. It first checks to see if the inverse exists, and if so,
## gets the inverse from the cache without performing the inverse. Otherwise
## it finds the inverse and puts the value in the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
