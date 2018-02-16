## Programming Assignment 2
## Functions below demonstrate the use of cache and lexical scoping in R


## makeCacheMatrix function takes matrix x as input, stores x and its inverse.
## It returns a list of 4 functions. It works together with the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve function takes as input an object of type makeCacheMatrix().
## If the inverse of the input is stored in cache, it returns it, otherwise
## it calculates it and sets it in the input object

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
