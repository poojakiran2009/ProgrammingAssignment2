## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # function for set
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        # function for get
        get <- function() x
        # function to set inverse
        setinv <- function(arg) m <<- arg
        # function to get inverse
        getinv = function() m
        # return a list
        return (list(set=set, get=get, setinv=setinv, getinv=getinv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinv()  
        # if exists in cache
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }

        data = x$get()
        m = solve(data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(m)
        
        return(m)
}
