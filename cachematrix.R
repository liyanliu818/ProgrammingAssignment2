## The following functions can create matrix which will cache its inverse
## for quick calculation in future calls.

## make a matrix which can store a value or its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
         x <<- y
         s <<- NULL
   }
   get <-function() x
   setsolve <- function(solve) s <<- solve  
   getsolve <- function() s
   list(set = set, get = get,
       setsolve = setsolve
       getsolve = getsolve
}
 

## calculate and cache the inverse of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached data")
            return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
