## makeCacheMetrix creates a special matric object.
## functions do



makeCacheMatrix <- function(x = matrix()) {
        ##creates a special matrix that stores the matrix x and its inverted matrix invx
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invx <<- inv
        getinv <- function() invx
        list (set = set, get = get,
              setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinv()
        if(!is.null(invx)){
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data)
        x$setinv(invx)
        invx
}
