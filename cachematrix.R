## makeCacheMetrix creates a special matrix object with a getter and setter funtcions for the matrix itself 
## and its inverted matrix.
## cacheSolve checks if the given matrix has been inverted, if not it calculates it. 
##Returns the inverted matrix in both cases. 


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
