## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly
## The following pair of functions will cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   	inv <- NULL
    	set <- function(y) {
        	x <<- y
        	inv <<- NULL
    	}
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve  function matrix returns the inverse of the matrix
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function. If you pass identity matrix/Singular it will display a message


cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
if(checksingular(data) == TRUE){
    inv <- solve(data)
    x$setinverse(inv)
    inv
}else{
        print("The matrix is singular")
      }

}


##Check if the matrix is singular
checksingular <- function(m) class(try(solve(m), silent=T))=="matrix"
