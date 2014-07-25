## a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     Ma_inver <- NULL
     set <- function(y){
       x <<- y
       Ma_inver <- NULL
     }
     get <- function() x
     setMa <- function(Ma)  Ma_inver <<- Ma
     getMa <- function()  Ma_inver
     list(set=set,get=get,setMa=setMa,getMa=getMa)
}

## This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
     Ma_inver <- x$getMa()
     if(!is.null(Ma_inver)){
        message("getting cached data")
        return(Ma_inver)
     }
     data <- x$get()
     Ma_inver <- solve(data,...)
     x$setMa(Ma_inver)
     
## Return a matrix that is the inverse of 'x'
     Ma_inver
}
