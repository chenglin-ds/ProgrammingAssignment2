## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## Initiate a Null matrix 
    inv <- NULL
## set the value of a matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
## assign (get) the value x to the matrix
    get <- function() x
## set the inv
    setinv <- function(invers) inv <<- invers
## return the inverse matrix
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
## first see if inv has been calculated
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
## if not yet stored in the cache, calculate it
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}