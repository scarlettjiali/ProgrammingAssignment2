## Put comments here that give an overall description of what your
## functions do

## make usage of makeCacheMatrix() and cacheSolve() to get inverse of matrix from cache.

## Write a short comment describing this function

## makeCacheMatrix() build a special matrix for a given matrix input
## mainly contain functions of set(), get(), setinv(), getinv()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## compute the inverse of the matrix with the input of the output of makeCacheMatrix
## First check whether already calculate the inverse matrix, 
## If so get the result from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting inv from cache")
        return(inv)}
    else{
        data <- x$get()
        inv <- solve(data) # Get inverse value
        x$setinv(inv)
        inv
    }
}
