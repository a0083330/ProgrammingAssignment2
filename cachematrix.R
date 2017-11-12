## The following functions will compute matrix inversion
## and cache result when the matrix inversion has already been computed

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y= matrix()) {
    x <<- y
    matrixInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) matrixInv <<- inv
    getInverse <- function() matrixInv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function returns inverse of matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInv <- x$getInverse()
    if(!is.null(matrixInv)) {
        message("getting cached data")
        return(matrixInv)
    }
    data <- x$get()
    matrixInv <- solve(data)
    x$setInverse(matrixInv)
    matrixInv

}
