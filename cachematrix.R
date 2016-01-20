## The following rows present two functions to cache a matrix
## and to calculate the inverse of such matrix

## The function 'makeCacheMatrix' caches a inversible matrix
makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                 x <<- y
                 Inv <<- NULL
         }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() Inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The function 'cacheSolve' computes the inverse of cached matrix

cacheSolve <- function(x, ...) {
        Inv <- x$getinv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        mat.data <- x$get()
        Inv <- solve(mat.data, ...)
        x$setinv(Inv)
        Inv
        ## 'Inv' is the matrix that is the inverse of 'x'
}
