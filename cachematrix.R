## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inversed result of the matrix
## 4.  get the inversed result of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
            setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inversed result of the special "matrix"
## created with the above function. However, it first checks to see if the
## inversed result has already been calculated. If so, it `get`s the one from the
## cache and skips the computation. Otherwise, it calculates it of
## the data and sets the value of it in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
