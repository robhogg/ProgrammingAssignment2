## A pair of functions illustrating the caching of data in a closure.

## Function (optionally) takes an invertible matrix as argument.
## Returns a CacheMatrix closure with the following methods:
## set(x): stores matrix x, and clears any previously cached data.
## get(): retruns the base matrix.
## setInv(): calculates and caches the inverse of the matrix.
## getInv(): returns the inverse of the base matrix. Triggers the calculation
##           of the inverse, if not already cached.
## N.B. setInv() and getInv() return an error if base matrix is not invertible.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function() {
        if(is.null(inv)) {
            print("Calculating and storing inverse")
            inv <<- solve(x)
        } else {
            print("Inverse already cached")
        }
    }
    getInv <- function() {
        setInv()
        inv
    }
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Takes a CacheMatrix closure as first argument, and returns the
## inverse of the CacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x$getInv()
}
