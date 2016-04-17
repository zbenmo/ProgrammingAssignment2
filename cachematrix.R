## The functions below deal with caching a matrix and its inverted
## form.

## This function "constructs" a wrapper for the matrix and its inverted value.
## an optional parameter is the matrix. If not provided here it can be set
## later.
## The return value is a list of functions to set/get the matrix value or the
## inverted value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function () x
        setinv = function (x.inverted) {
                inv <<- x.inverted
        }
        getinv = function () inv
        
        # the return value
        list(
                set = set,
                get = get,
                setinv = setinv,
                getinv = getinv
        )
}


## This function is making use of the chached matrix parameter
## as to return the invert of the matrix. The inverted value is either available
## in the cache or is calculated, saved for additional future usages.
## Cached or just calculated the inverted matrix is then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        if (!is.null(inv)) return(inv)
        mat = x$get()
        inv = solve(mat, ...)
        x$setinv(inv)
        inv
}
