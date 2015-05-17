## cachematrix.R provides two functions:
##
## makeCacheMatrix() create a special "matrix" that could caches its inverse.
## cacheSolve() returns the inverse of a "matrix" created from makeCacheMatrix.
##               The inverse is retrieved from cache, if available; otherwise 
##               the inverse will be computed and cached.
##
## Example Usage:
## > m <- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0),3,3))
## > cacheSolve(m)  # first calculate the inverse and set to cache
## > cacheSolve(m)  # retrieve from cache instead of recompute the inverse

## makeCacheMatrix(x) -- create cacheable matrix.
## 
## Usage: makeCacheMatrix(x)
## 
## x is a R matrix object. makeCacheMatrix take a matrix object as input and
## return a list of 4 functions (set, get, setsol, getsol):
## set    - set the value of the matrix
## get    - get the value of the matrix
## setsol - set the value of the inverse
## getsol - get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsol <- function(solve) s <<- solve
        getsol <- function() s
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}


## cacheSolve(x) - compute the inverse of x or retrieve the cached inverse
##
## Usage: cacheSolve(x)
## 
## x is a "matrix" created from makeCacheMatrix. When cacheSolve is called for
## the first time on x, the inverse for "matrix" x will be computed and stored.
## However, in subsequent call on the same "matrix" x, the cached value of the
## inverse will be returned, instead of recomputing the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsol()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsol(s)
        s
}
