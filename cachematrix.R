## makeCacheMatrix returns a list of four functions:
## set() sets the value of a stored matrix and clears a cache of the stored matrix's inverse.
## get() returns the value of the stored matrix
## setcache() sets the cached stored inverse matrix to whatever the first parameter is,
##     without checking.
## getcache() returns the cached stored inverse, even if it is NULL.
## mat is the persistent storage of the stored matrix.

makeCacheMatrix <- function(x = matrix()) {
    cachedinverse <- NULL
    set <- function(m) {
        cachedinverse <<- NULL
        mat <<- m
    }
    get <- function() mat
    setcache <- function(c) cachedinverse <<- c
    getcache <- function() cachedinverse
    list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## cacheSolve returns the inverse of x, using a cached entry if it is available. 
## If there is nothing cached, it will cache the result for future use.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cache <- x$getcache()
    if(!is.null(cache)) {
        message("using cached data")
        return(cache)
    }
    mat <- x$get()
    cache <- solve(mat)
    x$setcache(cache)
    cache
}
