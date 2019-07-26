#matrix inversion is computationally heavy, so caching is useful at times.


#These functions create a matrix that caches its inverse
makeCacheMatrix <- function(x = matrix()){
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


#now we find the inverse of the matrix which has been stored in the function above.
#else, if the inverse has already been found, it'll just return the cached inverse.

cacheSolve <- function(x, ...){
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
