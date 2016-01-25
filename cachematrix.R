## Two functions defined, one that caches the inverse of a matrix has the following functions listed
## 1. set: To set values of a matrix
## 2. get: To get values of a matrix
## 3. setinv: To find the inverse of the matrix and cache it
## 4. getinv: To get the cached inverse of the matrix
## The second function uses the previous function to get the cache value if there is one and if the matrix is unchanged.
## If a new matrix is provided, then its inverse is calculated and cached.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i        
}
