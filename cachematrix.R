# Matrix inversion can be computational costly.
# It saves time to cache the inverse of a matrix rather than  
# compute it repeatedly.
# The following two functions are for caching the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
# The cashSlove function returns the inverse of the matrix.
# It does the following:
# 1.Check if the inverse has already been computed.
# 2. If so, get the result and skips the computation.
# 3. If not, compute the inverse.
# 4. Set the value in the cache via setinverse function.
# 5. Print the the inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
## Return a matrix that is the inverse of 'x'
}

