## The function below creates a matrix that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function below computes the inverse of the matrix obtained
## from the above function. If already previously calculated, the 
## function below will retrieve it from the cache

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m ## Return a matrix that is the inverse of 'x'
}
