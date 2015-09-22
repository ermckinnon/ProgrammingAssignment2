## Inverse Matrix Solver 

# makeCacheMatrix & CacheSolve are functions for solving the inverse of a matrix
# They save computation time by storing results in cache memory and retreiving 
# previous computations from memory if they have already been calculated.


# makeCacheMatrix stores a list of four functions:
# set(),get() which store and return the univerted matrix to be solved
# setinverse(),getinverse() which store and return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  
        
}


## cacheSolve checks if the matrix has already been inverted
## If it has been inverted already the solution is retrieved from the cache
## If it has not been inverted the inverse is calculated and stored in cache 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting Inverse of Matrix from Cache")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
