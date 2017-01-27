# Below are functions to create a special object, that stores a matrix 
# and cache its inverse

# The first function, makeCacheMatrix, creates a special "matrix",
# which is really a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


# The second function cacheSolve computes the inverse of the special "matrix" 
# returned by makeCahceMatrix.  If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m     # Return a matrix that is the inverse of 'x'
}


##### Test
matrix_test <- matrix(c(7,0,-3,2,3,4,1,-1,-2), 3,3)

a <- makeCacheMatrix(matrix_test)
cacheSolve(a)
cacheSolve(a)  # a message "getting cached data" should appear
