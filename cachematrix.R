## makeCacheMatrix stores the value of the inverse matrix in memory if 
## already calculated while cacheSolve solves for the inverse matrix
## if it's new, otherwise retrieves it from memoery from makeCacheMatrix

## stores inverse of matrix in memory

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list( set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


## solves the inverse of the matrix if not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##test
x <- matrix(1:4,2,2)
z <- makeCacheMatrix(x)
z$get()
cacheSolve(z)
cacheSolve(z)
