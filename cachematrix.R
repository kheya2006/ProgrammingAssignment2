## The goal of this program is to calculate the inverse of a matrix. As the calculation can be expensive, the result is 
## stored in a cache and if the same matrix is called, the function will deliver the output from the cache stored before 
## instead of recalculating it.

## makeCacheMatrix function contains four different functions to get, set , inverse the matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function calculates the inverse of the given matrix if not available from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
