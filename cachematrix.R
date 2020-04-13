
## Initialize objects m and x
## makeCacheMatrix() creates an R object that stores a vector and its inverse. 
## The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() 
## in order to retrieve the inverse (by solve) from the cached value that is stored in the makeCacheMatrix() 
## object's environment. So at the first time the invertation is calculated and then the value is taken out
## of the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
