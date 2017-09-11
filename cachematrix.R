## The functions below does matrix inversion, and caches the results
#  for more efficient processing

## makeVector creates a special "vector", which is really a list containing a function to
#  1. set the value of the vector
#  2. get the value of the vector
#  3. set the value of the mean
#  4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(inp) {
    x <<- inp
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the mean of the special "vector" 
#  created with the above function. However, it first checks to see 
#  if the mean has already been calculated

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
