
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
  
  xinv <- NULL 
  set <- function(y) {
    m <<- y
    xinv <<- NULL 
  }
  
  get <- function() m 
  setInv <- function(inv) xinv <<- inv 
  getInv <- function() xinv 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInv() 

  if(!is.null(m)) { 
    message("getting cached data")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}
