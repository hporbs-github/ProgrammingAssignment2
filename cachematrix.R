## Together, makeCacheMatrix & cacheSolve return and cache the inverse of a matrix 
## if it has not yet been calculated.  If it has been calculated already, they simply
## return the cached inverse.


## makeCacheMatrix creates a list of 4 functions that:
##    1: set()       cache the argument matrix
##    2: get()       retrieve the the cached argument matrix
##    3: setInverse  calculate and cache the inverse of the argument
##    4: getInverse  retrieve the cached inverse  

  makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(rep(NA, nrow(x)*ncol(x)), nrow(x), ncol(x))
    set <- function(y) {
      x <<- y
      m <<- matrix(rep(NA, nrow(x)*ncol(x)), nrow(x), ncol(x))
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
}


## cacheSolve checks to see if the inverse of the argument matrix has
## already been calculated.  If so, it retrieves the cached matrix.  If not,
## it calculates and caches it.  

  cacheSolve <- function(x, ...) {
    z <- x$getInverse()
    if(!is.na(z[1,1])) {                             
      message("getting cached data")              
      return(z)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }
