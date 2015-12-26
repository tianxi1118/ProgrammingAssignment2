##Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly.
##Below are the functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix<- function(x = matrix()) {  ## create a matrix object called x
  m <- NULL
  set <- function(y) {       
    x <<- y                             ## set inverse matrix to be null 
    m <<- NULL
  }
  get <- function() x
  setMaInv <- function(inverse) m <<- inverse       ##create funciton called inverse to set inverse matrix 
  getMaInv <- function() m
  list(set = set, get = get,
       setMaInv = setMaInv,
       getMaInv = getMaInv)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getMaInv()                             ## call getMaInv function to retrieve the inverse from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)                      ## call solve function to return its inverse matrix
  x$setMaInv(m)
  m
}

