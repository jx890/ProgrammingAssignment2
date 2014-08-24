## Both functions below create an object that stores a given matrix
## and caches the inverse of the matrix
## Note - File ProgAssig2_Examples provides example of usage

## "makeCacheMatrix" - This function creates 
## a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##originally set to null
  set <- function(y) {
    x <<- y
    inv <<- NULL ##re-set to null for new vaue provided
  }
  get <- function() x ## returns matrix value
  setinverse <- function(inverse) inv <<- inverse ##sets inverse of matrix
  getinverse <- function() inv ## returns the inverse of matrix set before
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" - This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  ## If inverse is cached, return it
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if inverse is not cached then calculate, store & return
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
