

##The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  jj <- NULL
  set <- function(y) {
    x <<- y
    jj <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) jj <<- inverse
  getinv <- function() jj
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
        
cacheSolve <- function(x, ...) {
  jj <- x$getinv()
  if (!is.null(jj)) {
    message("getting cached data")
    
    return(jj)
  }
  data <- x$get()
  jj <- solve(data, ...)
  x$setinv(jj)
  jj
}

