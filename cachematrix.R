## Assigment 2 Christian Jaehnichen

## cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get the matrix
  get <- function() x
  ## set inverse
  setinverse <- function(inverse) i <<- inverse
  ## get inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## get inverse matrix or output of cached

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ## check if matrix already has an calculated inverse cached
  if(!is.null(i)) {
    ## text output that cached inverse is returned
    message("getting cached data") 
    ## return cached inverse
    return(i) 
  }
  data <- x$get()
  ## calculate inverse for the cases it is not cached
  m <- solve(data, ...) 
  x$setinverse(m)
  m
}
