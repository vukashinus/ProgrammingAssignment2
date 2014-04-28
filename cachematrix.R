
## Function returns a list of 4 element functions 
## whihch (1) get the matrix, (2) set the matrix, (3) get the matrix inverse and 
## (4) set the matrix inverse 
## Argument which function accepts is the initialized matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function checks if the inverse of the matrix already exists, 
## and if not it computes its value and stores it in cache variable
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## if the inverse matrix was already compute before, then 
  ## pull it out from cache variable m
  if(!is.null(m)) {
    message("getting cached inverse matrix data")
    return(m)
  }
  
  ## otherwise get the matrix and precompute its inverse
  data <- x$get()
  m <- solve(data, ...)
  
  #further, once the inverse is computed set the cache variable to its value
  x$setInverse(m)
  m
}
