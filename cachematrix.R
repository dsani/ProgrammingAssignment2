## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Set matrix function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Get matrix function
  get <- function() x
  ## Setting inverse matrix cache function
  setinv <- function(minv) inv <<- minv
  ## Get cached matrix function
  getinv <- function() inv
  ## Returns cachematrix data type 
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## Check if matrix has already been cached
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  ## Get matrix
  data <- x$get()
  ## Solve matrix
  inv <- solve(data, ...)
  ## Set cache
  x$setinv(inv)
  ## Return inverse matrix
  inv
}
