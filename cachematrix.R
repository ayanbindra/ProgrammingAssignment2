## makeCacheMatrix to make a matrix in cache
##cacheSolve to solve for inverse of a matrix after checking in cache


# Returns:
#   A matrix with functions to get/set value & get/set inverse
makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}



# Returns:
# The inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
}