## makeCacheMatrix to make a matrix in cache
##cacheSolve to solve for inverse of a matrix after checking in cache


# Returns:
# A matrix with functions to get/set value & get/set inverse
makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inver <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinv <- function() inver
  setinv <- function(inverse) inver <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}



# Returns:
# The inverse of the matrix
cacheSolve <- function(x, ...) {
  inver <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inver)) {
    message("inverse is cached")
    return(inver)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inver <- solve(m, ...)
  
  # cache inverse
  x$setinv(inver)
  
  # return inverse of matrix
  return(inver)
}