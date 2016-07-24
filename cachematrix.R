makeCacheMatrix <- function(x = matrix()) {
  ## x: a square invertible matrix
  ## VVIMP : return: a list containing functions to Set & get the matrix and set & get the inverse
  ##  VVIMP : this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## VVIMP : x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
  ## Example : Taken 9 normal Random Variables 
n <- matrix(rnorm(9),3,3)
  ## prints the value of n
n
  ## h  return: a list containing functions to Set & get the matrix and set & get the inverse
  ## this list is used as the input to cacheSolve()
h <- makeCacheMatrix(n)
  ## returns inverse
cacheSolve(h)
