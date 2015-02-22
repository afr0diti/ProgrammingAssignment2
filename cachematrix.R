## Here are two functios that are used to cache the inverse of a matrix

## The first function creates a special "matrix" object that can cache its inverse
## It is a list containing four other functions: set, get, set_inv, get_inv

makeCacheMatrix <- function(x = matrix()) {
  i <- matrix()
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  set_inv <- function(inv) i <<- inv
  get_inv <- function() i
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## The second function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. 
## If the inverse is already calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache and skips the computation 
## Otherwise it calculates the inverse of the matrix and sets 
## the inversed matrix in the cache via the set_inv function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get_inv()
  
  if(!is.na(i[1,1])) {
    ## If the inverse is already calculated, the inverse is retrieved 
    ## from the cache 
    message("getting cached data")
    return(i)
  }
  ## otherwise calculate the inverse of the matrix and set it in the cache via
  ## the set_inv function
  mat <- x$get()
  i <- solve(mat)
  x$set_inv(i)
  i
}