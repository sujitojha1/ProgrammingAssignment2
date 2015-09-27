## Caching example for finding matrix inversion
##  caching the inverse of a matrix rather than compute it repeatedly 

## Function for a special matrix with mean function included

makeCacheMatrix <- function(x = matrix()) {

  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) x_inv <<- inv
  get_inv <- function() x_inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
  
}


## Function to return matrix inverse from cache if its already computed
## or return by calculating inverser using SOLVE()

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
