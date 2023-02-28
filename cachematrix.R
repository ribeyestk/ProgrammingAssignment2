###Here I am defining the function "makeCacheMatrix" with an empty matrix 
makeCacheMatrix <- function(x = matrix()) {
  ### Here, I jave defined inv as NULL and the function set takes the argument y
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ### The get is retrieving the value of x
  get <- function() x
  ### "setInverse" is takes a single argument inverse, but also sets the value of "inv" to "inverse"
  setInverse <- function(inverse) inv <<- inverse
  ### Here i am using get to retrieve the value of "inv"
  getInverse <- function() inv
  ### A list of the elements is being returned 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


### The argument x is used to define the function "casheSolve"
cacheSolve <- function(x, ...) {
  ### Here I am getting the inverse of the matrix using getInverse and denoting it to the variable "inv"
  inv <- x$getInverse()
  ### hERE i am using if/return feature, where "inv" is being checked for "null" -- if not , inverse has been computed
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}