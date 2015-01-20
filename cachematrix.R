#example for running the code:
#mymatrix <- matrix(6:9,2,2)
#solvelist <- makeCacheMatrix(mymatrix)
#cacheSolve(solvelist)


#argument is a matrix
# returns a list
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invs) inv <<- invs
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#argument is a list
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) %*% data #inverse the matrix
  x$setinv(inv)
  inv
}