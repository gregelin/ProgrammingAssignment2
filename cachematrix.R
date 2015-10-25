## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # first make a local variable `im` for the inverse matrx
  im <- NULL
  # enable setting of global env variables
  set <- function(y) {
    ## scope matrix to a global env variable `x`
    x <<- y
    ## scope an empty global env variable `im`
    im <<- NULL
  }
  ## return original matrix
  get <- function() {x}
  ## Set `im` to be calculated inversematrix in global environment lexical scope by using `<<-`
  setinverse <- function(inversematrix) {
    im <<- inversematrix 
  }
  # retrieve the inverse matrix of x
  getinverse <- function() {im}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## assume matrix `x` is always invertable

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # `x` is a matrix
  # `ix` is inverse of matrix

  # `x` is makeCacheMatrix object
  ix <- x$getinverse()
  if (!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  
  ## inverse the matrix
  message("calculating inverse and caching result")
  matrix <- x$get()
  ix <- solve(matrix, ...)
  x$setinverse(ix)
  ix
}