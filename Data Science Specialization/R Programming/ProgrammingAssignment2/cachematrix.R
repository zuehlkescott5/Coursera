

## makeCacheMatrix creates list of methods to set, get matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix.inverse = NULL
  setmatrix = function(y) {
    x <<- y
    matrix.inverse <<- NULL
  }
  getmatrix = function()
    x
  setinverse = function(inverse)
    matrix.inverse <<- inverse
  getinverse = function()
    matrix.inverse
  list(
    set = setmatrix,
    get = getmatrix,
    setinv = setinverse,
    getinv = getinverse
  )
}


## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

cacheSolve <- function(x, ...) {
  matrix.data = x$get()
  matrix.inverse = solve(matrix.data, ...)
  
  x$setinv(matrix.inverse)
  
  return(matrix.inverse)
}
