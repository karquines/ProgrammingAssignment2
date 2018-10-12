## makeCacheMatrix is a function that creates a matrix object that can cache its inverse
## casheSolve is a function that creates the inverse of the matrix created in makeCasheMatrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## casheSolve is a function that creates the inverse of the matrix created in makeCasheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cahed data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
