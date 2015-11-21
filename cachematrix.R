makeCacheMatrix <- function(x = matrix()) {
  ##  Creates a list with functions to manipulate the given matrix.
  ##
  ##  Args:
  ##    x - Matrix. Default is: matrix().
  ##
  ##  Returns:
  ##    A list containing 4 functions to manipulate the matrix:
  ##      set()      - sets the value of the matrix,
  ##      get()      - gets the value of the matrix,
  ##      setsolve() - sets the inverse of the matrix,
  ##      getsolve() - gets the inverse of the matrix.
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  ##  Computes the inverse of the given matrix.
  ##
  ##  Args:
  ##    x   - Invertible matrix,
  ##    ... - Further arguments passed to solve() function.
  ##
  ##  Returns:
  ##    The inverse of the matrix.
  
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
