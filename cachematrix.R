## Overall, the functions will create a special "matrix," and then return the inverse of it via the cache.

## The function below contains a number of smaller functions in regard to creating a special "matrix."
## These smaller functions will set the value of the matrix, get the value of the matrix, set the value
## of the matrix's inverse, and get the value of the matrix's inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv_matrix <- function(inverse) m <<- inverse
  get_inv_matrix <- function() m
  list(set = set,
       get = get,
       set_inv_matrix = set_inv_matrix,
       get_inv_matrix = get_inv_matrix)
}

## The function below actually calculates the inverse of the special "matrix" that was generated in the first
## function. If the inverse has been calculated, nothing new will occur. However, what will happen otherwise
## is that the function will set the inverse of the matrix in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv_matrix(m)
  m
}