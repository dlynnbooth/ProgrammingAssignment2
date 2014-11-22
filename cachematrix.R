## makeCacheMatrix() creates matix objects that can cache their inverse
## cacheSolve() calculatues the inverse of that matrix
## if the inverse has already been calculated, it retrieves it from the cache

## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns a matrix that is the inverse of 'x'
## If 'x' is a new matrix, the inverse is calculated
## If the inverse of 'x' has already been calculated, the cached value is returned
## 'x' must be a matrix which is invertable

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
