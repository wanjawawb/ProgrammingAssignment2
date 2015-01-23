## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  m_cache <- NULL # matrix cache
  
  set <- function(in_matrix) {
    x <<- in_matrix 
    m_cache <<- NULL 
  }
  
  get <- function() x
  setinverse <- function(inverse) x <<- inverse
  getinverse <- function() m_cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of the above matrix created by makeCacheMatrix 
## If inverse already calculated, then retrieve inverse from cache
cacheSolve <- function(x, ...) {
  
  m_cache <- x$getinverse() #get inverse of matrix
  
  if(!is.null(m_cache)) {
    message("No calculation done.  Answer just got from cache")
    return(m_cache)
  }
  data <- x$get()
  m_cache <- solve(data, ...)
  x$setinverse(m_cache)
  m_cache
}