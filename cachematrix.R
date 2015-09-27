# makeCacheMatrix creates a list with four functions to:
#
# 1. set a matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    m_inv <- NULL
    
    set <- function(y) {
      x <<- y
      m_inv <<- NULL
    }
    get <- function() x 
    setinv <- function(inverse) m_inv <<- inverse 
    getinv <- function() m_inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  }


# The function cacheSolve calculates the inverse of a matrix.
# cacheSolve first checks if the inverse of the matrix exists already.
# If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {

    m_inv = x$getinv()
  
    if (!is.null(m_inv)){
    message("getting cached data")
    return(m_inv)
  }
  
  data = x$get()
  m_inv = solve(data, ...)
  x$setinv(m_inv)
  
  return(m_inv)
}
