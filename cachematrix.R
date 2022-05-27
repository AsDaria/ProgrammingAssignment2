## Put comments here that give an overall description of what your
## This function creates a special "matrix" object that can cache its inverse.

## consists get, set, setinv, getinv

library(MASS)
makeCacheMatrix <- ffunction(x = matrix()) {
      inv <- NULL #initializing inverse as NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x  #getting matrix x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() {
            inver <- ginv(x)
            inver%*%x #obtaining inverse of the matrix
      }
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above and retrieve the inverse from the cache

cacheSolve <- function(x, ...) { #getting cached data
      inv <- x$getinv()
      if(!is.null(inv)) {  #checking whether inverse is NULL
            message("getting cached data")
            return(inv) #returning inverse value
      }
      data <- x$get()
      inv <- solve(data, ...) #calculates inverse value
      x$setinv(inv)
      inv #returns a matrix that is the inverse of "x"
}
