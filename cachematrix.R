##makeCacheMatrix function does 4 things
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse matrix
# 4.get the value of the inverse matrix

##cacheSolve funcion does:
#calculates the inverse of the special matrix calculated by makeCacheMatrix
#if the inverse has already been calculated and the matrix has not changed 
#the function retrieves cache from the cache

## Function creating special matrix that can be cached

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve #use solve function to inverse
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Function calculating matrix inverse (if inverse already calculated uses cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
