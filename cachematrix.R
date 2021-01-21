##The makeCacheMatrix is a function that creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    }
  get <- function() {x}
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function(){x}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##cacheSolve function is used to retrieve the cache data

cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){message("getting cached data")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
