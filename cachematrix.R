## Catching the inverse of a matrix

## The two functions are used to create a special object that 
## stores a numeric vector and caches its inverse

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse  = setinverse,
       getinverse = getinverse )
}


## makeCacheMatrix function creates a special vector 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

 A <- matrix(c(1,2,3,4),2,2)
S <- makeCacheMatrix(A)
cacheSolve(S)
cacheSolve(S)
