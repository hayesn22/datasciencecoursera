## makeCacheMatrix creates a matrix and can cache the inverse. CacheSolve does the process of computing the inverse. If it is attempted to cache an inverse in
## that has already gone through the process, then the same inverse matrix will be returned again. 

##makeCacheMatrix sets the matrix as well as gets it. Then it takes that matrix and sets and gets the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## CacheSolve computes the inverse of the matrix created in makeCacheMatrix. If the inverse has already been computed, cacheSolve will return the already solved matrix again. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
