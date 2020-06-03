## makeCacheMatrix creates a list containing function for
## set the value of Matrix
## get the value of Matrix
## set the value of inverse of Matrix
## get the value of inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## It returns the inverse of a matrix returned by CacheMatrix 

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
       if(!is.null(inv)){
         message("getting cached data")
         return(inv)
       }
       data <- x$get()
       inv <- solve(data)
       x$setinverse(inv)
       inv
}
