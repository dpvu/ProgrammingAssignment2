## Put comments here that give an overall description of what your
## functions do

## A function to create a special object that contains a matrix and is capable 
## of storing a cache of the inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL # Set inv to null if matrix is changed with set
  }
  get <- function() x
  setinv <- function(inverse){
    inv <<- inverse
  } 
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## A function that returns the inverse of the matrix x from a special object 
## created with makeCacheMatrix function. The cache is first checked to see
## if the inverse has already been computed and stored. If it has, the cached
## inverse is returned. If not, it is computed, cached in the object and returned

cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
