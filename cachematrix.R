
## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## The function "cacheSolve" computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## create set function which takes floating variable y 
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ##if matrix not found, then create it with this function  
  get <- function() x
  ##set inverse of the matrix using solve function
  setinv <- function(solve) s <<- solve
  ## return inverse of the matrix
  getinv <- function() s
  ##create list that contains the above four mentioned functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ##  check If the inverse has already been calculated and stored in cache and the matrix has not changed  
  s <- x$getinv()
  ##if inverse is found, retrieve from cache
  if(!is.null(s)) {
    message("getting cached data for the matrix inversion")
    return(s)
  }
  ##if not found in cache go to makeCacheMatrix function and retrive matrix
  data <- x$get()
  ## create the inverse of matrix
  s <- solve(data, ...)
  ##set inverse so it is stored in cache
  x$setinv(s)
  s
}