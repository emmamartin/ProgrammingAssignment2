## Aim is to create a pair of functions that can cache the inverse of a matrix
## as this can take a long time, and don't want to have to repeat every time 


## the 1st function makeCacheMatrix will create a matrix object that can cache
## it's inverse


makeCacheMatrix <- function(x = matrix())  {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  
  #  output is a list of functions
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}




## the 2nd function computes the inverse of the matrix, if it has already been 
## returned it will retrive the cached value

cacheSolve <- function(x, ...) {
  
  m <- x$getmat()
  
  # if the value is not null (so it's been cached already) return 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}






