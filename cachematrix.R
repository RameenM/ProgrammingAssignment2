## Create matrix object to cache its inverse
## Function Parameter: matrix, output:list

makeCacheMatrix <- function(x = matrix()) {
  # Initializing inverse value
  p <- NULL
  
  # Setting the matrix
  set <- function(y){
    x <<- y
    p <<- NULL
  }
  
  # Getting the matrix
  get <- function() x
  
  # Setting the inverse of the matrix
  setinverse <- function(inverse) p <<- inverse
  
  # Getting the inverse of the matrix
  getinverse <- function() p
  
  # Output list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Computing output for Inverse Matrix with parameter: matrix, output:inverse matrix

cacheSolve <- function(x, ...) {
  p <- x$getinverse()
        ## Returning matrix if it is the inverse of 'x'
  
  # Returning matrix if it is the inverse of x matrix
  if(!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  
  # Getting matrix from the object
  data <- x$get()
  
  # Solving inverse using matrix multiplication
  p <- solve(data, ...)
  
  # Setting inverse of inverse matrix
  x$setinverse(p)
  
  # Returning matrix
  p
}

