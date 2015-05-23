

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse.
  inv<- NULL               
  set <- function(y) {     ## For setting the matrix y in the cache  
    x <<- y                ## Set the matrix y in the cache
    inv <<- NULL           ## Initialise the inverse matrix in the cache
  }
  get <- function() x      
  setinverse <- function(inverse) inv <<- inverse   ## This function is for setting the inverse matrix in the cache
  getinverse <- function() inv                      ## For getting the actual inverse matrix in the cache
  list(set = set, get = get,                                   
       setinverse = setinverse,
       getinverse = getinverse)
  
}




cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {      ## if the inverse of the matrix in the paramter is already in the cache wet return it it
    message("getting cached data")
    return(inv)
  }
  data <- x$get()          ## If the inverse of the matrix in parameter is not in the cache we
  inv <- solve(data, ...)  ##  calculate it with the solve function 
  x$setinverse(inv)        ## Modify the inverse matrix in the cache       
  inv                      ## Return the inverse of the matrix x             
}
