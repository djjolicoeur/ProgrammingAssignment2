## The following functions make it possible to 
## cache the inverse of am invertable matrix, eliding 
## the need for expensive duplications of the inversion 
## process. 


## Write a short comment describing this function
## - set value of matrix
## - get value of matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function
## The following function calculates the inverse of a 
## special "matrix", checking to see if we have already 
## cached the inverse or not.  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
