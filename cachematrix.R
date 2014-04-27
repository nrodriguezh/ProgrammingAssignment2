## These functions are part of the Peer Assignment for 
## the R Programming course, in which two function 
## cache the inverse of a matrix

## This function will create functions to be used by the second function 
## (cacheSolve).
## The output of makeCacheMatrix will be four functions that:
##    a. set function, 
##    b. get function, 
##    c. set the inverse,
##    d. get that inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                                 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
}


## This function uses makeCacheMatrix to return a matrix that is the 
## inverse of the set matrix 'X'.
## The first time cacheSolve runs the 'm' variable is NULL and it 
## will not solve for a matrix inverse.
## The second time it recognizes that 'm' was created and caches the 
## inverse of the matrix.

## See examples created for the test



cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()                 ## checks if there is an 'm' 
  if(!is.null(m)) {                     ## verifies if the 'm' is NULL.
    message("getting cached data")      ## If the 'm' isn't NULL it'll return it
    return(m)
  }
  data <- x$get()                       ## If the 'm' is NULL it gets x (a matrix)
  m <- solve(data, ...)                 ## It gives 'm' the value of the inverse of x
  x$setInvMatrix(m)                     ## This saves the inverse into the cache
  m                                     ## This returns the inverse of the matrix
}


#test <- matrix(data = c(1,0,5,2,1,6,3,4,0),nrow = 3, ncol = 3) 
#test2 <- makeCacheMatrix(test)
#test3 <- cacheSolve(test2)
#test3 <- cacheSolve(test2)