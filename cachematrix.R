## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creats a special matrix, which is really a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse of the matrix
##  4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {  
  i <- x$getinverse()
  if(!is.null(i)) {     #if i is not null, return the inverse matrix from cached data
    message("getting cached data")
    return(i)
  }
  data <- x$get()       #if i is null, need to call solve() to get the inverse matrix
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##The following code will test the above functions
amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # first time call will Computes, caches, and returns matrix inverse
amatrix$getinverse()  # Returns matrix inverse 
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
