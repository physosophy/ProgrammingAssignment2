## Put comments here that give an overall description of what your
## functions do

## create a list of functions that can get or set the value and the inverse of the matrix input

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL;
  set <- function(y){
    x <<- y;
    inverse <<- NULL;
  }
  get <- function(){
    x;
  }
  setinverse <- function(inv){
    inverse <<- inv;
  }
  getinverse <- function(){
    inverse;
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse);
  
}


## get the inverse of a matrix. If not calculated before, calculate. If calculated already, get cached value. Uses output from makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse();
  if(!is.null(inverse)){
    message("getting cached data");
    return(inverse);
  }
  matrix <- x$get();
  inverse <- solve(matrix);
  x$setinverse(inverse);
  inverse;
}