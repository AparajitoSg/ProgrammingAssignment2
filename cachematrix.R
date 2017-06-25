## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {## Define the argument with default mode of "matrix"
  inv <- NULL    ## This will initialize "inv" as "NULL" and will hold the value of the inverse matrix
  
  set <- function(y) {## to define the set function and to assign a new 
    x <<- y          ## value of the matrix in parent environment
    inv <<- NULL     ##If there is new matrix to reset inv to "NULL"
    
  }
  get <- function()x  ## define the get function  returns the value of the matrix arg.
  
  
  setInverse <- function(inverse) inv<<- inverse ## it assigns value of inv in parent environment
  getInverse <- function()inv  ## get the value of inv where it is called
  
  list(set = set,
       get = get,
       setInverse = setInverse, 
       getInverse = getInverse) 

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

