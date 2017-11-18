## Computes inverse of matrix and put in the cache
## how to call - cacheSolve(makeCacheMatrix(m))

## creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  matrix(list(set,get,getInverse,setInverse),2,2)
}


## computes the inverse of the matrix retrun from makeCacheMatrix function if not in cache

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  i <- m[[1,2]]()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- m[[2,1]]()
  i <- solve(data)
  m[[2,2]](i)
  i
}
