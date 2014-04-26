## The two functions in this R program calculate the inverse
## of an invertable square matrix, without having to repeat the calculation
## if it was already made.

## makeCacheMatrix allows to set the inverse of a matrix x in memory (setinverse)
## to retrieve it (getinverse), as well as to set the memory to null (set)
## and retrieve x (get).

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
  
}


## cacheSolve searches if by any chance the inverse of the matrix has already
## been calculated and put in memory, and in this case, retrieves it.
## If not, it calculates the inverse of the matrix.
## cacheSolve calculates the inverse of x by taking as an input makeCacheMatrix(x).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m  
  
  
}
