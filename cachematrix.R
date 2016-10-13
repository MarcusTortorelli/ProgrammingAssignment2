## Exemplo: 
## m <- makeCacheMatrix(matrix(c(2,0, 0,2), nrow = 2, ncol = 2))
## cacheSolve(m)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inversa) inv <<- inversa
   getinv <- function() inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   if (!is.null(inv)){
      message("getting cached data")
      return(inv)
   }
   dados <- x$get()
   inv <- solve(dados, ...)
   x$setinv(inv)
   inv
}
