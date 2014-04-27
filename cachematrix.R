## This program chaches the inverse of a matrix
## Assumption: The matrix will be invertible

## This function will create a special matrix object which is actually a list
## containing function to set the value of the matrix,get the value of the 
## matrix, set the value of inverse of a matrix, get the value of inverse
## of a matrix

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) {
                x <<- y
                m <<- NULL
         }
         get <- function() x
         setInverse <- function(Inverse) m <<- Inverse
         getInverse <- function() m
         list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
         

}

## This function calculates inverse of a matrix. It first checks if the inverse
## has already been calculated. If yes, then it returns the cached inverse. 
## Else, it will calculate the inverse and sets the value of the inverse in 
## cache.

cacheSolve <- function(x, ...) {
       m <- x$getInverse()  ## checks if the inverse has already been calculated
       if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m         ## Return a matrix that is the inverse of 'x'
}
