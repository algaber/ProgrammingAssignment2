## The following functions compute and cache the inverse matrix. It able to reduce 
## the computation time compared to calculation with loops.
#La primera funcion crea una matrix especial para almacenar en cache
#Segunda funcion coge la matrix anterior y checks si ha sido calculada
#si es asi coge el resultado de cache sino la calcula y pone el resuldo en cache
## Write a short comment describing this function
## 
## The operators <<- and ->> are normally only used in functions, and cause a search 
## to made through parent environments for an existing definition of the variable
## being assigned. If such a variable is found (and its binding is not locked) 
## then its value is redefined

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
# y debe ser la matrix en cache?      
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
         setinverse <- function(inverse)
                 m <<- inverse
         getinverse <- function()
                 m
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
        

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m.data <- x$get()
        m <- solve(m.data, ...)
        x$setinverse(m)
        m
        
}
