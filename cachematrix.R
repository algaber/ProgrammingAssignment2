## The following functions computes and caches the inverse matrix. 
##The "makeCacheMatrix" function creates a special "matrix" object that cache 
##its inverse. 
##The "cacheSolve" function computes the inverse of the special "matrix" returned by
##makeCacheMatrix previous. If the inverse has already been calculated (and the matrix 
##has not changed), then the cacheSolve retrieve the inverse from the cache.


##The makeCacheMatrix function has several functions and variables.

## m: it is the matrix which It is null or has the value of the inverse of the
##matrix x if recalculated by cacheSolve. Not really recalculates the
##inverse, as it seeks bind in parent environments (in this case a function). 
##This is achieved because the variable is assigned by <<-. The operator <<- causes a 
##search to made through parent environments for an existing definition of the variable
##being assigned.

##The functions defined in makeCacheMatrix the comment from the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
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

##The cacheSolve function has two possibilities. In essence that gives value m
##with x$getinverse(), from the list generated in makeCacheMatrix. If I was
##found, then m is not null, get inverse, "m", of parent environment. And it isn't
##calculated, so it you reduce computation time.

##The other possibility is that m is zero because it has not been previously found 
##then calculated using the solve () function, but taking the varible from x$get().

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


##This is an example which able to reduce the computation time compared to calculation
##with loops. We used lexical scoping.

