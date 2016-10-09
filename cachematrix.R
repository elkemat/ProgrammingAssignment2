## The functions allow to cache the inverse of a matrix, to avoid recalculating 
## of potentially time-consuming operations.

## makeCacheMatrix creates an object of the type makeCacheMatrix().
## When called with a matrix as argument, it returns a set of defined functions
## within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                ## y is assigned to x object from the parent environment 
                x <<- y
                ## value NULL is assigned to the i object from the parent
                ## environment, to clear the cached value when x is reset
                i <<- NULL
        }
        
        ## as x is not defined in get(), get() makes use of lexical scoping and 
        ## retrieves x from the parent environment makeCacheMatrix()
        get <- function() x
        
        ## xInverse is assigned to i object from the parent environment
        setInverse <- function(xInverse) i <<- xInverse
        
        ## as i is not defined in getInverse(), getInverse() makes use of 
        ## lexical scoping and retrieves i from the parent environment 
        ## makeCacheMatrix()
        getInverse <- function() i
        
        ## Each function is assigned to an element within a list().
        ## Elements are named to allow using the $ operator to access the 
        ## function
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve takes an object of the type makeCacheMatrix() as argument,
## meaning four functions and two data objects (a matrix x and its inverse).
## Because makeCacheMatrix() returns objects of type list() also any other 
## objects defined in the environment of the original function (i.e. x and i)
## can be accessed by cacheSolve().
## Called for the first time cacheSolve() calculates and caches the inverse 
## from of matrix x. Called a second time it simply fetches the inverse from the 
## makeCacheMatrix() object.

cacheSolve <- function(x, ...) {
        ## Accesses the value of i stored in the makeCacheMatrix() object.
        ## $ can be used as extract operator because the functions are defined
        ## with names in makeCacheMatrix().
        i <- x$getInverse()
        
        ## If i is not null, i.e. the inverse was already calculated before, 
        ## a message is printed and the value of i is returned.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## If i is null the function continues by accessing x stored in the 
        ## makeCacheMatrix() object and calculating the inverse matrix.
        data <- x$get()
        i <- solve(data, ...)
        
        ## i in the makeCacheMatrix() object is set to the calculated i and 
        ## returned.
        x$setInverse(i)
        i
}
