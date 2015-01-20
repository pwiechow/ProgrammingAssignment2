## ----------------------------------------------------------------------------
## Functions in this file are used to calculate and cache a matrix inversion. 
## ----------------------------------------------------------------------------


## ----------------------------------------------------------------------------
## The function returns a "wrapper" object for the given matrix
## (argument x of the function) and its inversion. The "wrapper" is in fact
## a list with setters and getters for a matrix and its inversion.
## ----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        ## Variable for storing cached matrix inversion
        inversion <- NULL
        
        ## Sets the matrix. When invoked a matrix
        ## cached inversion will be set to NULL as well
        set <- function(y) {
                x <<- y
                inversion <<- NULL
        }
        
        ## Gets a matrix
        get <- function() {
                x
        }          

        ## Sets a matrix inversion
        setInversion <- function(i) {
                inversion <<- i
        }
        
        ## Gets a matrix inversion
        getInversion <- function() {
                inversion
        }      
        
        ## The result of the function that contains all supported operations
        list(set = set, get = get,
             setInversion = setInversion, getInversion = getInversion)
}


## ----------------------------------------------------------------------------
## The function returns an inversion of a matrix based on x argument.
## The argument is an object created by makeCacheMatrix function.
## ----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversion <- x$getInversion()
        
        if (!is.null(inversion)) {
                ## Found cached inversion => return it
                message("getting cached inversion of a matrix")
                return(inversion)
        }
        
        ## Not found cached inversion => get original matrix
        cachedMatrix <- x$get()
        ## Calculate inversion
        inversion <- solve(cachedMatrix, ...)
        ## Cache inversion
        x$setInversion(inversion)
        
        ## Return inversion
        inversion        
}
