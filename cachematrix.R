## The program makes use of the lexical scoping rules of the R Language to cache values
## for R objects to reduce computations and optimize the program

##  The function creates the template for the special Matrix that will be passed on the
##  cache solve function

makeCacheMatrix <- function(x = matrix()) {
     
        i <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) i <<- inverse
        
        getInverse <- function() i
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function checks if the value of the inverse has been evaluated: In case previous
## evaluation is available it skips reevaluating and uses that value otherwise computes
## and assign to the matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getInverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data, ...)
        
        x$setInverse(i)
        
        i

}

## Thank you for evaluating. Looking forward to feedback and suggestions