##  The program makes use of the lexical scoping rules of the R Language to cache values
##  for R objects to reduce computations and optimize the program. This is demostrated by
##  computing the inverse of a matrix using two functions explained below: 

##  The function creates the template for the special Matrix that will be passed on the
##  cache solve function. It provides for basic functions to set the matrix and the inverse and 
##  consequently get the matrix value and the inverse if already computed.
##  The key here is the use of '<<-' operator which is used to assign a value to an object in an 
##  environment that is different from the current environment. In this case it the objects will
##  be defined in the global environment which will be accessible to other functions.

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


##  Since this function would have access to the object created using the above function, it will
##  checks if the value of the inverse has been evaluated using getInverse: In case previous
##  evaluation is available it skips reevaluating and uses that value. otherwise computes
##  and assign to the matrix object using setInverse.

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

##  The above implementation has been checked and verified in the R Studio using this the test:
##  myMatrix <- makeCacheMatrix(matrix(1:4, byrow = TRUE, nrow = 2))
##  cacheSolve (myMatrix)
##  cacheSolve (myMatrix)

##  Which correctly evaluates to 
##  > cacheSolve (myMatrix)
##  [,1] [,2]
##  [1,] -2.0  1.0
##  [2,]  1.5 -0.5
##  > cacheSolve (myMatrix)
##  getting cached data
##  [,1] [,2]
##  [1,] -2.0  1.0
##  [2,]  1.5 -0.5

##  Thank you for evaluating my work. Looking forward to feedback and suggestions