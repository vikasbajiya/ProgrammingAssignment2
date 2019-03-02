
        ## Put comments here that give an overall description of what your
        ## functions do
        ## The two functions defined in this file implement a cache mechanism to maintain in memory
        ## the inverse of a matrix (solve function)
        ## This is accomplished by using the super assigment operator "<<-" (see documentation)
        ##
        
        ## Write a short comment describing this function
        ## A file called "TestCases1.txt" is included in the git repository providing examples on how to test
        ## these functions
        
makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix(x)
        ## arguments: x of class 'matrix'
        ##
        ## this functions initialize a vector with 4 functions:
        ##      set: sets the x matrix on the enclosing environment using the variable 'mat'
        ##           additionally sets the inverse of x to null indicatin that has not been calculated
        ##      get: allows to retrieve the original x matrix
        ##      setinverse: sets the inverse of x on the enclosing environment using the variable 'inv'
        ##      getinverse: allows to retieve the inverse matrix of x which is stored in 'inv' (could be NULL)
        
        ## Special Consideration: because the matrix x can only be set using the provided 'set' function
        ##                        and the inverse is set to NULL, 
        ##                        there is no need to verify if the original matrix has changed
        
        makeCacheMatrix <- function(x = matrix()) {
                
                ## we only want to accept matrix objects as x
                if(!is.matrix(x)) {
                        message("only matrix object allowed")
                        ## if not a matrix we return NA
                        return(NA) 
                }
                # because only square matrix has an inverse we only allow those
                if(!identical(ncol(x),nrow(x))) {
                        message("only square matrix allowed")
                        ## if not square matrix we return NA
                        return(NA) 
                }
                
                inv <- NULL
                mat <- x
                set <- function(y) {
                        mat <<- y
                        inv <<- NULL
                }
                get <- function() mat
                setinverse <- function(solve) inv <<- solve
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
        
        
        ## Write a short comment describing this function
        ## cachesolve(x, ...)
        ## arguments: x of class list, which is the list created with makeCacheMatrix
        ##            any other class will generate an error
        
        cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                ## Return a matrix that is the inverse of 'x'
                inv <- x$getinverse()
                ## if the inverse has already been calculated return the cache value of the inverse
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                ## if the inverse has not been calculated 
                ## then it gets calculated, set in the cache and returned
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
                }        