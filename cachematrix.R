##############################################################################################
# This script contains 2 functions: makeCahceMatrix and cacheSolve.
# 
# makeCacheMatrix takes a square matrix (x) as an argument and creates a list of 4 functions, 
# one to set the value of a matrix, one to get the value of a matrix, one to set the inverse 
# of a matrix (in the cache!) and one to get the inverse of a matrix.
#
# cacheSolve is used to solve for the inverse of the matrix (x) inputed into makeCacheMatrix.
# It checks to see if the inverse of x is stored in the cache and if not it computes and 
# stores the inverse in the cache if it is not already present.
##############################################################################################

makeCacheMatrix <- function(x = matrix()) {
        if (nrow(x) != ncol(x)) {                        #Check to see if he matrix is square
                message("Matrix must be square!")
        }
        else {
                x_inv <<- matrix()                    
                x_inv <<- NULL                           #Create empty matrix to hold inverse
                setmat <- function(y = matrix()) {       #Create function to set matrix value and clear existing inverse value
                        x <<- y
                        x_inv <<- NULL
                }
                getmat <- function() {x}                 #Create function to recall the value of the matrix x
                setinv <- function(inv) {x_inv <<- inv}  #Create function to set the inverse of the matrix x
                getinv <- function() {x_inv}             #Create function to get the value of x_inv
                list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)  #Return function list
        }
}

cacheSolve <- function(x = matrix(), ...) {
       x_inv <- x$getinv()                               #Set x_inv to the value of the inverse of x
       if(!is.null(x_inv)) {                             #Check to see if x_inv is already in the cache
               message("Relax, I'm Getting cached data for you. It won't take long.")   #If x_inv is in the cache tell the user to chill out while the cached data is returned.
               return(x_inv)                             #Return the inverse of x
       }
       else {                                            #If x_inv is not in the cache, let's compute it!
            data <- x$getmat()                           #Create a local variable (matrix) called data that is set to the value of the matrix to be inverted
            x_inv <- solve(data)                         #Compute the value of the inverse
            x$setinv(x_inv)                              #Set the value of the inverse in the cache
            return(x_inv)                                #Return the value of the inverse of x
       }
}

