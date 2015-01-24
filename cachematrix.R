## makeCacheMatrix is a function that stores the values of a matrix
## and allows to store the value of the inverse of that matrix 
## without calculation. 
## cacheSolve is a function that gets the inverse matrix from 
## chache if it exists, or calculates and  stores it if it doesn't
## exist.

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(mat = numeric()) {  # we need a numeric matrix as argument
        inv <- NULL  # inverese matrix has NULL value in the beginning 
        set <- function(y){  # function 'set' allows to input a new matrix
                mat <<- y    # setting the new matrix
                inv <<- NULL # erasing the old values of inverse matrix if they exist 
        }
        get <- function() mat  # functon 'get' shows the values of the matrix
        setinv <- function(inverse) inv <<- inverse  # function 'setinv' introduce values for inverse matrix
                                                     # without calculation
        getinv <- function() inv  # function 'getinv' shows the values of the inverse matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv) # list of functions related to the matrix
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(myMatrix, ...) {  # we need the values and objects stored with makeCacheMatrix
        inv <- myMatrix$getinv() # assigning the values stored (if they exist) of the inverse matrix 
        if (!is.null(inv)) { # if there's an inverse matrix in cache... return it
                message("inverse is from cache")
                return(inv)
        }
        # else...
        data <- myMatrix$get()  # get the original matrix from myMatrix
        inv <- solve(data)  # calculate its inverse
        myMatrix$setinv(inv)  # set the inverse matrix in myMatrix
        inv  # and show the values of the inverse matrix
}

# Too much comments? I hope it wasn't been a problem, specially because I'm strugling with English.
# Have a nice day!