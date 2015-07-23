## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeCacheMatrix(x)
# The function makeCacheMatrix takes a single argument x, which should be
# a square matrix. The function returns a list, which contains four elements, 
# each of which are functions:
# 1. set: Running this allows you to change the stored matrix, which clears
#         cached inverse.
# 2. get: Outputs the stored matrix.
# 3. setinverse: Used to cache the inverse of the matrix in the list.
# 4. getinverse: Outputs the cached matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
    # Define variable i to store inverse matrix
    i <- NULL
    # Check if matrix is square
    squaretest <- testsquarematrix(x)
    # If the matrix isn't square, store NULL value
    if (squaretest == FALSE) {
        message("Not a square matrix, storing NULL value")
        x <- NULL
    }
    # Define function to set/change the stored matrix
    set <- function(y) {
        # Run function to test if matrix is square
        squaretest <- testsquarematrix(y)
        # If matrix is square, store it, otherwise, store NULL.
        if (squaretest == TRUE) {
            x <<- y
            inverse <<- NULL
        } else {
            message("Not a square matrix, storing NULL value")
            x <<- NULL
            inverse <<- NULL
        }
    }
    # Define function to output the stored matrix
    get <- function() x
    # Define function to store the matrix inversion
    setinverse <- function(inverse) i <<- inverse
    # Define function to output the stored matrix inversion
    getinverse <- function() i
    # Create a list object that stores the set, get, setinverse, and getinverse
    # functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Function cacheSolve(x, ...)
# The function cacheSolve takes two arguments, x and "...". The variable x
# should be a list object created by the function makeCacheMatrix. The "..."
# are additional arguments that you might wish to pass to the solve function,
# which performs the matrix inversion. Upon execution, this function checks
# whether the inverse matrix is cached. If it is, it returns it. If not, it
# calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Retrieve the inverted matrix if it exists, or NULL value if it
    # doesn't exist.
    i <- x$getinverse()
    # Check if variable i is NULL. If it is not, return the cached data.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # If the variable i is NULL, then compute the inverted matrix.
    # Grab the stored matrix.
    data <- x$get()
    # Compute the matrix inverse
    if (!is.null(data)) {
        i <- solve(data, ...)
        # Store the matrix inverse
        x$setinverse(i)
        # Output the matrix inverse.
        i
    } else {
        message("There is no stored matrix, so I cannot take the inverse.")
        x$setinverse(NULL)
    }
}

## Function testsquarematrix(x)
# Function used to test whether a matrix is square. Takes a matrix as input.
# Outputs TRUE if the matrix is square, outputs FALSE if matrix is not square.
testsquarematrix <- function(x) {
    # Grab dimensions of matrix
    tmp_dim <- dim(x)
    # Check if the matrix is square.
    if (tmp_dim[1] == tmp_dim[2]) {
        TRUE
    } else {
        FALSE
    }
}