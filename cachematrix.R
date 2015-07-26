## This R script contains 2 key functions
## makeCacheMatrix: returns a list object that store the inverse of a matrix
## cacheSolve: returns the inverse of a matrix. The inverse could come from
##          cache or calcuated.

## create a few example matrices for testing
x1 <- 1:4
mx1 <- matrix(x, 2)
solve(mx1)

x2 <- 2:5
mx2 <- matrix(x, 2)
solve(mx2)

x3 <- 3:6
mx3 <- matrix(x, 2)
solve(mx3)

x4 <- 1:4
mx4 <- matrix(x, 2)
solve(mx4)



makeCacheMatrix <- function(x = matrix()) {
    #initialize the inverse
    inverse <- NULL
    
    #set the matrix x to be used to calculate the inverse
    #everytime x is reset, the inverse is reset to null
    set <- function(y) {
        #if every element of y is the same as every elemnt of x, 
        #there is no need to initialize or set the inverse to null.
        if(!( dim(x)==dim(y) && all(x==y) )) {
            x <<- y
            inverse <<- NULL
        } else {
            message("all elements in the matrix are the same")
        }
    }
    
    #return the input matrix
    get <- function() x
    
    #cache the inverse
    setinverse <- function(inv) inverse <<- inv
    
    #return the inverse
    getinverse <- function() inverse
    
    #return the list object
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


## create test object
cm <- makeCacheMatrix(mx1)
cm$get()
## test to see if the function compares 2 matrices correctly
cm$set(mx4)
## calculate the inverse of x
cacheSolve(cm)
