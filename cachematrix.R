## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

x1 <- 1:4
mx1 <- matrix(x, 2)
solve(mx1)

x2 <- 2:5
mx2 <- matrix(x, 2)
solve(mx2)

x3 <- 3:6
mx3 <- matrix(x, 2)
solve(mx3)


makeCacheMatrix <- function(x = matrix()) {
    ## testing git
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
cm <- makeCacheMatrix(mx1)
cm$get()

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

cacheSolve(cm)
