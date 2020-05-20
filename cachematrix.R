## These functions cache potentially computational heavy matrixes by caching the inverse of the matrix. 

## Below creates a list of the special matrix. First it set and gets the matrix then sets and gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL ## i to be used in the later code
                set <- function(y) {
                        x <<- y    ## assign y to x in the parent environment. Sets the matrix
                        i <<- NULL  
                }
                get <- function() x ## gets the matrix
                setinverse <- function(inverse) i <<- inverse ##set i as the inverse
                getinverse <- function() i ## get the inverse
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## assign functions in a list in the parent environment, naming each element
}


## Below returns the inverse of the orginal special matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse() ## gets inverse of input
        if(!is.null(i)) {
                message("getting cached data")
                return(i) ##checks the whether result is null and returns valid cached inverse to parent environment
                }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i ## if !is.null is false, the vector will calculate the inverse
}
