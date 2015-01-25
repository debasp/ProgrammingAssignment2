## Idiea is to store inverse matrix calculed in function cacheSolve 
## in its calling environment show that, the calculated value will 
## not get lost on function environment refresh.

## This function build a cache matrix object and return a list of functions 
## which store and retrives inverse value from the calling environment

makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        getinverse <- function() inverseM
        setinverse <- function (inverse) inverseM <<- inverse
        list(set=set, get=get, getinverse=getinverse, setinverse=setinverse)
}


## This is the method where cache matrix is used store/retrieve inverse value of Matrix.
## First call, Inverse is calculated using solve function and then stored in the cacheMatrix 
## Subsequent call the inverse value will be return from the enclosing environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("Getting data from cache")
                return( inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
}
