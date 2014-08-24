## The following 2 functions serve to cache the inverse of a matrix 
## 

## makeCacheMatrix function takes a matrix as input and returns a list object 
## with named elements which will be used when calling cacheSolve function 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
	  
        get <- function() { x }
       
        setinverse <- function(solve) { i <<- solve }
       
        getinverse <- function() { i } 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function checks if the inverse matrix has been stored and if yes returns it
## If not it computes it, stores it and returns it

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i   ## Return a matrix that is the inverse of 'x'
}
