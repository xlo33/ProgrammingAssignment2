## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #matrix than cache the inverse
        n <- NULL
        set <- function(y){
                x <<- y
                n <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) n <<- inverse
        getInverse <- function() n 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #inverse of matrix returned by makeCacheMatrix
        n <- x$getInverse()
        if(!is.null(n)){
                message("getting cached data")
                return(n)
        }
        mat <- x$get()
        n <- solve(mat,...)
        x$setInverse(n)
        n
}