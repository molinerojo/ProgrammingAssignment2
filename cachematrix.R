## JOM - A pair of functions that cache the inverse of a matrix.

## JOM - This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


## JOM - This function computes the inverse of the special "matrix" returned by 
## JOM - makeCacheMatrix above. If the inverse has already been calculated 
## JOM - (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        
}

## example:
## x <- rbind(c(1,2,3,4),c(2,3,4,1),c(3,4,1,2),c(4,1,2,3))
## m = makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]   [,2]   [,3]   [,4]
## [1,] -0.225  0.025  0.025  0.275
## [2,]  0.025  0.025  0.275 -0.225
## [3,]  0.025  0.275 -0.225  0.025
## [4,]  0.275 -0.225  0.025  0.025
## 
## > cacheSolve(m)
## getting cached data
## [,1]   [,2]   [,3]   [,4]
## [1,] -0.225  0.025  0.025  0.275
## [2,]  0.025  0.025  0.275 -0.225
## [3,]  0.025  0.275 -0.225  0.025
## [4,]  0.275 -0.225  0.025  0.025
## > 
