## Take a matrix(), calculate its inverse
## Store the inverse in a "cache" variable
## Futher calls to calculate the inverse looks up
## the cache variable and prints (instead of re-calculating 
## inverse)

## Sample output given at the end of the file


## Two functions to make this possible:
##  makeCacheMatrix() :   takes input matrix, 
##                        calculates inverse
##                        defines the following functions:
##                        set(), get(), setinverse() and
##                        getinverse()
##
##                        Basically takes a snapshot of the environment


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cacheSolve():
## Takes a makeCacheMatrix object
## If the inverse is set earlier, look up the cache and print
## Othersize calculate inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



# -------------------
# > source("cachematrix.R")
# > aMatrix <- makeCacheMatrix(matrix(c(4, 7, 2, 6), 2, 2))
# > cacheSolve(aMatrix)
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > cacheSolve(aMatrix)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > cacheSolve(aMatrix)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > cacheSolve(aMatrix)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > 
