## Inversion of the matrix usually a costly operation, so those two function
## are wrote to compute matrix inversion with caching and decrease computation
## time for the same input.

## Creates a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'. Result caching applied.
## !! Assume that the input matrix is invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

#############################
## Code usage sample ########
############################# 
## > source("cachematrix.R")

############################# Matrix creation
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > x
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

############################# Cached matrix creation
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > m$getinverse()
## NULL

############################# First calculation
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > m$getinverse()
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

############################# Cached calculation
## > cacheSolve(m)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
