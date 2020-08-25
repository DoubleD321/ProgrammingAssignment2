## rows 2-48 are the example provided as reference for the wk3 assignment
## introduce the <<- operator which can be used to assign a
## value to an object in an environment that is different 
## from the current environment. Below are two functions 
## that are used to create a special object that stores a 
## numeric vector and cache's its mean.
## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to

## 1 set the value of the vector
## 2 get the value of the vector
## 3 set the value of the mean
## 4 get the value of the mean

##makeVector <- function(x = numeric()) {
##        m <- NULL
##        set <- function(y) {
##                x <<- y
##                m <<- NULL
##        }
##        get <- function() x
##        setmean <- function(mean) m <<- mean
##        getmean <- function() m
##        list(set = set, get = get,
##             setmean = setmean,
##             getmean = getmean)
##}


## The following function calculates the mean of the special
## "vector" created with the above function. However, it first
## checks to see if the mean has already been calculated. If 
## so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the 
## value of the mean in the cache via the setmean function.

## cachemean <- function(x, ...) {
##        m <- x$getmean()
##        if(!is.null(m)) {
##                message("getting cached data")
##                return(m)
##        }
##        data <- x$get()
##        m <- mean(data, ...)
##        x$setmean(m)
##        m
##}

## Put comments here that give an overall description of what your
## Below are two functions 
## that are used to create an inverse of a matrix and cache the inverse 


## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "inverse matrix", 
## which is really a list containing function to

## 1 sets the matrix
## 2 gets the matrix
## 3 sets the inverse matrix
## 4 gets the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y){
                x<<- y
                inv <<- NULL
        }
        get<- function() (x)
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() {
                inv <- ginv(x)  ## gets the inverse of x
                inv
                } 
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## once you get the inverse you can recall this information/matrix from cache
## via the cacheSolve function
## The cacheSolve function calculates the inverse of the matrix 
## created with the above function. However, it first
## checks to see if the inverse matrix has already been calculated. If 
## so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the data and sets the 
## value of the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data") ## if getInverse is in cache,
                ## the getting cached data message will appear". This avoids
                ## doing the calculation for inverse of the matrix 
                return(inv)
        }
        matdata <- x$get()
        inv <- solve(matdata, ...) ## found via google search, the standard 
        ## R function for geting inverse "solve"
        x$setInverse(inv)
        inv
}

## results/ouput below
##> testmatrix<- makeCacheMatrix(matrix(1:16,4,4))
##> testmatrix$get()
##[,1] [,2] [,3] [,4]
##[1,]    1    5    9   13
##[2,]    2    6   10   14
##[3,]    3    7   11   15
##[4,]    4    8   12   16
##> testmatrix$getInverse()
##[,1]    [,2]  [,3]    [,4]
##[1,] -0.285 -0.1075  0.07  0.2475
##[2,] -0.145 -0.0525  0.04  0.1325
##[3,] -0.005  0.0025  0.01  0.0175
##[4,]  0.135  0.0575 -0.02 -0.0975
##> cacheSolve((testmatrix))
##getting cached data
##[,1]    [,2]  [,3]    [,4]
##[1,] -0.285 -0.1075  0.07  0.2475
##[2,] -0.145 -0.0525  0.04  0.1325
##[3,] -0.005  0.0025  0.01  0.0175
##[4,]  0.135  0.0575 -0.02 -0.0975
## just to test that the inverse of the inverse gets back to the original
## matrix I set f as the inverse matrix and took the inverse of f
##> f<- testmatrix$getInverse()
##> ginv(f)
##[,1] [,2] [,3] [,4]
##[1,]    1    5    9   13
##[2,]    2    6   10   14
##[3,]    3    7   11   15
##[4,]    4    8   12   16
##> 
