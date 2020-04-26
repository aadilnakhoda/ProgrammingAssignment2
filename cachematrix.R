## In this example we introduce the <<- operator which can be used to 
## assign a value to an object in an environment that is different 
## from the current environment. Below are two functions that are used 
## to create a special object that stores a matrix and cache's its inverse.

## The first function, makeCachematrix creates a special 'matrix', 
## which can cache its inverse 



makeCacheMatrix <- function(x = matrix()) {
m <- NULL  
set <- function(y){        ## Set the matrix
        x <<- y
        m <<- NULL
}
get <- function() x        ## Get the matrix
        
        setInverse <- function(inverse) m <<- inverse ## Set the inverse of the matrix
        getInverse <- function() m ## Get the inverse of the matrix
        list(set = set, get=get,   ## Creating a list of the above functions 
             setInverse=setInverse,
             getInverse=getInverse)
        }


## The following function calculates the inverse of the special 
## "matrix" created with the above function. However, it first 
## checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setInverse function.

## Computing the inverse of a square matrix can be done with the solve function. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        
m<-x$getInverse() ## Return a matrix that is the inverse of 'x'
if(!is.null(m)){  ## Return cached data if inverse already calculated
        message("getting cached data")
        return(m)
}
        data<- x$get() ## Getting the matrix
        m <- solve(data, ...) ## Solving for the inverse of the matrix
        x$setInverse(m) ## Setting the inverse of the matrix
        m
        }
