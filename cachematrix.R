## This collection of functions will compute the inverse of a matrix and allow
#users to retrieve this value repeatedly by caching the value of the inverse assuming
#the underlying matrix has not changed.  This is useful in speeding up processing
#by avoiding recomputing the inverse after it has been computed once

## makeCacheMatrix will aggregate a set of functions used to set and get the data 
#as well as set and get the inverse of a matrix into a single function
#the results
makeCacheMatrix <- function(matrix = matrix()) 
{
    #create a variable in the "makeCacheMatrix" environment to store/presereve the value
    #of the inverse of the matrix
    inverse.matrix <- NULL
    
    #store the matrix data in the matrix variable and set the inverse.matrix varaible
    #to null
    set <- function(y) 
    {
        matrix <<- y
        inverse.matrix <<- NULL
    }
    
    #return the original matrix data
    get <- function() 
    {
        matrix
    }
    
    #set the null inverse.matrix variable to the computed inverse
    setinverse <- function(inverse) 
    {
        inverse.matrix <<- inverse
    }
    
    #return the inverse.matrix that is already computed
    getinverse <- function() 
    {
        inverse.matrix
    }
    
    #return a list of functions
    list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will solve for the inverse of a matrix.  it will first
#check to see if the matrix inverse has already been stored in the function 
#object that was passed as an argument.  if found, it will return the cached value
#if  not, it will compute, cache, and return the inverse matrix
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}

