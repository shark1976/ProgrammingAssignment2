## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL ##set i to NULL
        ## the set function is used to assign a new matrix to the variable x. In the same time the inverse matrix is set to NULL. 
        set <- function(y) { ## define a function that assigns y to x and NULL to i
                x <<- y
                i <<- NULL
        }
        ## the get function is used to return the matrix x
        get <- function() x  ## define a function that returns x
        ## the setinverse function is used to store the value of the input parameter inverse in the variable i 
        setinverse <- function(inverse) i <<- inverse ## define a function that sets i to parameter mean
        ## the getinverse variable is used to return the value of the variable i
        getinverse <- function() i ## define a function that returns i
        ## the function makeCacheMatrix returns a list with the functions defined within it as elements
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse() ##call getinverse function and assign to i
        if(!is.null(i)) { ## if i is not NULL
                message("getting cached data") 
                return(i)
        }
        data <- x$get() ## get matrix x
        i <- solve(data, ...) ## calculate inverse on matrix data
        x$setinverse(i) ## set i to mean
        i        
}
