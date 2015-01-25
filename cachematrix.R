## This function returns a list of functions. It takes a matrix as input and provides functions that allow to get and set the matrix
## It also allows to store the inverse matrix within its environment and to return this inverse matrix 
## If the matrix is modified through the set function then also the inverse is set to NULL.

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
        setinverse <- function(inverse) i <<- inverse ## define a function that sets i to parameter inverse
        
        ## the getinverse variable is used to return the value of the variable i
        getinverse <- function() i ## define a function that returns i
        
        ## the function makeCacheMatrix returns a list with the functions defined within it as elements
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function returns the inverse of a matrix that is stored within an environment (cache) defined by the list passed to it as input parameter.
## Its input parameter is a list that needs to contain the functions getinverse, get and setinverse. Such a list is created by passing a matrix to the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'     
        i <- x$getinverse() ##call getinverse function and assign to i
        
        ## check if the inverse matrix is not NULL, i.e. there is something stored for the inverse matrix
        if(!is.null(i)) { ## if i is not NULL
                message("getting cached data") 
                return(i)
        }
        
        ## if nothing can be found for the inverse matrix, it needs to be calculated here
        data <- x$get() ## get matrix x
        i <- solve(data, ...) ## calculate inverse on matrix data
        
        ## and it is stored in the cache
        x$setinverse(i) ## set i to inverse
        i        
}
