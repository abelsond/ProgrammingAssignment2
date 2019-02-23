## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  #default is accepting the null matrix
        x_inv <- NULL #set matrix inverse to be null
        #browser()
        setmatrix <- function(y) {
                x <<- y ##If long vector of data already made in global environment, then don't reallocate.  Just create a pointer to global number. 
                #trick is, y is a free variable, meaning it hasn't been allocated.  So, set will remain a function ready to run, but not yet done. 
                x_inv <<- NULL  ##if calling set, then 
        }
        getmatrix <- function() x  #Returns x - a handle to the raw data
        setinverse <- function(inverse) x_inv <<- inverse #sets m by the MEAN - defined in global environment with pointer.  Don't need to carry value
        getinverse <- function() x_inv  #m is set to be null when you first call makeCacheMatrix if inverse has not been allocated. 
        list(set = setmatrix, get = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(matrix_functions, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## 'x' is a list of functions that caches a pointer to a matrix in x$get()
        #browser()
        matrix_inv <- matrix_functions$getinverse() 
        if(!is.null(matrix_inv)) {
                #It getmean has a value -> return it
                message("getting cached data")
                return(matrix_inv)  # and leaves the function
        }
        #Otherwise, will need to calculate the mean.  But, still, get a handle to the data rather than making anew, if one exists
        data <- matrix_functions$get() #If already have the data, do not replicated it in memory, just use the one that exists
        matrix_inv <- solve(data, ...)  # WHAT DOES THE ... do??? Allows you to add more data to the mean
        matrix_functions$setinverse(matrix_inv) #Stores the mean so that you don't need to calculate again.  The x is the makeVector list of functions you previously made
        matrix_inv
}


