
################NOTES###############################

#Roger's example of a make function.   It outputs a list OF functions (a vector of functions).  
#The set function is a free variable.  It draws on y, a free variable. 
#It sets x to y in the local environment if y can be found in the global environment. So, y isn't a free variable, UNLESS not previously defined.
#If y not previously defined, then x will remain defined by the input number only. 
#Thus benefit of SET FUN is that it does not create a replicate value for x.  It allows you to set one without passing whole data. 
#However, get function will give you a handle to this data
#Similarly, mean is defined elsewhere and does not get recalculated or reproduced, it is simply found globally and set inside this function
#This set makes a pointer from within the function the the global version.
#Lastly getemean returns the mean defined locally with a HANDLE to the global version.

#These are get(x) which returns the 
makeVector <- function(x = numeric()) {  #x can be a long numeric vector.  It is the raw data.
        m <- NULL
        browser()
        set <- function(y) {
                x <<- y ##If long vector of data already made in global environment, then don't reallocate.  Just create a pointer to global number. 
                #trick is, y is a free variable, meaning it hasn't been allocated.  So, set will remain a function ready to run, but not yet done. 
                m <<- NULL  ##if calling set, then 
        }
        get <- function() x  #Returns x - a handle to the raw data
        setmean <- function(mean) m <<- mean #sets m by the MEAN - defined in global environment with pointer.  Don't need to carry value
        getmean <- function() m  #m is set to be null when you first call makeVector if mean has not been allocated. 
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
#At end of call you have 4 ready defined LOCAL functions.  R knows what they are.  None have evaluated or allocated any data, even though you passed them area. 



#This is roger's example function to calculate something from the vector of functions you have created
cachemean <- function(x, ...) { #x is the makeVector with values set, get, setmean, getmean
        browser()
        m <- x$getmean() 
        if(!is.null(m)) {
                #It getmean has a value -> return it
                message("getting cached data")
                return(m)  # and leaves the function
        }
        #Otherwise, will need to calculate the mean.  But, still, get a handle to the data rather than making anew, if one exists
        data <- x$get() #If already have the data, do not replicated it in memory, just use the one that exists
        m <- mean(data, ...)  # WHAT DOES THE ... do??? Allows you to add more data to the mean
        x$setmean(m) #Stores the mean so that you don't need to calculate again.  The x is the makeVector list of functions you previously made
        m
}