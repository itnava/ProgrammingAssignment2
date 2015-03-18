## Inverting a matrix using lexical scoping


## Solves and stores the matrix inversion. 
## Building a cache, useful for memory intensive tasks. 
## The function takes a matrix as the input

makeCacheMatrix <- function(x = matrix()) {
       inverse <- NULL            #define a NULL variable, it can be assigned an appropriate value later
       set <- function(y){        #y is the initial matrix, to be inverted
               x <<- y            #assign the matrix to x
               inverse <<- NULL   #define NULL variable, acts as placeholder
       }
       get <- function() x        # Assign value to get
       setsolve <- function(solve) m <<- solve  # the matrix is inverted using the solve function, assigned to variable m
       getsolve <- function() m   # returns the inverted matrix
       list(set = set, get = get, # list of output for function that can be called outside the function environment
              setsolve = setsolve,
              getsolve = getsolve)
}

#Solves and returns the inverted matrix

cacheSolve <- function(x = matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() # x$getsolve contains the inverted matrix in makeCacheMatrix
        #if m is not NULL, returns the cached matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #if m is NULL as it would be the first time a matrix is entered,
        #the matrix is assigned to data and inverted
        else{
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
        # inverted matrix is returned
                m
        }
}
