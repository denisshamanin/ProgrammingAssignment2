## This function takes a matrix as an argument 
## and returns a list of four functions that allow
## to either set or print the matrix (set, get) or 
## set or print the inverse matrix (setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {

        ## initiate i, where i will be the inverse matrix
        i <- NULL 
        
        ## if a new matrix is being set, its inverse has not been calculated 
        ## yet and is therefore set to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        } 
        
        ## returns the initial matrix x
        get <- function() x 
        
        ## allows to set an inverse matrix
        ## is actually pretty dangerous as it's possible to call from outside 
        ## x$setinverse(<some random matrix>) and
        ## change the stored inverse of a matrix manually
        setinverse <- function(inverse) i <<- inverse
        
        ## returns an inverse of a matrix
        getinverse <- function() i
        
        ## returns a list of four functions to work with the given matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes a list of four functions y as defined in 
## makeCacheMatrix and calculates an inverse matrix i for a given
## stored matrix if that has not been done yet
## or returns the already calculated cached version 

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## stores the inverse matrix to i
        i <- y$getinverse() 
        
        ## if i is not NULL, then it has been calculated already
        ## and we are returning the cached version and exit cacheSolve
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if i has not been calculated yet, we calculate the inverse matrix
        ## using solve()
        i <- solve(y$get())
        ## we set the inverse matrix to the result of our calculation
        y$setinverse(i)
        ## print out the inverse matrix
        i
}
