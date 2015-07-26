##These two functions work together to speed up the process of caching the inverse 
##of a matrix, rather than repeatedly calculate it.


## makeCacheMatrix is a function which creates a special 'matrix' object which can 
## cache its inverse and pass the object a list of functions (via the list object, 
## containing functions (set, get, setinv and getinv).

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        
        x <<- y  ## sub vector x with y (input) in the makeCacheMatrix function
        i <<- NULL ## restore value of i to NULL
    }
    get <- function() x ## get the vector x stored in the main function
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    ## list is used to assign functions to object created by makeCacheMatrix
    list(set = set, 
         get = get,
         setinv = setinv, ## set the inverse (doesn't calculate it) 
         getinv = getinv)
}


## cacheSolve computes the inverse of the matrix object created in makeCacheMatrix, 
## first checking if the inverse has been calculated, if it hasn't, cacheSolve will
## calculate it, if it has it will get the value from teh cache and set it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i = x$getinv() ## verify the value of i stored with getinv exists
    ## check value of i is not NULL
    if (is.null(i)){
        message("checking data cache")
        return(i)  ## end function and return i if value of i exists and is not NULL
    }
    
    ## if the value of i doesn't exist, calculate and set it with: 
    mtrx_data <- x$get()
    i <- inverse(mtrx_data, ...)
    x$setinv(i)
    ## return inverse 
    i
    
    
}
