## These two functions work together to cache a matrix and its inverse

## Accepts a matrix and returns a list of functions for use in
## cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize the cached variable in this contructor
        csh_inv_mtrx <- NULL
        
        ## set the matrix
        csh_mtrx <- x
        
        ## set the values of the matrix and return. Don't compute solve here
        set <- function (v, r, c){
                ## if we're calling set, the constructor is not going to
                ## clear the cached variable or set a matrix for us
                csh_inv_mtrx <<- NULL
                csh_mtrx <<- x
        }
        
        ## return the matrix created
        get <- function() csh_mtrx
                
        ## function for setting the cashed inverted value
        set_inv_mtrx <- function(inv_mtrx) csh_inv_mtrx <<- inv_mtrx
        
        ## function for getting the cashed inverted value
        get_inv_mtrx <- function() csh_inv_mtrx
        
        ## return
        list(set = set,
             get = get,
             set_inv_mtrx = set_inv_mtrx,
             get_inv_mtrx = get_inv_mtrx)
}

## Accepts an instance of the makeCacheMatrix() object and returns either the
## cached version of the inverse, or computes/stores a new inverse if one 
## doesn't already exist in cache
cacheSolve <- function(x, ...){
        ## attempt to get the value of the cached inverted matrix     
        invmtrx <- x$get_inv_mtrx()
        
        if (!is.null(invmtrx)) {
                message("getting cached data")
                return(invmtrx)
        }
        
        data <- x$get()
        invmtrx <- solve(data)
        x$set_inv_mtrx(invmtrx)
        invmtrx
}

