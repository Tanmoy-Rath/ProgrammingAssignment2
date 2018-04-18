## makeCacheMatrix() : Creates a matrix data type and returns a list containing its inbuilt functions
##	It has 4 inbuilt functions setm() , getm() , setinv(), getinv()
##		setm() : resets the matrix to new values
##		getm() : gets the matrix values
##		setinv() : stores the inverse in memory
##		getinv() : gets the inverse from the memory cache

makeCacheMatrix <- function(x = matrix()) {
        
        ## First, the inverse is set to NULL
        inv <- NULL
        
        
        ## setm() : resets the matrix to new values
        setm <- function(matx) {
                x <<- matx
                inv <<- NULL
        }
        
        
        ## getm() : gets the matrix values
        getm <- function() x
        
        
        ## setinv() : stores the inverse in memory
        setinv <- function(INVS) inv <<- INVS
        
        
        ## getinv() : gets the inverse from the memory cache
        getinv <- function() inv
        
        
        ## The 4 functions are returned as list elements
        list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
}




## cacheSolve() works as follows
##      (1)  It checks if inverse exists, and returns if inverse exists
##      (2)  If inverse doesnot exist, then it calculates and stores the inverse in memory

cacheSolve <- function(x, ...) {
        
        ## First check whether inverse exists or not
        ## Return the inverse if it exists
        ## Otherwise proceed further to calculation
        
        IN <- x$getinv()
        if(!is.null(IN)) {
                message("getting cached data")
                return(IN)
        }
        
        
        
        ## Get the matrix by calling getm()
        ## Call the solve() function to calculate inverse
        ## Set the inverse to memory to be cached later
        ## Print an appropriate message.
        
        DATA <- x$getm()
        IN <- solve(DATA, ...)
        x$setinv(IN)
        message("Calculation finished. Inverse can now be cached from memory.")
        
        
        
        ## Returns the inverse
        IN
}