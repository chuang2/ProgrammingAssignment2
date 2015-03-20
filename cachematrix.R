#makeCacheMatrix function takes a matrix as input argument, and converts it to an object that supports caching.

makeCacheMatrix <- function(x = matrix()) { #Default argument is empty matrix
  i <- NULL #Default value for inverse i is set to NULL
  set <- function(y) {
    x <<- y #Searches through parent environments for existing definitions of x and m
    i <<- NULL 
  }
  get <- function() x #A function that returns x
  setinv <- function(inverse) i <<- inverse #A function that sets the inverse from outside
  getinv <- function() m #A function that returns m
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv
  ) #A list containing all the prior functions
} #We must assign the result of makeCacheMatrix(x) to another variable, then use that variable as the argument for cacheSolve

#cacheSolve function takes the object we received from makeCacheMatrix, and either returns the cached value of i (if it exists), or calculates and sets the inverse of the matrix in i (if i does not exist).

cacheSolve <- function(x, ...) { #x is a makeVector object from above
  i <- x$getinv() #Remember this from the above function makeCacheMatrix
  #Sets the LOCAL m$cachemean to the m/mean from makeCacheMatrix object
  if(!is.null(i)) { #Checks if i is NOT a null value
    message("getting cached data")
    return(i) #Returns the value of i
  }
  data <- x$get() #If the is.null condition above is fulfilled, obtains the vector
  #x using the get function and assigns it to data
  i <- solve(data, ...) #Creates the value of m using the solve function on data
  x$setinv(i) #Sets the inverse of x in cache, using the setinv function and i as arguments
  i #Prints value of i
}
