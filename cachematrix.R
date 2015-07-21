
## makeCacheMatrix
## Creates a special object to cache the matrix.
## The function receives a matrix as parameter
makeCacheMatrix <- function(x = matrix()) {
  
  # The makeCacheMatrix object comprises 4 functions in a list (a.k.a object properties)
  # 1. set the matrix #set#
  # 2. get the matrix #get#
  # 3. set the inverse of the matrix #setinverse#
  # 4. get the inverse of the matrix #getinverse#
  
  # Initialization
  inv <- NULL
  
  # Set function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get function
  get <- function() x
  
  # Set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Encapsulate into a list (a.k.a object properties)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
  

}

## cacheSolve
## This function is used to compute the inverse of the matrix and cache the result
## Using cacheSolve again on the same special matrix, you get the cached result, avoiding recalculation.  
## It also returns a message that shows if the result are from cache or not.

cacheSolve <- function( x, ...){
    ## Return a matrix that is the inverse of 'x'
    
    ## First, try get the inverse matrix from cache
    InverseMatrix <- x$getinverse()
    
    ## Check if the inverse was in cache-
    ## If it is null then cache is empty and we must calculate the inverse
    if (is.null(InverseMatrix)) {
      
      message('Empty cache: Inverse Matrix Calculated.')
      
      # Get the matrix
      data <- x$get()
      
      # Calculate Inverse
      InverseMatrix <- solve(data, ...)
      
      # Once calculated, stores it in the cache
      x$setinverse(InverseMatrix)
      
    } else {
      
      # Already stored in the cache
      message('Inverse Matrix from Cache.')
    
      }
    
    InverseMatrix
  }

## Example of use

## mymatrix <- matrix(1:4, nrow=2, ncol=2)
## mycache <- makeCacheMatrix(mymatrix)
## myinverse <- cacheSolve(mycache)
## print(myinverse)
