## The first function (makeCacheMatrix) creates a matrix object that can cache and retrieve its inverse.
## The second function (cacheSolve) computes the inverse of the values returned by the first function.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## This function creates a matrix object that can store its inverse. This defines the argument with the default mode of matrix().
  m <- NULL ## This initializes the object m as NULL, allowing to hold the value of the matrix inverse.
  set <- function(y) { ## Defines the set function to assign the new value of the matrix in the parent environment.
    x <<- y
    m <<- NULL ## If there is a new matrix this will reset m to NULL.
  }
  get <- function() x ## Defines the get function, telling it to return the values of the matrix argument.
  setinverse <- function(inverse) m <<- inverse ## Assigns the value of m in parent environment.
  getinverse <- function() m ## Retrieves the m value when called.
  list(set = set,get=get,setinverse=setinverse,getinverse=getinverse) ## This allows you to refer to the functions with the $ operator.
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) { 
  m <- x$getinverse()
  if(!is.null(m)) { ## If the inverse has already been calculated, this returns the values from the cache without having to compute it again.
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## If the inverse hasn't been calculated, this does that.
  m <- solve(data, ...)
  x$setinverse(m) ## It stores the results from the computation in the cache with the setinverse function.
  m
}