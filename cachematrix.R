## makeCacheMatrix creates a special "matrix", which is really a list containing a function to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { # input x will be a matrix
  inv <- NULL            # inv will be our "inverse" and it's reset to NULL every time makeCacheMatrix() is called
 
  set <- function(y) {   # this fuciton will be used by cacheSolve() to get values for x or for inv (inverse) and for setting the inverse
    x <<- y
    inv <<- NULL
  }

  get <- function() x    # this function will return the value of the original matrix
 
  setinv <- function(solve) inv <<- solve  # will be called by cacheSolve() during the first access and will store the value
  
  getinv <- function() inv                 # will return the cached value to cacheSovle() on subsequent accesses
  
  list(set = set, get = get,               # This is a list of the internal functions, and will be accesses each time makeCacheMatrix() is called
       setinv = setinv,
       getinv = getinv)
}

## calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinv() function.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix()
  inv <- x$getinv()              # accesses the object 'x' and gets the value of the inverse
  
  if(!is.null(inv)) {              # if inverse was already cached (not NULL), 
    message("getting cached data") # send a message and return inverse value and end the function
    return(inv)
  }
  
  data <- x$get()                  # if x$getinv() returned NULL
  inv <- solve(data, ...)          # and if inv was NULL then calculate the inverse
  x$setinv(inv)                    # store the calculated inverse value in x 
  inv                              # return the inverse to the code that called this function
}
