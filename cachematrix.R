# The following are a pair of functions that cache the inverse of a matrix.
#   
# The first one, called `makeCacheMatrix`, creates a special "matrix" object
# that can cache its inverse.
# The second one, called `cacheSolve`, computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

# In these functions the matrix supplied is always supposed to be invertible.

## The first function, `makeCacheMatrixr` creates a special "vector", which is really a list containing a function to
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                    m <- NULL
                    set <- function(y) {
                                        x <<- y
                                        m <<- NULL
                                       }
                    get <- function() x
                    setsolve <- function(solve) m <<- solve
                    getsolve <- function() m
                    list(set = set, get = get,
                         setsolve = setsolve,
                         getsolve = getsolve)
                    }


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setsolve`
# function.

# The inverse of the matrix 'x' is calculated by the function 'solve'

cacheSolve <- function(x, ...) {
                              m <- x$getsolve()
                              if(!is.null(m)) {
                                              message("getting cached data")
                                              return(m)
                                               }
                              data <- x$get()
                              m <- solve(data, ...)
                              x$setsolve(m)
                              m
                              }
