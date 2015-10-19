# --------------- makeCacheMatrix - programming assignment 2 ------------------

## Create a matrix object that contains a matrix 
## and the cache for it's inverted form

makeCacheMatrix = function(x = matrix()) {
   
   # property to store our inverse matrix inside
   minv <- NULL
   
   # standard getters and setters
   # set a new matrix
   set <- function(y) {
      
      # TODO: not implemented, check: has to be square matrix, otherwise you cannot inverse it
      
      x <<- y       # store the matrix in the property x for the matrix
      m <<- NULL    # nullify the inverse matrix if we set a new matricx
   }
   
   get <- function() x
   setinverse <- function(m) minv <<- m
   getinverse <- function() minv
   
   # returns a list of all availalable methods of the object,
   # useful for introspection
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)  
}

## engine to crete an inverted matrix
## x has to be of form makeCacheMatrix, not just any matrix
## returns the inverted matrix

cacheSolve <- function(x, ...) {
   
   # retrieve a cache dinverse matrix
   minv <- x$getinverse()
   
   # test whether there is a cached matrix, if so we will take that one and return
   if(!is.null(minv)) {
      
      # tell the user...
      message("getting cached data")
      
      return(minv)
      # ends the function here
   }
   
   # else branch: there is no cached matrix, we will continu
   
   # retrieve the matrix
   matrix <- x$get()
   
   # TODO: check whether matrix is square; not implemented in this assignment
   # but will cause trouble in practice
   
   # calculate the inverse
   minv <- solve(matrix, ...)
   
   # store the cached matrix
   x$setinverse(minv)
   
   # return the cached matrix
   minv
}

# --------------- extra unit tests ------------------

## run all unit tests
unitTestAll = function() {
   unittest01()
   unittest02()
}


#  unit test 01: input and output prediction
#
#  input      inverse        unity
#
#   1  2     -1/3   2/3      1  0
#         x               =
#   2  1      2/3  -1/3      0  1
#
unittest01 = function() {
   
   # input
   mcells = c(1,2, 2,1)
   X= matrix(mcells, 2, 2, TRUE)
   
   # expected inverse
   expinvcells = c(-1/3, 2/3, 2/3, -1/3)
   Z = matrix(expinvcells, 2, 2, TRUE)   
   
   unitTester(X, Z,  "test 01")
}

#  unit test 02: input and output prediction
#
#  input      inverse        unity
#
#   1     x  1           =   1
#

unittest02 = function() {
   mcells = c(1)
   X= matrix(mcells, 1, 1, TRUE)
   
   expinvcells = c(1)
   Z = matrix(expinvcells, 1, 1, TRUE)   
   
   unitTester(X, Z, "test 02")
   
}   


unitTester = function(X, Z, desc) {   
   
   mymatrix = makeCacheMatrix()
   mymatrix$set(X)
   cacheSolve(mymatrix)
   Y = mymatrix$getinverse()
   
   matricesAreIdenticalTester(X, Z, Y, desc)
}


matricesAreIdenticalTester = function(X, Z, Y, testdesc) {
   
   if (is.matrix(Y) && is.matrix(Z) && dim(Y) == dim(Z) && all(Y == Z)) {
      print(paste("OK", testdesc))
   } else {
      print(paste("FAILED", testdesc))
      print("result:")
      print(Y)
      print("expected:")
      print(Z)
   }   
}
