## This script has 2 functions makeCashMatrix and cacheSolve
## call makeCashMatrix first passing an invertible matrix to it (assumes it's invertible)
## then call cashSolve passing the matrix just created, and it will display the inverted matrix
## usage exampe:
## a<-matrix(c(1,2,3,4), nrow=2, ncol=2)
## v<-makeCacheMatrix(a)
##cacheSolve(v)
##note: if the inerted matrix has already been cache it will return it from the cache
## and display 'getting cached inverted matrix'


#this method takes an inertible matrix (without checking for invertibility)
#and adds the functions to the special vector
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL # sets m to NULL iniatially
  
  set<-function(y){ 
    x<<- y
    m<<- NULL  
  } #defines set function, x to new matrix y, m (inverted matrix) to null
  get<-function() x #fucntion get to return matrix x
  setinverse<-function(solve) m <<-solve # sets the inverted matrix to m
  getinverse<-function() m # returns the inverted matrix, m
  list(set=set,
        get=get,
        setinverse = setinverse,
        getinverse = getinverse) # returns the special vector with the functions
}

## returns the inverted matrix, checks the cache, if it's available returns from there
## if not, calculates it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached inverted matrix")
    return(m)  
  }  # checking the chache, return if available
 #if not in cache, calc inverted matrix
  data<-x$get() #get matrix and set data to it
  m<-solve(data)  #invert data
  x$setinverse(m) #set inverted matrix value
  print(m) #print inverted matrix (for fun)
}