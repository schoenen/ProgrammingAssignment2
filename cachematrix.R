## The aim of these functions is to reduce computacional costs while programming. Getting the mean of a vector may not have
## higher computacionals costs, but resolve an equation with many variables (getting the inverse of a matrix) is indeed.

## the first function (makeCacheMatrix), as it did in the vector example, it creates an special matrix
## which creates a list that set the value of a matrix, gets the value, sets the inverse and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## this function is the one that actually calculate the inverse of the matrix. It looks in the cache if the inverse 
## of this matrix has been calculated, with a "if". If it has been calculated it returns the inverse and sends a message.
## if it hasn't, it calculates it and stores it into cache.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
