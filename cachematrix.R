## As matrix inversion is a very computation intensive process for large size matrices,
## we can eliminate the need for using looping codes for calculating the inverse
## of the matrix, by caching the value of the inverse matrix and recalling this 
## cached inverse matrix instead of re-computing the inversion. This condition is
## only possible if there is no change to the initial matrix. The below is the 
## code for the inversion process. It consists of two main functions.
## 1. makeCacheMatrix
## 2. cacheSolve

## Creates a Matrix object that can cache the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  # Creating a makeCacheMatrix function will consist of
  # four functions encapsulated in a list
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
  
  # Initially set to NULL
  # Changes when the user sets the value
  invr<-NULL
  set<-function(y){
    x<<-y
    invr<<-NULL
  }
  
  get<-function()x
  setinverse<-function(inverse) invr<<-inverse
  getinverse<-function() invr
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function is used to compute the inverse of the orginal matrix.
## If the inverse for the matrix is already calculated(i.e., the matrix is unchanged),  
## then it returns the cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr<-x$getinverse()
  if(!is.null(invr)){
    message("getting cached data :")
    return (invr)
    
  }
  
  data<-x$get()
  invr<-solve(data)
  x$setinverse(invr)
  invr
}
## Sample Run
##x<-cbind(c(1,2),c(3,4))
##m<-makeCacheMatrix(x)
##m$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##cacheSolve(m)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##cacheSolve(m)
##getting cached data :
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
