## This program illustrates the use of caching in R programming. I am caching the inverse of a matrix
## There are two functions makeCacheMatrix() and cacheSolve(). Calculating the inverse of matrices may take long so there is need to cache resluts  

## This function creates a matrix. This matrix i will find the inverse of using cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
   i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)i<<-inverse
  getinverse<-function()i
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}


## This function calculates the inverse of the matrix created by makeCacheMatrix(). 
##If the matrix does not change, this function should return the cached result

cacheSolve <- function(x, ...) {
	i<-x$getinverse()
  if(!is.null(i)){
    message("GETTING CACHED DATA...")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
 ## Return a matrix that is the inverse of 'x'
}
