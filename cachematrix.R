## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. Below are two funtions that are used to create a special object that stores 
## a matrix and caches the inverse of it.

## The first function "makeCacheMatrix" is used to create a special object, which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inv<-NULL
   set<-function(y){
      x<<-y
      inv<<-NULL}

   get<-function() x
   setinv<-function(inverse) inv<<-inverse
   getinv<-function() inv
   list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The following function calculates the inverse of the special object created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix data and 
## sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
   inv<-x$getinv()
   if(!is.null(inv)){
         message("getting cached data")
         return(inv)
   }
   data<-x$get()
   inv<-solve(data,...)
   x$setinv(inv)
   inv
}
