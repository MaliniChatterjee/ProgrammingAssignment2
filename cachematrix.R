## makeCachematrix is a function which will perform the following tasks in four steps
## 1.Set the matrix Step 
## 2.Get the matrix Step 
## 3. set the inverse Step setinv
## 4. get the inverse Step getinv

makeCacheMatrix <- function(x = matrix()) {
inv=NULL

set = function(y) {
  x <<- y
  inv <<- y
}
get = function() x
setinv = function(inverse) inv<<-inverse
getinv= function() inv
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve function first checks if the its cached or not.
## If cached, we return the data along with a message 
## Else, we set the inverse and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)) {
    message ("DATA CACHED AND FETCHED")
    return(inv)
  }
  calc.data=x$get()
  inv=solve(calc.data,...)
  x$setinv(inv)
  return(inv)
}

