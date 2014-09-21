#Based on the outline from the vector examples
#Given the makeCacheMatrix function, assume x is an invertable matrix
#note!! this only works if you assign makeCacheVector to a variable and then
#run cacheSolve on that variable, otherwise you reinitiaize m every time
#which is probably obvious, but took me several hours to figure out


makeCacheMatrix <- function(x = matrix()) {

#initialize m and create set function y with x (matix) and m (inverse matrix)
  m <- (NULL)
  y <- (NULL) 
  set <- function(y){
  x <<- (y)
  m <<- (NULL)
}

#create get function 
get <- function() x

#create setinvmatrix function to solve for inverse of matrix
setinvmatrix <- function(solve) m <<- solve

#create getinvmatrix function to return cached matrix 
getinvmatrix <- function() m

#set up list with set,get, setinvmatrix and getinvmatrix functions
list(set=set, get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)

}

#given cacheSolve function with arguments passed from makeCacheMatrix
cacheSolve <- function(x = matrix(), ...) {

#if m exists then return m as the inverse of the matrix 
    m <- x$getinvmatrix() 
    print(m)
    if(!is.null(m)){
      message("Returning cached data...")
      return(m)
    }
    
#if m is null then calculate the inverse of the matrix and cache the value
    data <- x$get()
    m <- solve(data)
    x$setinvmatrix(m)
    m

}
