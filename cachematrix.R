## Put comments here that give an overall description of what your
## functions do

#The first function, makeCachematrix creates a special "matrix", 
#which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(m = matrix()) {

  m_inverse<-NULL
  #set the value of the matrix
  set<-function(x){
    m <<- x
  m_inverse <<- NULL
}
#get the value of the matrix
get <- function() 
  return(m)
#set the value of the inverse
setinverse <- function(solve) m_inverse <<- solve
#get the value of the inverse
getinverse <- function() 
  return(m_inverse)
return(list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse))
}


#The following function calculates the inverse of the special "matrix" 
## created with the `makeCacheMatrix` function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 

cacheSolve <- function(m, ...) {
  m_inverse <- m$getinverse()
  ## Calculate the inverse of special "matrix" x, assign it to m but first check if m already exists, 
  ## if it does, get inverse of x from cache
  if(!is.null(m_inverse)) {
    message("Getting cached data...")
    return(m_inverse)
  }
  ## Else return a matrix that is the inverse of 'x', assign it to m
  data <- m$get()
  m_inverse <- solve(data, ...)
  
  # Set the value of the inverse in the cache via the setinverse function
  m$setinverse(m_inverse)
  return(m_inverse)
}



