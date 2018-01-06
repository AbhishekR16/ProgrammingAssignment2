#these two functions works in collaboration to return an inverse of a matrix from the cache memory and 
#if there is no value available in the cache for the required matrix then it calculates the inverse and 
#the save it in the cache for future retrieval  

#makeCacheMatrix function recieves a matrix from the user and 
#creates a list of functions to facilitate the setter and getter for the data and its inverse and 
#returns the list to the parent environment making all these funtions accesible to the parent environment
makeCacheMatrix<- function(x = matrix())
{
  inv<-NULL
  set<- function(y)
   {
     x <<- y
     inv <<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) inv <<- inverse
  getinverse<- function()inv
  list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}
# cachesolve function takes an object of makeCacheMatrix type and 
#uses its functions to check and retrieve the cache data and 
#if not avaialbe then calculates the inverse and set the value in the cache for future retrieval 
cachesolve<- function(z,...)
{
  inv<- z$getinverse()
  if(!is.null(inv))
    {
      message("returning cached data")
      return(inv)
    }
  data = z$get()
  inv<- solve(data)
  z$setinverse(inv)
  inv
}
samplematrix<- matrix(c(1,5,6,7,8,4,5,6,8), 3,3)
trail01<- makeCacheMatrix(samplematrix)
cachesolve(trial01)
