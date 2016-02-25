## Function to calculate inverse of a matrix incorporating caching 
## Inverse of a matrix can be calculated using solve(A) command
## For eg:- 
## >A<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## >solve(A)#Gives the inverse of A
## >solve(A)#The computation is repeated again to find inverse 
## This second computation for calculating inverse could be avoided by storing the 
## value of the inverse found during the first time
## So the aim of this assignment is to create a special structure which will 
## store the value of the matrix and its inverse and use it while computing 
## inverse


## Function makeCacheMatrix
## Creates a structure with the matrix,its inverse and functions to access them
## getMatrix - returns the matrix
## setInverse - used to set inverse from the calling environment
## getInverse - returns the inverse of the matrix
## when called the first time makeCacheMatrix sets the inverse to NULL
makeCacheMatrix <- function(x = matrix()) 
{
  inv<-NULL
  getMatrix<-function() x
  setInverse<-function(invx) inv<<-invx
  getInverse<-function() inv
  list(getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse) 
 }


## Function to find the inverse of the matrix
## Called using the special structure created using makeCacheMatrix
## First use the getInverse to check if the inverse already exists
## If yes return the inverse
## Else calculate the inverse using solve(x) by first retrieving 
## matrix using getMatrix
## Store the inverse using setInverse

cacheSolve <- function(x, ...) 
{
  invx <- x$getInverse()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  xtemp <- x$getMatrix()
  invx <- solve(xtemp)
  x$setInverse(invx)
  invx
}

## Sample Output
## >m<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## >cm<-makeCacheMatrix(m)
## >cm$getInverse()
##  NULL
## >cacheSolve(cm)
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
## >cm$getInverse()
## [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
## >cacheSolve(cm)
## getting cached data
##[,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
