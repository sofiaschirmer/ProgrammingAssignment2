## first we have to create a matrix function 'x'

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x    #this function return the value of a named object
  setInverse <- function(inverse) j <<- inverse   #this function compute the inverse of the cumulative distribution function of an continuous variable
  getInverse <- function() j  
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}  #create a list with different types of objects ('set, get, setInverse, getInverse')

## now we create a function for the inverse of the matrix 'x'
## we have to create an syxtax "if () is...else" for to statement with a logical condition
cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  } #####is.null is an function that indicates whether a data object is of the data type NULL, so
 ######The function returns TRUE in case of a NULL object and FALSE in case that the data object is not NULL 
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
} 
#now we create a new object with this function to solve the problem

#after this i saved the file with the name "ProgrammingAssignment2/cachematrix.R
#now we can apply this fuction to transforme an inverse of matrix

source("ProgrammingAssignment2/cachematrix.R")
#create an matrix with this funcion, 1:4, with nrow = 2 and ncol = 2
# set the matrix value
my_matrix1 <- makeCacheMatrix(matrix(1:4, 2, 2))
#now check that we stored it correctly
my_matrix1$get()
      [,1] [,2]
[1,]    1    3
[2,]    2    4
#its ok :)
#now we want the inverse of this matrix
my_matrix1$getInverse()
NULL
#there's a problem! 
#we can solve this use the functon cacheSolve 
cacheSolve(my_matrix1)
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
my_matrix1$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
