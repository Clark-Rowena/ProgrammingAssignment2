## A matrix is provided for analysis.  The objective is to determine the inverse of the matrix with minimal calculation time.
## To reduce processing time two functions are provided so that
## the calculation of the inverse of the input data matrix is only performed if the calculation has
## not previously been performed.
## makeCacheMatrix - initialises an object, stores the input data matrix and defines functions get (read) and set (write)
## cacheSolve - takes the object defined by makeCacheMatrix.  If the inverse matrix in the object is empty, the inverse calculation
## has not been performed.  The calculation proceeds and output is stored in the global object.
##  If the inverse matrix in the object contains values, this information is retrieved skipping the calculation step.

makeCacheMatrix <- function(x = matrix()) {
      ## input data to the function will be global variable x, a matrix
      
      ## variable inversematrix of type matrix is created and initialised to dimension 0 x 0
      ## empty matrix = dimension 0 x 0 = inverse of input data matrix not calculated
      inversematrix <- matrix(nrow=0,ncol=0)
      
      ## define the functions for the resulting object
      
      ## new matrix data is provided, store matrix in global variable and reset the 
      ## previously calculated inverse matrix stored in global variable
      set <- function(newmatrixdata) {
            x <<- newmatrixdata
            inversematrix <<- matrix(nrow=0,ncol=0)      
      }
      ## associates input matrix x with name "get"
      get <- function() x
      ## future cacheSolve function combines this solve command with a matrix resulting in the inverted matrix
      ## being placed in the setinversematrix global variable
      setinversematrix <- function(solve) inversematrix<<-solve  
      ## stores the initial inversematrix
      getinversematrix <- function() inversematrix
      
      ## create the object using functions defined above
      list(set=set, get=get,setinversematrix=setinversematrix, getinversematrix=getinversematrix)
}


## this function is provided with an object input produced by function makeCacheMatrix.
## the object defines the function of get and set and defines the input data matrix
## if the inverse of the input data matrix has previously been calculated and stored in the object, the information is retrieved
## if the inverse of the input data matrix has not been calculated, the function calculates and stores the matrix inverse
cacheSolve <- function(x, ...) {
      ## the input variable x is an object created by the function makeCacheMatrix
      
      ## from object x retrieve the inverse matrix      
      inversematrix <- x$getinversematrix()
      
      ## if dimensions of the matrix does not equal 0x0 then retrieve calculated inverse matrix
      ## and leave function after outputting the inverse matrix to the screen
      if(nrow(inversematrix)!=0 & ncol(inversematrix)!=0){
            print("getting cached data")
            return(inversematrix)
      }
      
      ## inverse of matrix has not previously occurred so proceed to calculate and store data
      ## obtain the matrix data from the object
      data <-x$get()
      ## calculate inverse matrix using solve function
      inversematrix <-solve(data,...)
      ## store the inverse matrix in the data object
      x$setinversematrix(inversematrix)
      ## return the calculated inverse matrix to the cacheSolve function
      inversematrix
}
