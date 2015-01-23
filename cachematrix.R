## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#creates a square  matrix that can have its invrse cashed.
makeCacheMatrix <- function(x = matrix()) {
#https://class.coursera.org/rprog-010/forum/thread?thread_id=364

    z <- NULL # sets the value of z to NULL as per example https://class.coursera.org/rprog-010/human_grading/view/courses/973491/assessments/3/submissions
    set <- function(y) { #set the value of the matrix
      x <<- y # caches the inputted matrix so that we can see if it changed
      z <<- NULL #sets the value of matrex inverse back to NULL
    }
	# get the value of the matrix
    get <- function() x
	#set the value of inverse
    set_inverse <- function(solve) z <<- solve
	# get the value of the inverse
    get_inverse <- function() z
	# create a list store the four new functions
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
 
}


## Write a short comment describing this function

# Write a short comment describing this function
#function works out the inverse of the square matrix sent by makeCacheMatrix 
#https://class.coursera.org/rprog-010/forum/thread?thread_id=726

cacheSolve <- function(x, ...) {
 # Return a matrix that is the inverse of 'x'
  
  # get the inverse of the matrix        
  z <- x$get_inverse()
  
  # check matrix is not null 
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  # if not null i.e. its populated: get the inverse of the matrix   
  data <- x$get()
  z <- solve(data, ...)
  # set the inverse of the matrox 
  x$set_inverse(z)
  z
}
