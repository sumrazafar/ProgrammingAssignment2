## The functions calculate the inverse of the matrix using the R method : Solve
## Assumption: Input is always a square matrix

## These functions work together to check if the inverse of a matrix is present in cache or not? If it is not present in cache, 
## only then the inverse of the matrix is calculated (via SOLVE) otherwise, it just fetches the result from cache! The result is added to cache everytime
## a unique/new matrix is given to the function. Therefore, the cached results increase as we keep on calcuting inverse of the matrices.

# makeCacheMatrix function returns a list containing function definitions to
# 1. Set the Matrix
# 2. Get the Matrix
# 3. Set the Inverse 
# 4. Get the Inverse

# this function is called/used by CacheSolve!

makeCacheMatrix <- function(x = matrix()) {
	i = NULL
	
	# Set the Matrix in environment
	set = function(y){
		# " <<- " is used to assign value in environment different from current one
		x <<- y
		i <<- NULL	
	}
	
	# Get the Matrix
	get = function() x
	
	# Set the Inverse in environment
	setInverse = function(inverse) i <<- inverse
	
	# Get the Inverse
	getInverse = function() i
	
	# return the list of functions defined
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This function returns the inverse of the matrix that is given as an input

cacheSolve <- function(x, ...) {
        
		# get the inverse for matrix x 
		result = x$getInverse()
		
		# check if the inverse is already calculated? 
		# it would be previously calculated if inverse is not null and contains a value
		if(!is.null(result)){
			# found in cache - no need to calculate - 
			message("getting the cached data")
			# return the value from cache
			return (result)
			
		}
		
		# not found in cache - To be calculated
		# get the matrix
		mat = x$get()
		# calculate the inverse
		result = solve(mat)
		
		# set the value of inverse in cache
		x$setInverse(result)
		
		#return the inverse
		return(result)
}
