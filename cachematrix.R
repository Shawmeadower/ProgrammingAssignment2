#Week 3 Programming Assignment
#Created by Erin Shaw-Meadow

## Put comments here that give an overall description of what your functions do ----
#the following functions allow the user to compute the inverse of a matrix and cache the result so that the next time the user wants the inverse of that same matrix, R doesn't have to recompute the inverse and instead can retrieve the cached inverse matrix.

## Write a short comment describing this function ----
#this first function creates a list of functions that will allow the user to set and get matrix objects to be used in the cacheSolve function. The inclusion of the <<- operator allows the makeCacheMatrix to store a given matrix (provided as an argument of this function) in memory outside of the current environment. 

makeCacheMatrix <- function(x = matrix()){  #argument of this function should be an invertable matrix
        i <- NULL   #creates an empty object i that will later be used to store the inverse matrix
        set <- function(y){   #this is the first of four functions in makeCacheMatrix()  
                x <<- y       #caches the value of x in memory
                i <<- NULL    #erases any previously cached values of i
        }
        get <- function() x   #prints the matrix x (argument of makeCacheMatrix)
        set.inverse <- function(solve) i <<- solve   #caches output of solve() to object i
        get.inverse <- function() i  #retrieves object i
        list(set = set, get = get,   #creates a list of the four above functions
             set.inverse = set.inverse, 
             get.inverse = get.inverse)
}


## Write a short comment describing this function ----
#this second function requires the use of the makeCacheMatrix as its argument and either calculates the inverse matrix of the invertable matrix defined in makeCacheMatrix or retrieves the cached value if the inverse was previously calculated. Providing a matrix that hasn't been passed through the makeCacheMatrix function will cause an error. 

cacheSolve <- function(x, ...) {
        i <- x$get.inverse()  #indexes from the list of functions in the makeCacheMatrix to search for cached value i
        if(!is.null(i)) {     #if i has already been cached for a given matrix,
                message("getting cached data") #the following message will appear in the console,
                return(i)     #and the inverse of the input matrix will be returned
        } 
        data <- x$get()       #if there is no cached value of i, this command retrieves the input matrix
        i <- solve(data, ...) #and passes it as an argument in the solve function to calculate the inverse
        x$set.inverse(i)      #the result object i is cached using the third function in makeCacheMatrix
        i                     #and the value of i is printed to the console
}



