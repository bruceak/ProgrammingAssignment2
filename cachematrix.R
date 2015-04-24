makeCacheMatrix <- function(x){  ##function containing list of functions capable of 
  m <- NULL                      ##creating a matrix and displaying its inverse
  set <- function(y, r, c) {  ##  set is a function that takes numeric inputs for y 
    x <<- matrix(y, r, c)     ##  (matrix contents) r (number of rows in matrix) 
    m <<- NULL                ##  and c (number columns in matrix)
}
get <- function() x   ##  get grabs matrix x assuming it has already been created
setinverse <- function(inverse) m <<- inverse  ## manually set the inverse matrix, m
getinverse <- function() m   ##  getinverse will display the inverse of x assuming 
list(set = set, get = get,   ##  x exists and inverse has been solved and cached
     setinverse = setinverse,
     getinverse = getinverse)}


cacheSolve <- function(x, ...) {  ##input for cache solve is the makeCacheMatrix function
  m <- x$getinverse()   ##reference to the getinverse function within makeCacheMatrix
  if(!is.null(m)) {   ##check to see if inverse already exists
    message("getting cached data")
    return(m)  ##returns cached data if inverse has been calculated
  }
  data <- x$get()  ##grabs matrix from get function in makeCacheMatrix
  m <- solve(data, ...)  ##computes inverse of given matrix
  x$setinverse(m)  ##sets the inverse within the makeCacheMatrix function
  m  ##displays computed inverse matrix
}
