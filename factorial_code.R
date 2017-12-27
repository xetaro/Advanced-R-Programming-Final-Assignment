# The overall purpose of this assessment is to evaluate the ability to apply functional programming 
# and object oriented programming concepts in R. 
# These part of assignment compares different functional programming techniques and benchmarks their performance. 

# The objective of Part 1 is to write a function that computes the factorial of an integer
# greater than or equal to 0. Recall that the factorial of a number n is n * (n-1) * (n - 2) * . * 1. 
# The factorial of 0 is defined to be 1.

# Load packages:

library(purrr)
library(microbenchmark)

# For this Part we will need to write four different versions of the Factorial function:

# 1. Factorial_loop: a version that computes the factorial of an integer using looping (such as a for loop):

factorial_loop<-function(n) {
  
  factorial = 1 #set factorial variable to 1
  
  # check is the number is negative, positive or zero
  if(n < 0) {
    print("factorial does not exist for negative numbers")
  } else if(n == 0) {
    print("The factorial of 0 is 1")
  } else {
    for(i in 1:n) #for loop to expand n up to 1
    {
      factorial = factorial * i 
    }
    print(factorial)
  }
}

# 2. Factorial_reduce: a version that computes the factorial using the reduce() function in the purrr package. 
#    Alternatively, you can use the Reduce() function in the base package.

factorial_reduce <-function(n) {
  
  factorial = 1 #set factorial variable to 1
  
  # check is the number is negative, positive or zero
  if(n < 0) {
    print("factorial does not exist for negative numbers")
  } else if(n == 0) {
    print("The factorial of 0 is 1")
  } else {
    reduce(1:n, function(x, y){
      x * y
    })
  }
}


# 3. Factorial_func: a version that uses recursion to compute the factorial.

factorial_func <- function(n) {if(n <= 0) 1 else n * factorial_func(n-1) }


# 4. Factorial_mem: 
res <- c(1, rep(NA,100))
factorial_mem <- function(n) {
  
  stopifnot(n >= 0)
  
  if (!is.na(res[n])) {
    res[n]
  } else {
    res[n] <<- n * factorial_mem(n-1)
  }
  return(res[n])
}

# Performance of 4 functions with microbenchmark:

# We use the microbenchmark package to time the operation of 4 functions and
# provide a summary of their performance. 

time_test <- function(n) {
  microbenchmark(factorial_loop(n), factorial_reduce(n), factorial_func(n), factorial_mem(n))
}


microbenchmark_results <- map(seq(from = 5, to = 60, by = 10),time_test)
names(microbenchmark_results) <- as.character(seq(from = 5, to = 60, by = 10))

sink("factorial_output.txt") # Send R Output to a File

cat("results of microbenchmark tests\n")
microbenchmark_results

sink()


