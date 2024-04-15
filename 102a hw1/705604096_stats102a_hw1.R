gcd <- function(x, y){
  #This function will compute the greatest common divider of two numbers
  #Args:
  #x: integer of size 1
  #y: integer of size 1
  #Return:
  #The greatest common divider of the two inputs, integer of size 1
  if(x == 0){
    gcd <- y
  }
  if(y == 0){
    gcd <- x
  }
  while(y){
    my_variable <- y
    y <- x%%y
    x <- my_variable
  }
  if(length(x) != 1 | length(y) != 1){
    stop("Input length is invalid")
  }
  print(abs(x))
}


lcm <- function(x){
  #This function will compute the least common multiple of two numbers
  #Args:
  #x: integer vector of length [2, 100]
  #Return:
  #The least common multiple of the vector, integer of size 1
  lcm <- x[1] 
  if(length(x) >= 2 & length(x) <= 100){
    for(i in 2:length(x)){
      my_gcd <- gcd(lcm, x[i])
      lcm <- lcm * x[i] / my_gcd
    }
  } else {
    stop("The vector length is out of range")
  }
  print(lcm)
}



  

is_prime <- function(x){
  #This function will tell the user if numbers in a vector are prime or not
  #Args: 
  #x: integer vector of any length
  #Return:
  #A logical vector answering TRUE if a number is prime or FALSE if a number is not prime
  my_vec <- logical(length(x))
  for(i in seq_along(x)){
  my_vec[i] <- sum(x[i]%%(1:x[i]) == 0) == 2
  }
  print(my_vec)
}


get_factors <- function(x){
  #This function will factorize an input value into it's prime factorization and their respective exponents
  #Args:
  #x: integer of length 1
  #Return:
  #List of values of the prime factorization of the input and their respective exponents
  y <- integer(0)
  exponents <- integer(0)
  for(i in 2:x){
    if(x%%i == 0){
      my_variable <- is_prime(i)
      if(my_variable == TRUE){
        y <- c(y, i)
        exponents <- c(exponents, x^(1/i))
      }
    }
  }
  my_list <- list(y, exponents)
  names(my_list) <- c("primes", "exponents")
  print(my_list)
}








  
