pqnumber <- function(sign, p, q, nums){
  #this function will construct a pqnumer object
  #Args:
  #sign: integer vector of length one
  #p: integer vector of length one
  #q: integer vector of length one
  #nums: integer vector between 0-9
  #Return:
  #the pqnumber object if all components are satisfied; integer vector of length 1
  if(abs(sign) != 1){
    stop('The sign input is invalid. This is not a pqnumber.')
  }
  if(abs(p) != p){
    stop('p is invalid, it must be non-negative.')
  }
  if(abs(q) != q){
    stop('q is invalid, it must be non-negative.')
  }
  if(length(nums) != p + q + 1){
    stop('The nums input is invalid. This is not a pqnumber.')
  } 
  structure(list(sign = as.integer(sign), p = as.integer(p), q = as.integer(q), 
                 nums = as.integer(nums)), class = 'pqnumber')
}

is_pqnumber <- function(x){
  #this function will determine if the input is a pqnumber or not
  #Args:
  #x: integer vector of length one
  #Return:
  #logical vector of length one
  class(x) == 'pqnumber'
}

print.pqnumber <- function(x, DEC = TRUE){
  if(DEC){
    print(x)
  }else{
    print_str <- paste0('sign = ', x$sign,
                        '\np = ', x$p,
                        '\nq = ', x$q,
                        '\nnums = ', paste(x$nums, collapse = ''))
  }
  cat(paste0(print_str))
}

as_pqnumber <- function(x, p, q){
  #this function will coerce the input to be of the pqnumber class
  #Args:
  #x: integer vector of length one
  #p: integer vector of length one
  #q: integer vector of length one
  #Return:
  #output will be the coerced pqnumber object
  UseMethod('as_pqnumber')
}

as_numeric.pqnumber <- function(x, p, q){
  #this function will coerce a pqnumber object to a numeric vector
  #Args:
  #x: integer vector of length one
  #p: integer vector of length one
  #q: integer vector of length one
  #Return:
  #output is the coerced numeric vector 
  nums <- (abs(x) * 10^seq(p, -q)) %% 10 %/% 1
  sgn <- if(x == 0) 1 else base::sign(x)
  pqnumber(sign, p, q, nums)
}

subtract <- function(x, y){
  #this function will subtract the two inputs
  #Args:
  #x: integer vector of length one
  #y: integer vector of length one
  #Return:
  #output will be an integer vector of length one; the difference of the two inputs x and y
  y$sign <- y$sign * -1
  add(x, y)
}

carry_over <- function(z){
  #this function will determine what to carry over in our operations
  #Args:
  #z: integer vector 
  #Return:
  #integer vector of length one of what to carry over
  n <- length(z)
  carry <- 0 
  for(i in 1:n){
    zi <- z[i] + carry
    z[i] <- zi %% 10
    carry <- zi %/% 10
  }
  if(carry != 0){
    z[n + 1] <- carry
  }
  z
}

add <- function(x, y){
  #this function will add the two inputs
  #Args:
  #x: integer vector of length one
  #y: integer vector of length one
  #Return:
  #integer vector of length one; the sum of the two inputs
  max_p <- max(x$p, y$p)
  max_q <- max(x$q, y$q)
  n <- max_p + max_q + 1
  z <- rep(0L, n)
  if(x$sign == y$sign){
    x_vals <- x$nums
    y_vals <- y$nums
    sgn <- x$sign
    
  } else {
    if(x > y){
      x_vals <- x$nums
      y_vals <- -y$nums
      sgn <- x$sign
    }else{
      x_vals <- -x$nums
      y_vals <- y$nums
      sgn <- y$sign
    }
  }
  z[(1 + max_p - x$p):(1 + max_p + x$q)] <- z[(1 + max_p - x$p):(1 + max_p + x$q)] + x$nums
  z[(1 + max_p - y$p):(1 + max_p + y$q)] <- z[(1 + max_p - y$p):(1 + max_p + y$q)] + y$nums
  z <- carry_over(z)
  
  
  digit_carry <- length(z) - n
  pqnumber(sgn, max_p, max_q + digit_carry, z)
}

multiply <- function(x, y){
  #this function will multiply two pqnumbers given in the input
  #Args:
  #x: pqnumber; integer vector of length one
  #y: pqnumber; integer vector of length one
  #Return:
  #pqnumber that is the result of the multiplication of two input pqnumbers;
  #integer vector of length one
  n <- (x$q + y$q + 1)
  z <- rep(0L, n)
  for(r in 1:(1 + y$p + y$q)){
    z[r:(r + (x$q + y$q + 1))] <- z[r:(r + (x$q + y$q + 1))] + (x$nums + y$nums[r])
  }
  z <- carry_over(z)
  sgn <- x$sign
  pqnumber(sgn, x$p, x$q + z, z)
}

