# 1a helper func
f_parta <- function(x){
  x^3 + 23
}

# 1b helper func
f_partb <- function(x){
  x^x - 18
}

# 1c helper func
f_partc <- function(x){
  exp(-(x^2)) - 0.1
}

# 2 helper func
g_part2 <- function(x){
  log(18, x)
}

# 3a func
f_reg <- function(x, a){
  x * x - a
}
f_prime <- function(x){
  2 * x
}
get_sqrt <- function(a, tol, iter_max = 1000, verbose = TRUE){
  #This function will compute the square root of an arbitrary value without the square root function
  #Args:
  #a: integer vector of length 1
  #tol: integer vector of length 1
  #iter_max: integer vector of length 1
  #verbose: logical vector of length 1
  #Return:
  #x: integer vector of length 1
  #its: integer vector of length 1
  if(a < 0){
    stop('a must be non negative')
  }
  its <- 0
  x <- 2
  tol <- 1e-8
  while((abs(f_reg(x, a)) > tol) & its < iter_max){
  x <- x - f_reg(x, a) / f_prime(x)
  its <- its + 1
  }
  if(verbose == TRUE){
    print(its)
  }
  x
}

# 3d
f_reg2 <- function(x, a, b){
  x^b - a
}
f_prime2 <- function(x, b){
  b*x^(b-1)
}
get_abroot <- function(a, b, tol, iter_max = 1000, verbose = TRUE){
  #This function will compute the b-th root of an arbitrary value without the square root function
  #Args:
  #a: integer vector of length 1
  #b: integer vector of length 1
  #tol: integer vector of length 1
  #iter_max: integer vector of length 1
  #verbose: logical vector of length 1
  #Return:
  #x: integer vector of length 1
  #its: integer vector of length 1
  if(a < 0 | b < 0){
    stop('a and b must be non negative')
  }
  its <- 0
  x <- 5
  tol <- 1e-8
  while((abs(f_reg2(x, a, b)) > tol) & its < iter_max){
    x <- x - f_reg2(x, a, b) / f_prime2(x, b)
    its <- its + 1
  }
  if(verbose == TRUE){
    print(its)
  }
  x
}

f <- function(x, n, a){
  x^n - n*a*log(x)
}
f_expr <- expression(x^n - n*a*log(x))
f_prime_1 <-  D(f_expr, name = "x")
f_prime_2 <-  D(f_prime_1, name = "x")
get_min <- function(f, x, n, a){
  #This function will compute the minimum value of a function
  #Args:
  #a: integer vector of length 1
  #n: integer vector of length 1
  #x: integer vector of length 1
  #f: function
  #Return:
  #x: integer vector of length 1
  #its: integer vector of length 1
  x <- 1
  its <- 0
  tol <- 1e-8
  while(abs(eval(f_prime_1)) > tol){
  x <- x - eval(f_prime_1) / eval(f_prime_2)
  its <- its + 1
  }
  print(its)
  x
}