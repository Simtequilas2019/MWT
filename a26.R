riemann_int <- function(f,n) {
  if (n==0){
    m = 1
  } else {
    m = m = 2*3^(n-1)
  }
  x = seq(0,1,1/(m))
  len = length(x)
  values = numeric(len)
  integral = 0
  for (i in 1:len) {
    values[i] = f(x[i],n)
    integral = integral + values[i]*(1/len)
  }
  print(values)
  print(x)
  print(integral)
}