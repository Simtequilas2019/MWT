
i <- function(x, y) {

  print(paste0("Das Produkt der Zahlen ", x, " und ", y, " ist ", x*y))
  
}

ii <- function(n) {
  logs <- c()
  for (i in 1:n) {
    logs <- append(logs, log(i,10))
  }
  print(logs)
}

iii <- function(n) {
  print(typeof(n))
  if(!is.integer(n)) stop("Input must be an integer")
  if(n < 0) return(0)
  else return(n)
}

i(3,4)
ii(1000)

# Funktion nimmt nur ganze Zahl
number = as.integer(4)
iii(number)


