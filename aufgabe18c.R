cantor_function <- function(x,n){
  if (n==0){
    return(x)
  } else {
    if (0<=x && x<=1/3){
      return(0.5*cantor_function(3*x,n-1))
    } else if (1/3 < x && x < 2/3) {
      return(0.5)
    } else if (2/3<=x && x <=1) {
      return(0.5 + 0.5*cantor_function(3*x-2,n-1))
    } else {
      return("FEHLER")
    }
  }
}

x = seq(0,1,0.005)
val_1 = numeric(200)
val_2 = numeric(200)
val_5 = numeric(200)
val_10 = numeric(200)

for (i in 1:length(x)){
  val_1[i] = cantor_function(x[i],1)
}
plot(x,val_1)


for (i in 1:length(x)){
  val_2[i] = cantor_function(x[i],2)
}
plot(x,val_2)

for (i in 1:length(x)){
  val_5[i] = cantor_function(x[i],5)
}
plot(x,val_5)

for (i in 1:length(x)){
  val_10[i] = cantor_function(x[i],10)
}
plot(x,val_10)




