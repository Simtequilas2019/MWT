simulate <- function(n) {
  succes <- 0
  for(i in 1:10000) {
    switch <- c(-1,1)
    stichprobe <- sample(switch,n,replace=TRUE)
    result <- abs(sum(stichprobe))
    if(result <= n*0.1) {
      succes <- succes+1
    }
  }
  acc <- succes/10000
  print(acc)
}
simulate(10000)
