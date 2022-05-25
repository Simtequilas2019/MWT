simulate <- function(n, iterations) {

  sum_birthdays <- c()
  for(i in 1:iterations) {
    # create random birthday vector,
    # number between 1 and 365 stands for
    # birthday of person
    birthdays <- sample(1:365, size=n, replace = TRUE)
    sum_birthdays <- c(sum_birthdays, birthdays)

    # find duplicates
    #duplicates <- duplicated(birthdays)
  }
  h<-hist(sum_birthdays, breaks=365)

  # getting relative frequencies
  h$counts <- h$counts / sum(h$counts)
  title <- sprintf("Histogram of birthdays over %s samples, %s people",
                  iterations, n)
  plot(h, xlab="day of the year", ylab="Relative Frequency", main=title)
}
