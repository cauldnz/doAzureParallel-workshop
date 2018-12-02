#Define our Birthday Sim Function
pbirthdaysim <- function(n, nsims=100000, feb29=TRUE) {
  ## Using nsims simulations, estimate the probability
  ## that a room of n people includes a shared birthday
  bdays <- 1:366 
  ## Feb 29 represented as day 366
  ## We'll sample other days 4 times as often
  ## compared to day 366
  probs <- c(rep(4,365),1)
  if(!feb29) {
    # drop Feb 29 from bdays and probs
    bdays <- bdays[-366]
    probs <- probs[-366]
  }
  probs <- probs/sum(probs)
  anydup <- function(ignored) 
    ## simulate n birthdays, return TRUE if
    ## there's a duplicate
    any(duplicated(sample(bdays, n, prob=probs, replace=TRUE)))
  
  sum(sapply(seq(nsims), anydup)) / nsims
}

#Run on a single core
bdayp <- 1:100
system.time(for (n in 1:100) bdayp[n] <- pbirthdaysim(n))
plot(bdayp, xlab="People in room", 
     ylab="Probability of shared birthday")
abline(h=0.5)

#Run locally on all cores
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
system.time(bdayp <- foreach(n=1:100) %dopar% pbirthdaysim(n))
bdayp <- unlist(bdayp)
plot(bdayp, xlab="People in room", 
     ylab="Probability of shared birthday")
abline(h=0.5)
