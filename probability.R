prob_success <- 0.25
trials <- 16
number_of_successes <- 2
prob_winning = dbinom(number_of_successes, trials, prob_success)
cat("Probability of winning", number_of_successes, "time(s) with", trials, "attempts is", prob_winning, "\n")

#prob_failure <- 4/5
#trials <- 2
#prob_losing = prob_failure^trials
#cat("Probability of winning in at least", trials, "attempts is", 1-prob_losing, "\n")


1/
cat("(10,0,0)",dmultinom(c(10,0,0),
                         prob=c(1/3,1/3,1/3)),"(5,2,4)",
    dmultinom(c(5,2,4),prob=c(1/3,1/3,1/3)))

x <- c(1,1,1,1)
xp <- c(0.5,0.5,0.5,0.5)
mu <- sum(x*xp)
mu2 <- sum((x^2)*xp)
VarX <- mu2 - mu^2
cat("X:",mu,mu2,VarX)

y <- c(1:2)
yp <- c(0.4, 0.6)
ymu <- sum(y*p)
ymu2 <- sum((y^2)*p)
VarY <- ymu2 - ymu^2
cat("y:",ymu,ymu2,VarY)

yxmu <- sum()

# 5096 : Sampling Fundamentals (ODL)
