library(mosaicCalc)

# Define the function f(x)
f <- makeFun((3/2)*(x^2) ~ .)

# Find indefinite integral (anti-derivative)
F <- antiD(f(x) ~ x)

# Integrate over range space
F
cat("F(upper)=",F(0.5),"F(lower)=",F(-1),"E(X)=",F(0.5)-F(-1))


f <- makeFun(x + (3/2)*y^2 ~x)