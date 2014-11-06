# Author: Jeremy Werner

# This script will calculate the approximate probability of any two people in a
# room of size N having the same birthday. It will print a graph showing the
# probability for the input N

######
N = 40
######


# Calculates an approximation to the birthday problem's probability
prob_fun <- function(n){
	x = exp(-n*(n-1)/(2*365))
	return(1 - x)
}

if(N<40){
	vect = 1:60
	plot(vect, prob_fun(vect), main = "Birthday Probabilities", xlab = "Number of People", ylab = "Probability of a pair")
	abline(v = N, col = "RED", lty = 2)
	abline(prob_fun(N), 0, col = "RED", lty = 2)
	text(N, prob_fun(N/2), paste("N=", N, "P(N)=", round(prob_fun(N), 3)), pos = 4, offset = 0.2)
}else{
	vect = 1:(N+20)
	plot(vect, prob_fun(vect), main = "Birthday Probabilities", xlab = "Number of People", ylab = "Probability of a pair")
	abline(v = N, col = "RED", lty = 2)
	abline(prob_fun(N), 0, col = "RED", lty = 2)
	text(N/2, .1, paste("N=", N, "P(N)=", round(prob_fun(N), 3)), pos = 4, offset = 0.2)
}


