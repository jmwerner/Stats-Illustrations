# Author: Jeremy Werner
# Description: This script will plot two illustrations of approximate confidence
#              intervals. The first graphs 100 confidence intervals of level alpha
#              with repeated sampling. The second plots four samples of different
#              sizes with their respective confidence intervals. Inspiration and
#              outline for first example from link below
# Reference: http://www.math.utah.edu/~treiberg/M3074SimIntEg.pdf


# First CI function

intervals1 = function(alpha, n, m){

	ta2 <- qt(alpha/2,df=n-1,lower.tail=FALSE)
	za2 <- qnorm(alpha/2,lower.tail=FALSE)
	low <-1:100
	high <- 1:100
	mean = 1:100
	captured <- rep(TRUE,100)
	c1 <- ta2/sqrt(n)
	c12 <- c1^2
	for(j in 1:m){
		x <- rnorm(n)
		xbar <- mean(x)
		s <- sd(x)
		captured[j] <- xbar^2 >= c12*s^2
		low[j] <- xbar - c1*s
		high[j] <- xbar + c1*s
		mean[j] = xbar
	}

	plot(c(low,high),type="n",xlim=c(1,100),xlab="Trial",ylab=expression(mu),pch=19)
	abline(h = c(0, za2/sqrt(n), -za2/sqrt(n)), lty = c(1,2,2), col = "gray")
	points(high, col = 3-captured, pch = 20)
	points(low, col = 3-captured, pch = 20)
	points(mean, col = "blue", pch = "-")
	for(i in 1:100){
		lines(c(i,i), c(low[i],high[i]), col = 3-captured[i], pch = 19)
	}

	title(expression(paste("Simulation of t-Confidence Intervals for ", mu, " with Sample Size 20")))

	legend("bottomleft", legend = c(expression(paste(mu," Captured")), expression(paste(mu," Not Captured"))), fill = c(3,2))

}# intervals1


#CI on number lines (different sample sizes)
intervals2 = function(alpha){
	par(mfrow = c(4,1))
	lapply(c(16,25,36,75), FUN = function(x,alph){
		sample = rnorm(x)

		plot(sample, rep(0,length(sample)), xlim = c(-2,2), pch = 4, col = "blue", ylab = "", main = paste("Sample size: ", x, "     Alpha:", alph), yaxt = 'n')
		
		mu = mean(sample)
		k = qnorm(1-alpha/2)*sd(sample)/sqrt(length(sample))
		abline(v = mu + k, col = "RED")
		abline(v = mu - k, col = "RED")
	}, alph = alpha)
}#intervals2



##############
# Main Block #
##############
m_in <- 100
alpha_in <- .05
n_in <- 20

intervals1(alpha_in, n_in, m_in)

###############
alpha_in = .05

intervals2(alpha_in)



