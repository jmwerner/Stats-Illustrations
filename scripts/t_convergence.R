# Author: Jeremy Werner

# This script will plot a normal distribution and a t distribution with n = 1
# degrees freedom. It will then pause until the <enter> key is pressed, then it will
# increment n by 1 and plot again. Under default settings, it will plot 30 times
# and show convergence in the t distribution to the normal distribution as n increases

x <- seq(-4, 4, length=100)
degf <- c(rep(1,5),seq(1,30,1))

for (i in 1:length(degf)){
	plot(x,dnorm(x), type = "l",lwd = 2, main = paste("Degrees Freedom:", degf[i]), ylab = "", xlim = c(-3,3))
 	lines(x, dt(x,degf[i]), col="RED", lwd = 2, lty = 2)
	legend("topright", c("Normal Distribution", "t Distribution"), col = c("Black", "Red"), lwd = c(2,2), lty = c(1,2))
	readline()
}
