# Author: Jeremy Werner
# This script draws four 'targets' with random 'dart throws' to
# illustrate the standard variance/bias Mean Squared Error example. Drawn are
# examples of variance with no bias, variance with bias, 
# small variance with bias, and small variance with no bias, respectively. 

###################################################################
## Input Section

# n = number of 'dart throws' per target
n = 50

# v = variance of 'dart throws'
v = 1

# b = bias of 'dart throws'
b = 1.5

## End Input Section
###################################################################

require(plotrix)
input_list = list(c(0,v),c(b,v),c(b,v/3), c(0,v/3))

par(mfrow = c(2,2))
lapply(input_list, FUN = function(pars,n_in){
	x = rnorm(n_in, pars[1]*sample(c(-1,1),1), pars[2])
	y = rnorm(n_in, pars[1]*sample(c(-1,1),1), pars[2])
	plot(0,0, type = "n", xlim = c(-4,4), ylim = c(-4,4), ylab = "", xlab = "")
	draw.circle(x=0,y=0,c(.1,1,2,3))
	points(x,y, pch = 4, col = "RED")
}, n_in = n)
