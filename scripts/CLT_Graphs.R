# Author: Jeremy Werner

# This script takes a sample from a skewed distribution (chi^2 with given degrees of 
#  freedom) of size 'sample_n' and calculates its average. The sample points are plotted
#  as green vertical lines and the sample mean in red over the theoretical density as the
#  first graph. The sampling is repeated 'iterations' times and the means of each sample 
#  are plotted in a histogram below. With more iterations and/or larger sample n, the 
#  histogram will become more and more symmetric, hence loosely demonstrating the Central
#  Limit Theorem through repeated sampling. Also, the script will only graph every 
#  'graph_every' iteration to avoid large amounts of iterations taking too long.

######
iterations = 100
graph_every = 10
sample_n = 5
chi_df = 3
######

# Pre-computing the chi^2 density for quick iterative plotting
xval = (1:100000)/1000
yvals = dchisq(xval, df = chi_df)

par(mfrow = c(2,1))
means = rep(-1, iterations)

for(i in 1:iterations){
	rsamp=rchisq(sample_n, df = chi_df)
	m = mean(rsamp)
	means[i] = m
	if(i %% graph_every == 0){
		plot(xval,yvals, xlim = c(0,3*chi_df), type = "l", ylab = "", xlab = "X Value")
		for(k in 1:sample_n){
			abline(v = rsamp[k], col = "green", lwd = 1)
		}
	
		abline(v=m, col = "red", lwd = 2)
		hist(means[1:i], xlab = "Means", xlim = c(0,3+3*chi_df))
	}
} 

