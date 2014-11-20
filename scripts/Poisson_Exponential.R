# Author: Jeremy Werner

# This script will repeatedly sample from an exponential distribution and plot the times of events
# sequentially on "number_of_trials" number of timelines. These "number_of_trials" number of timelines 
# will then have their occurrences added up (as printed on the right of the graph), hence creating observations from a 
# poisson distribution while actually being able to picture the event times (instead of just the number, like we usually do). 
# The value of the parameter lambda can be changed as well as the time period over which we are counting events, denoted
# as "lambda_input" and "top_of_time_period_input" respectively. Finally, the sampled poisson realizations can be plotted against 
# the theoretical probability mass function to show similarities

# User inputs
##
lambda_input = 20
number_of_trials = 25
top_of_time_period_input = 15 
##

plot_event_times = function(lambda, ymax, top_of_time_period){

	plot(0,0, type = "n", xlim = c(0,top_of_time_period), ylim = c(1,ymax), yaxt = 'n', ylab = "", xlab = "Time")
	abline(v = 0)
	sampling_vector = rep(0, ymax)

	for(i in 1:ymax){
		ind = 1
		t = 0
		obs = list()
		while(ind){
			# Iterative sampling of exponential distribution coming from inverse CDF trick 
			t = t - log(1-runif(1))/(lambda/top_of_time_period)
			if(t > top_of_time_period){
				ind = 0
				out_vector = unlist(obs)
			}else{
				obs[[ind]] = t
				ind = ind + 1
			}
		}
		sampling_vector[i] = length(out_vector) 
		abline(i, 0, col = "grey")
		points(out_vector, rep(i,length(out_vector)), col = "RED", pch = "|", lwd = 2)
		mtext(length(out_vector), 4, at = i)
	}
	return(sampling_vector)
} # plot_event_times


# Sample density (from above) vs theoretical PMF

plot_density_vs_pmf = function(lambda, sampling_vector_in, tot_in){
	x <- seq(0, 5*tot_in, 1)
	dpois_nums = dpois(x, lambda)
	density_nums = density(sampling_vector_in)
	plot(density_nums, main = "Sample Density vs Poisson PMF", ylim = range(c(dpois_nums,density_nums$y)), type = "n")
	points(x, dpois_nums, type = "h", col = "RED")
	points(x, dpois_nums, col = "RED")
	points(density_nums, type = "l", lwd = 2)  # So the red pmf doesn't overlap the black density, because of ocd
	legend("topright", c("Sample Density", "Poisson PMF"), lty = c(1,1), col = c("black", "red"), pch = c(NA,1), lwd = c(2,1))
}


# Main Block
poisson_samples = plot_event_times(lambda_input, number_of_trials, top_of_time_period_input)
plot_density_vs_pmf(lambda_input, poisson_samples, top_of_time_period_input)
