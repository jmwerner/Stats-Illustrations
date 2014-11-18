


lambda = 5


ymax = 25
top_of_time_period = 10 # 10 is recommended, but code was adjusted to be relatively flexible
plot(0,0, type = "n", xlim = c(0,top_of_time_period), ylim = c(1,ymax), yaxt = 'n', ylab = "", xlab = "Time")
abline(v = 0)
top_of_sampler = 1000
sampler = rep(top_of_time_period+1,top_of_sampler)

for(i in 1:ymax){
	ind = 1
	iter = 2
	sampler[1] = rexp(1,1/lambda)
	while(ind){
		sampler[iter] = sampler[iter-1] + rexp(1, 1/lambda)
		if(sampler[iter] >= 10){
			ind = 0
		}
		iter = iter + 1
	}
	to_plot = sampler[sampler < top_of_time_period & 1:top_of_sampler < iter]

	abline(i, 0, col = "grey")
	points(to_plot, rep(i,length(to_plot)), col = "RED", pch = "|", lwd = 2)
	mtext(length(to_plot), 4, at = i)
}

