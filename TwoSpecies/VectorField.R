###  prep R ###
	rm(list=ls())
	set.seed(6174)
	library(phaseR)
	library(deSolve)

### functions ###
	# ode
	mut.pop.dyn <- function(t , y , parameters){
			x <- y[1]
			y <- y[2]
			r <- parameters[1]
			K <- parameters[2]
			alpha <- parameters[3]
			beta <- parameters[4]
			dy <- numeric(2)
			dy[1] <- r*x*(1-(x-(alpha*y))/K)
			dy[2] <- r*y*(1-(y-(beta*x))/K)
			list(dy)
	}

	# plot vector field and nullclines at once
	phasePlot <- function(mut.pop.dyn , parameters) {
	x.lim <- c(-5 , 40)
	y.lim <- c(-5 , 40)
	plot(0 , type = "n" , xlim = x.lim , ylim = y.lim , main = "" , xlab = "" , ylab = "" , las = 1)
	mtext(expression(N[1]) , 1 , line = 2.5)
	mtext(expression(N[2]) , 2 , line = 2.5 , las = 1)
	flowField(mut.pop.dyn , x.lim = x.lim , y.lim = y.lim , parameters = parms , points = 15 , arrow.type = "proportional")
	abline(v = 0 , h = 0 , lwd = 2)
	nc <- nullclines(mut.pop.dyn , x.lim = x.lim , y.lim = y.lim , parameters = parms , points = 250 , lwd = 1.5)
	}

### plot ###
	parms <- c(r = 0.1 , K = 20 , alpha = 0.25 , beta = 0.25)
	phasePlot(mut.pop.dyn , parameters)

### add trajectory ###
	points <- 8
	x <- c(runif(points , 0 , 40))
	y <- c(runif(points , 0 , 40))
	traj.pts <- matrix(c(x,y) , ncol = 2)
	traj <- trajectory(mut.pop.dyn , y0 = traj.pts , t.end = 100 , parameters = parms)
