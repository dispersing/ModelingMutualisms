#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                #
#  This creates a a 3 x 2 panel of graphs of     #
#  mutualistic interactions modeled using the    #
#  glorious logisitc equation.  They are:        #
#                                                #
#  i-ii. populatoins 1 and 2 without mutualism   #
#  iii. a population with mutualism againt the   #
#       logistic equation                        #
#  iv.  populations 1 and 2 with different       #
        mutualistic effects                      #
#  v. populations 1 and 2 with different values  #
#     of K                                       #
#  vi. a population where the mutualistic effect #
#      is added normally (-alpha*N_j) and when   #
#      it is added to K                          #
#                                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### prepare R ###
rm(list=ls())
set.seed(6174)
library(deSolve)

### functions ###
	mut.pop.dyn <- function(t , y , parms) {
		with(as.list(c(y,parms)), {
		dN1 <- r*N1*(1-(N1-(a*N2))/K1)
		dN2 <- r*N2*(1-(N2-(B*N1))/K2)
		list(c(dN1 , dN2))
		})
	}
	
	logistic <- function(t , y , parms) {
		with(as.list(c(y , parms)), {
			dN1 <- r*N1*(1-N1/K1)
			list(c(dN1))
		})
	}
	
	mut.pop.dyn.denom <- function(t , y , parms){
		with(as.list(c(y , parms)), {
		dN1 <- r*N1*(1-(N1/(K1+(a*N2))))
		dN2 <- r*N2*(1-(N2/(K2+(a*N1))))
		list(c(dN1 , dN2))
		})
		
	}


### plots ###
	par(mfrow=c(3,2) , oma = c(1,1,2,2) , mar = c(2,2,1,1) , cex = 0.9)

	# plot N1 v. N2
	#parameters
	yini <- c(N1 = 1 , N2 = 1)
	times <- seq(0 , 65 , 1)
	parms <- c(r = 0.2 , K1 = 20 , K2 = 20 , a = 0.25 , B = 0.25)
	out <- ode(times = times , y = yini , func = mut.pop.dyn , parms = parms)
	out.log <- ode(times = times , y = c(N1=1) , func = logistic , parms = parms)
	# plot
	plot(out[,2] , xlab = "" , ylab = "" , las = 1 , type = "l" , lwd = 2 , xaxt = "n" , yaxt = "n")
	mtext(expression(Population~density~(N[1])) , 2 , line = 0.5)
	mtext(expression(time~(t)) , 1 , line = 0.75)
	plot(out[,3] , xlab = "" , ylab = "" , las = 1 , type = "l" , lwd = 2 , xaxt = "n" , yaxt = "n" , lty = 2)
	mtext(expression(Population~density~(N[2])) , 2 , line = 0.5)
	mtext(expression(time~(t)) , 1 , line = 0.75)

	#plot N1 v. logistic
	plot(out[,2] , xlab = "" , ylab = "" , las = 1 , type = "l" , lwd = 2 , xaxt = "n" , yaxt = "n")
	mtext(expression(Population~density~(N[1])) , 2 , line = 0.5)
	mtext(expression(time~(t)) , 1 , line = 0.75)
	lines(out.log[,2] , col = "#FF7777" , lwd = 2)
	legend("topleft" , legend = c(expression(no~mutualist),"(red)") , bty = "n" , adj = c(.15,0.2) , cex = 0.9)

	#plot different alphas
	#parameters
	yini <- c(N1 = 1 , N2 = 1)
	parms <- c(r = 0.2 , K1 = 20 , K2 = 20 , a = 0.25 , B = 0.1)
	out <- ode(times = times , y = yini , func = mut.pop.dyn , parms = parms)
	# plot
	plot(out[,2] , xlab = "" , ylab = "" , las = 1 , type = "l" , lwd = 2 , xaxt = "n" , yaxt = "n")
	mtext(expression(Population~density~(N)) , 2 , line = 0.5)
	mtext(expression(time~(t)) , 1 , line = 0.75)
	lines(out[,3] , lty = 2 , lwd = 2)
	lines(out.log[,2] , col = "#FF7777" , lwd = 2)
	legend("topleft" , legend = c(expression(alpha~(solid)),expression(beta~(solid))) , bty = "n" , adj = c(.15,0.2) , cex = 0.9)

	#plot different Ks
	#parameters
	yini <- c(N1 = 1 , N2 = 1)
	parms <- c(r = 0.2 , K1 = 20 , K2 = 10 , a = 0.25 , B = 0.25)
	out <- ode(times = times , y = yini , func = mut.pop.dyn , parms = parms)
	out.log.K1 <- ode(times = times , y = c(N1=1) , func = logistic , parms = parms)
	parms <- c(r = 0.2 , K1 = 10 , K2 = 20 , a = 0.25 , B = 0.25)
	out.log.K2 <- ode(times = times , y = c(N1=1) , func = logistic , parms = parms)

	# plot
	plot(out[,2] , xlab = "" , ylab = "" , las = 1 , type = "l" , lwd = 2 , xaxt = "n" , yaxt = "n")
	mtext(expression(Population~density~(N)) , 2 , line = 0.5)
	mtext(expression(time~(t)) , 1 , line = 0.75)
	lines(out[,3] , lty = 2 , lwd = 2)
	lines(out.log.K1[,2] , col = "#FF7777" , lwd = 2)
	lines(out.log.K2[,2] , col = "#FF7777" , lwd = 2 , lty = 2)
	legend("topleft" , legend = expression(K[1]!=K[2]) , bty = "n" , adj = c(.15,0.2) , cex = 0.9)

	#plot alpha in denominator
	parms <- c(r = 0.2 , K1 = 20 , K2 = 20 , a = 0.25 , B = 0.25)
	out <- ode(times = times , y = yini , func = mut.pop.dyn , parms = parms)
	out.denom <- ode(times = times , y = yini , func = mut.pop.dyn.denom , parms = parms)
	plot(out[,2] , xlab = "" , ylab = "" , las = 1 , type = "l" , lwd = 2 , xaxt = "n" , yaxt = "n")
	mtext(expression(Population~density~(N)) , 2 , line = 0.5)
	mtext(expression(time~(t)) , 1 , line = 0.75)
	lines(out.denom[,2] , col = "#7777FF" , lwd = 2)
	lines(out.log[,2] , col = "#FF7777" , lwd = 2)
	legend("topleft" , legend = c("demoninator" , expression(with~alpha~(blue))) , bty = "n" , adj = c(.15,0.2) , cex = 0.9)
