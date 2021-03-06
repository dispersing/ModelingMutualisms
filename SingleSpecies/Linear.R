### functions ###
	births <- function(b, X, N){
		b-(X*N)
		}
	
	deaths <- function(d, Y, N) {
		d+(Y*N)
		}
	
	logistic <- function(X, Y, b, d, N){
		(births(b, X, N)-deaths(d, Y, N))*N
		}
	
	logistic.pc <- function(X, Y, b, d, N){
		(births(b, X, N)-deaths(d, Y, N))
		}

### parameters ###
	X <- 1
	Y <- 1
	b <- 10
	d <- 1

### limits ###
	N.min <- 0
	N.max <- 5
### plot ###
	par(mfrow = c(1,1) , pin = c(3,3))
	curve(logistic(X = X, Y = Y, b = b, d = d, N = x) , from = N.min , to = N.max , xlab = "N" , ylab = "" , las = 1)
	abline(h = 0 , v = 0)
	abline(b,-X , lty = 2 , col = "#FF0000")
	abline(d,Y , lty = 2 , col = "#0000FF")
	abline(v = (b-d)/(X+Y) ,  , lty = 2)
	curve(logistic.pc(X = X, Y = Y, b = b, d = d, N = x) , from = 0 , to = 5  , col = "#FF00FF" , lwd = 1.5, add = T)
	mtext(expression("black"==frac(dN,dt)) , 4 , 0.5 , las = 1)
	text((b-d)/(X+Y) , 0 , "K" , adj = c(1.25,1.25))
	mtext(expression(frac(dN,dt)~frac(1,N)) , 2 , 2 , las = 1)
