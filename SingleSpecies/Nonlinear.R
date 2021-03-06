### functions ###
	births <- function(b, X, N){
		(b*X)/(X + N)
		}
	
	deaths <- function(d, Y, N) {
		(d*N)/(Y + N)
		}
	
	pita <- function(X, Y, b, d, N){
		(births(b, X, N)-deaths(d, Y, N))*N
		}
	
	pita.pc <- function(X, Y, b, d, N){
		(births(b, X, N)-deaths(d, Y, N))
		}

### parameters ###
	X <- 1
	Y <- 1
	b <- 9
	d <- 3

### limits ###
	N.min <- 0
	N.max <- 14
### plot ###
	par(mfrow = c(1,1) , pin = c(3,3))
	curve(pita(X = X, Y = Y, b = b, d = d, N = x) , from = N.min , to = N.max , xlab = "N" , ylab = "" , las = 1 , ylim = c(-d,b))
	abline(h = 0 , v = 0) #0,0 axes

	curve(births(X = X, b = b, N = x) , from = N.min , to = N.max , add = T , col = "#0000FF" , lty = 2)
	curve(deaths(Y = Y , d = d, N = x) , from = N.min , to = N.max , add = T , col = "#FF0000" , lty = 2)

	abline(h = c(b,0), lty = 1 , col = "#0000FF")
	abline(h = c(d,-d), lty = 1 , col = "#FF0000")
	curve(pita.pc(X = X, Y = Y, b = b, d = d, N = x) , from = N.min , to = N.max  , col = "#FF00FF" , lwd = 1.5, add = T)
	# abline(v = b/d ,  , lty = 2) #when X = Y, b/d is K; uncomment this and the next line
	# text(b/d , 0 , "K" , adj = c(1.25,1.25))
	mtext(expression("black"==frac(dN,dt)) , 4 , 0.5 , las = 1)
	mtext(expression(frac(dN,dt)~frac(1,N)) , 2 , 2 , las = 1)	
