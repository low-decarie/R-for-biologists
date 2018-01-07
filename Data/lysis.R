# Lysis model taken from Brown S.P et al, PLoS Biology 2006
# This is model 'mu2'.  Data are simulated from the 
# best fitting parameters.

real_params<- c(0.465, 0.107)

mn<- function(nslots, params){
# Predicted occupancy probabilities for lysis model
	mu0<- params[1]
	mu1<- params[2]
	mmn<- rep(0,nmax)
	mmn[1]<- 1
	for (i in 2:nslots){
		mmn[i]<- mmn[i-1]*(i-1)/(mu0+(mu1+1)*i)
	}
	return(mmn/sum(mmn))
}
nmax<- 50
sim_data<- rpois(nmax, 500*mn(nmax, real_params))
the_data<- sim_data

loglik.multinom<- function(params){ # multinomial likeihood
	predicted.fraction<- mn(nmax, params)
	loglik<- the_data*log(predicted.fraction)-lgamma(the_data+1)
	return(-sum(loglik))
}
mn.fit<- optim(c(2, 2), loglik.multinom)

loglik.ls<- function(params){ # Least squares
	predicted.fraction<- mn(nmax, params)
	loglik<- (the_data/sum(the_data)-predicted.fraction)^2
	return(sum(loglik))
}
ls.fit<- optim(c(2,2) , loglik.ls)

plot(log(mn(50, real_params)), type="l", xlab="number of bacteria", ylab="log(Fraction)") # True model
points(log(the_data/sum(the_data)))
lines(log(mn(50, mn.fit$par)), col="blue")# Multinom fit
lines(log(mn(50, ls.fit$par)), col="red")# least sq fit
