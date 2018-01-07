mindist<-function(this.point, other.points){
# Calculate distance to nearest point in an auxilliary set
	# distance-squared from this.point to all others
	dist2<-(other.points$X-this.point$X)^2+(other.points$Y-this.point$Y)^2
	mindist2<-min(dist2)
	return(sqrt(mindist2))
	}

avmindist<-function(focal.points,other.points){
	n.focal.points<-dim(focal.points)[1]
	total.mindist<-0
	for (i in 1:n.focal.points){
		this.point<-focal.points[i,]
		total.mindist<-total.mindist+mindist(this.point, other.points)
		}
	return(total.mindist/n.focal.points)
	}

resample.adults<-function(fertile, sterile){
	# Takes the list of actual fertiles and steriles, and 
	# produces a random resample from these

	n.fertile<-dim(fertile)[1]
	n.sterile<-dim(sterile)[1]

	all.plants<-rbind(fertile, sterile)
	n.all<-n.fertile+n.sterile
	# Create a random subsample of size n.fertile 
	i.new.fertile<-sample(1:n.all,n.fertile)
	new.fertile<-all.plants[i.new.fertile,]
	# All the other trees are `sterile'
	new.sterile<-all.plants[-i.new.fertile,]
	return(list(NEW.FERT=new.fertile,NEW.STER=new.sterile))
	}

count.sig<-0
avmd.data<-avmindist(seedling, fertile)
nsamples<-1000
for (isamp in 1:nsamples){
	resampled<-resample.adults(fertile, sterile)
	new.fertile<-resampled$NEW.FERT
	new.sterile<-resampled$NEW.STER
	sig<- avmd.data > avmindist(seedling, new.fertile)
	count.sig<-count.sig + sig
}

pval<-count.sig/nsamples
pval



generate.adult.positions<-function(n.sterile, n.fertile, xmax, ymax){
	# Adult plants distributed at random
	fertile<-data.frame(X=runif(n.fertile, 0, xmax), Y=runif(n.fertile, 0, ymax))
	sterile<-data.frame(X=runif(n.sterile, 0, xmax), Y=runif(n.sterile, 0, ymax))
	return(list(FERT=fertile, STER=sterile))
	}
generate.seedling.positions<-function(n.seedling, fertile, ss){
	# Generate vector of exponentially distributed 
	# displacement of seedlings from parents
	r.sap<-ss*rexp(n.seedling)
	# Vector of angles of displacement of seedlings from parents
	theta.sap<-runif(n.seedling, min=0, max=2*pi)
	seedlings<-data.frame(X=r.sap*cos(theta.sap), Y=r.sap*sin(theta.sap))
	# Choose seedlings parent identity at random from fertiles
	n.fertile<-dim(fertile)[1]
	iparent<-sample(1:n.fertile, n.seedling, replace=TRUE)
	seedlings<-seedlings+fertile[iparent,1:2]
	return(data.frame(seedlings, CLASS=rep("seedling",n.seedling)))
	}

adults<-generate.adult.positions(1000, 300, 30, 30)
fertile<-data.frame(adults$FERT, CLASS=rep("fertile", dim(adults$FERT)[1]))
sterile<-data.frame(adults$STER, CLASS=rep("sterile", dim(adults$STER)[1]))
seedling<-generate.seedling.positions(50, fertile, 3.5)
plants<-rbind(fertile, sterile, seedling)	

pval.distance<-function(ss, nster=1000, nfert=300, nseed=50, xmax=30, ymax=30){
	adults<-generate.adult.positions(nster, nfert, xmax, ymax)
	fertile<-adults$FERT
	sterile<-adults$STER
	seedling<-generate.seedling.positions(nseed, fertile, ss)
	#adults2<-generate.adult.positions(nseed, 0, xmax, ymax)
	seedling<-adults2$STER
	count.sig<-0
	avmd.data<-avmindist(seedling, fertile)
	for (isamp in 1:1000){
		resampled<-resample.adults(fertile, sterile)
		new.fertile<-resampled$NEW.FERT
		new.sterile<-resampled$NEW.STER
		sig<- avmd.data < avmindist(seedling, new.fertile)
		count.sig<-count.sig + sig
		}
	return(list(NSIG=count.sig, LSS=avmindist(seedling, sterile), LSF=avmindist(seedling, fertile)))
	}


