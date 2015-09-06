# Functions available in github repository, use the following to install.
library(devtools); library(roxygen2) 			# Required to install packages from Github
install_github('zoevanhavre/Zmix_devVersion2') 		# Downloads and installs functions
library(ZmixUnderConstruction)				# Load package


# Processied  results used in the paper can be directly accessed with
data(PD_y1_pp)
data(PD_y2_pp)
data(PD_y3_pp)



# Running Analysis:

# IMPORTANT: Many files will be created from the following code. Please move to a working directory.
# WARNING: Very computationally intensive. Suggest supercomputer, and plenty of time and processing power.
# FOR A QUICK TEST: run a smaller version, reducing the number of iterations "iter=50000", and/or the number of values of alpha (each creates a tempering chain - do not remove lowest value(s))


# ANALYSIS
# load data
data(PD_y1);data(PD_rawY1);data(PD_y2); data(PD_rawY2); data(PD_y3); data(PD_rawY3)

# Gibbs sampler with Prior Parallel Tempering, 10 Groups
PD_y1_zmix<-Zmix_multi_tempered(YZ=PD_y1, iter=50000, k=10,  alphas= c(30, 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))), sim=FALSE, EndSize=25000)
PD_y2_zmix<-Zmix_multi_tempered(YZ=PD_y2, iter=50000, k=10,  alphas= c(30, 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))), sim=FALSE, EndSize=25000)
PD_y3_zmix<-Zmix_multi_tempered(YZ=PD_y3, iter=50000, k=10,  alphas= c(30, 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))), sim=FALSE, EndSize=25000)

# Post-processing +  label switching
PD_y1_pp<-PostProc_mvn_PD( PD_y1_zmix,  PD_y1_zmix$Y, PD_rawY1,Propmin=0.01, isSim=FALSE, simlabel="Y1", savelabel="PD_y1")
PD_y2_pp<-PostProc_mvn_PD( PD_y2_zmix,  PD_y2_zmix$Y,PD_rawY2,Propmin=0.01, isSim=FALSE, simlabel="Y2", savelabel="PD_y2")
PD_y3_pp<-PostProc_mvn_PD( PD_y3_zmix,  PD_y3_zmix$Y,PD_rawY3,Propmin=0.01, isSim=FALSE, simlabel="Y3", savelabel="PD_y3")


## To SAVE: Uncomment  the following
# save(PD_y1_zmix, file="PD_y1_zmix.RDATA")
# save(PD_y1_pp, file="PD_y1_pp.RDATA"  )
# save(PD_y2_zmix, file="PD_y2_zmix.RDATA")
# save(PD_y2_pp, file="PD_y2_pp.RDATA"  )
# save(PD_y3_zmix, file="PD_y3_zmix.RDATA")
# save(PD_y3_pp, file="PD_y3_pp.RDATA"  )


############# CODE FOR PLOTS
#Pairwise Allocation Similarities
Y1_Simi<-diag(dim(PD_y1_zmix[[4]])[2])
for (rr in 1:dim(Y1_Simi)[1]){
	for (cc in 1:dim(Y1_Simi)[2]){
		.y1<-			PD_y1_zmix[[4]][,rr]
		.y2<-			PD_y1_zmix[[4]][,cc]
		# percentage true
		Y1_Simi[rr,cc]<- sum(.y1==.y2)/length(.y1)
}	}
Y2_Simi<-diag(dim(PD_y2_zmix[[4]])[2])
for (rr in 1:dim(Y2_Simi)[1]){
	for (cc in 1:dim(Y2_Simi)[2]){
		.y1<-			PD_y2_zmix[[4]][,rr]
		.y2<-			PD_y2_zmix[[4]][,cc]
Y2_Simi[rr,cc]<- sum(.y1==.y2)/length(.y1)
}}
Y3_Simi<-diag(dim(PD_y3_zmix[[4]])[2])
for (rr in 1:dim(Y3_Simi)[1]){
	for (cc in 1:dim(Y3_Simi)[2]){
		.y1<-			PD_y3_zmix[[4]][,rr]
		.y2<-			PD_y3_zmix[[4]][,cc]
		# percentage true
		Y3_Simi[rr,cc]<- sum(.y1==.y2)/length(.y1)
}}


#pdf("ZmixSimi.pdf", width=8, height=3,pointsize=10)
par(mfrow=c(1,3))
image(Y1_Simi[ order(PD_y1_zmix[[5]][,1]), order(PD_y1_zmix[[5]][,1])], main="Y1", col=grey.colors(25), axes=F,  xlab="Index (Y ordered by 1st PC)",ylab="Index (Y ordered by 1st PC)")
axis(2, at=seq(50,dim(Y1_Simi)[1],by=50)/dim(Y1_Simi)[1],  labels=seq(from=50, to=dim(Y1_Simi)[1], by=50))
axis(1, at=seq(50,dim(Y1_Simi)[1],by=50)/dim(Y1_Simi)[1],  labels=seq(from=50, to=dim(Y1_Simi)[1], by=50))
image(Y2_Simi[ order(PD_y2_zmix[[5]][,1]), order(PD_y2_zmix[[5]][,1])], main="Y2", col=grey.colors(25),axes=F,  xlab="Index (Y ordered by 1st PC)",ylab="Index (Y ordered by 1st PC)")
axis(2, at=seq(50,dim(Y2_Simi)[1],by=50)/dim(Y2_Simi)[1],  labels=seq(from=50, to=dim(Y2_Simi)[1], by=50))
axis(1, at=seq(50,dim(Y2_Simi)[1],by=50)/dim(Y2_Simi)[1],  labels=seq(from=50, to=dim(Y2_Simi)[1], by=50))
image(Y3_Simi[ order(PD_y3_zmix[[5]][,1]), order(PD_y3_zmix[[5]][,1])], main="Y3", col=grey.colors(25),axes=F,  xlab="Index (Y ordered by 1st PC)",ylab="Index (Y ordered by 1st PC)")
axis(2, at=seq(50,dim(Y3_Simi)[1],by=50)/dim(Y3_Simi)[1],  labels=seq(from=50, to=dim(Y3_Simi)[1], by=50))
axis(1, at=seq(50,dim(Y3_Simi)[1],by=50)/dim(Y3_Simi)[1],  labels=seq(from=50, to=dim(Y3_Simi)[1], by=50))
#dev.off()



# Clustered Spikes
library(plyr)
	rawData	<-PD_rawY1
	Zhat		<-PD_y1_pp[[3]][[2]]
	.rawData	<-melt(rawData)
	.rawData	<-cbind("Time"=rep(1:dim(rawData)[1], dim(rawData)[2]), .rawData)
	zzs		<-data.frame( 'variable'=unique(.rawData$variable),'Zs'=Zhat)
	.rawData	<-merge(.rawData, zzs, by='variable')
	fp1		<-.rawData

	rawData	<-PD_rawY2
	Zhat		<-PD_y2_pp[[3]][[2]]
	.rawData	<-melt(rawData)
	.rawData	<-cbind("Time"=rep(1:dim(rawData)[1], dim(rawData)[2]), .rawData)
	zzs		<-data.frame( 'variable'=unique(.rawData$variable),'Zs'=Zhat)
	.rawData	<-merge(.rawData, zzs, by='variable')
	fp2		<-.rawData


	rawData	<-PD_rawY3
	Zhat		<-PD_y3_pp[[3]][[1]]    # Results may change slightly. ensure this refers to the best model.
	.rawData	<-melt(rawData)
	.rawData	<-cbind("Time"=rep(1:dim(rawData)[1], dim(rawData)[2]), .rawData)
	zzs		<-data.frame( 'variable'=unique(.rawData$variable),'Zs'=Zhat)
	.rawData	<-merge(.rawData, zzs, by='variable')
	fp3		<-.rawData


ggplot(fp1, aes(x=Time, y=value, group=variable))+geom_line(aes(colour=Zs), alpha=0.4)+ggtitle("Y1 clustered spikes (OF)")+theme_bw()
ggplot(fp2, aes(x=Time, y=value, group=variable))+geom_line(aes(colour=Zs), alpha=0.4)+ggtitle("Y2 clustered spikes (OF)")+theme_bw()
ggplot(fp3, aes(x=Time, y=value, group=variable))+geom_line(aes(colour=Zs), alpha=0.4)+ggtitle("Y3 clustered spikes (OF)")+theme_bw()

