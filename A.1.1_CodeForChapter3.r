	# WARNING:  run on R version  3.1.0 and 3.1.1
		# Also, some of these models are very time consuming to run; to obtain more rapid if less
		# well supported results, one can reduce the number of iterations.


		# install library of main functions
		library(devtools)
		library(roxygen2)
		install_github('zoevanhavre/Zmix')
		library(Zmix)

		# Simulate HMMs:
		## PLOT SIMULATIONs ##########################
		set.seed(222)

		#pdf("SimHists.pdf", width=8, height=5)
		par(mfrow=c(2,2), mar=c( 5.1, 2.6 ,2.1, 1.5))
		hist(d1$Y, freq=F,main="Sim 1", xlab='Y', ylim=c(0, 0.28))
		lines(density(sim6func(n=100000)$Y), lty=2, lwd=2, col="firebrick1")

		hist(d2$Y, freq=F, main="Sim 2", xlab='Y', ylim=c(0,.25))
		lines(density(sim1func(n=100000)$Y),  lty=2, lwd=2, col="firebrick1")

		hist(d3$Y, freq=F,main="Sim 3", xlab='Y', ylim=c(0, 0.28))
		lines(density(sim2func(n=100000)$Y), lty=2, lwd=2, col="firebrick1")

		hist(d4$Y, freq=F,main="Sim 4",xlab='Y', ylim=c(0, 0.28))
		lines(density(sim5func(n=100000)$Y), lty=2, lwd=2, col="firebrick1")

		#dev.off()

		#### PART 1: Exploratory simulations
		set.seed(222)
		# create datasets
		S1n100<-sim6func(n=100)
		S2n100<-sim1func(n=100)
		S3n100<-sim2func(n=100)
		S4n100<-sim5func(n=100)

		S1n200<-sim6func(n=200)
		S2n200<-sim1func(n=200)
		S3n200<-sim2func(n=200)
		S4n200<-sim5func(n=200)

		# Run Zmix with tempering
		S1n100.zmix<-Zmix_univ_tempered(y=S1n100, k=10,iter=50000,  tau=1, isSim=TRUE, alphas= c(30,
		 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))))
		S2n100.zmix<-Zmix_univ_tempered(y=S2n100, k=10,iter=50000,  tau=1, isSim=TRUE, alphas= c(30,
		 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))))
		S3n100.zmix<-Zmix_univ_tempered(y=S3n100, k=10,iter=50000,  tau=1, isSim=TRUE, alphas= c(30,
		 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))))
		S4n100.zmix<-Zmix_univ_tempered(y=S4n100, k=10,iter=50000,  tau=1, isSim=TRUE, alphas= c(30,
		 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))))
		S1n200.zmix<-Zmix_univ_tempered(y=S1n200, k=10,iter=50000,  tau=1, isSim=TRUE, alphas= c(30,
		 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))))
		S2n200.zmix<-Zmix_univ_tempered(y=S2n200, k=10,iter=50000,  tau=1, isSim=TRUE, alphas= c(30,
		 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))))
		S3n200.zmix<-Zmix_univ_tempered(y=S3n200, k=10,iter=50000,  tau=1, isSim=TRUE, alphas= c(30,
		 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))))
		S4n200.zmix<-Zmix_univ_tempered(y=S4n200, k=10,iter=50000,  tau=1, isSim=TRUE, alphas= c(30,
		 20, 10, 5, 3, 1, 0.5, 1/2^(c(2,3,4,5,6, 8, 10, 15, 20, 30))))

		# Process results including label switching
		## Plots will be created and saved in working directory
		S1n100.zmix.pp<-Process_Output_Zmix(S1n100.zmix, isSim=TRUE,  Burn=20000, LineUp=1, Pred_Reps=1000, Zswitch_Sensitivity=0.01, makePlots=TRUE, Plot_Title="S1n100.zmix", SaveFileName="S1n100.zmix", PlotType="Boxplot")
		S2n100.zmix.pp<-Process_Output_Zmix(S2n100.zmix, isSim=TRUE,  Burn=20000, LineUp=1, Pred_Reps=1000, Zswitch_Sensitivity=0.01, makePlots=TRUE, Plot_Title="S2n100.zmix", SaveFileName="S2n100.zmix", PlotType="Boxplot")
		S3n100.zmix.pp<-Process_Output_Zmix(S3n100.zmix, isSim=TRUE,  Burn=20000, LineUp=1, Pred_Reps=1000, Zswitch_Sensitivity=0.01, makePlots=TRUE, Plot_Title="S3n100.zmix", SaveFileName="S3n100.zmix", PlotType="Boxplot")
		S4n100.zmix.pp<-Process_Output_Zmix(S4n100.zmix, isSim=TRUE,  Burn=20000, LineUp=1, Pred_Reps=1000, Zswitch_Sensitivity=0.01, makePlots=TRUE, Plot_Title="S4n100.zmix", SaveFileName="S4n100.zmix", PlotType="Boxplot")
		S1n200.zmix.pp<-Process_Output_Zmix(S1n200.zmix, isSim=TRUE,  Burn=20000, LineUp=1, Pred_Reps=1000, Zswitch_Sensitivity=0.01, makePlots=TRUE, Plot_Title="S1n200.zmix", SaveFileName="S1n200.zmix", PlotType="Boxplot")
		S2n200.zmix.pp<-Process_Output_Zmix(S2n200.zmix, isSim=TRUE,  Burn=20000, LineUp=1, Pred_Reps=1000, Zswitch_Sensitivity=0.01, makePlots=TRUE, Plot_Title="S2n200.zmix", SaveFileName="S2n200.zmix", PlotType="Boxplot")
		S3n200.zmix.pp<-Process_Output_Zmix(S3n200.zmix, isSim=TRUE,  Burn=20000, LineUp=1, Pred_Reps=1000, Zswitch_Sensitivity=0.01, makePlots=TRUE, Plot_Title="S3n200.zmix", SaveFileName="S3n200.zmix", PlotType="Boxplot")
		S4n200.zmix.pp<-Process_Output_Zmix(S4n200.zmix, isSim=TRUE,  Burn=20000, LineUp=1, Pred_Reps=1000, Zswitch_Sensitivity=0.01, makePlots=TRUE, Plot_Title="S4n200.zmix", SaveFileName="S4n200.zmix", PlotType="Boxplot")

		# model statistics
		S1n100.zmix.pp[[2]]
		S2n100.zmix.pp[[2]]
		S3n100.zmix.pp[[2]]
		S4n100.zmix.pp[[2]]

		S1n200.zmix.pp[[2]]
		S2n200.zmix.pp[[2]]
		S3n200.zmix.pp[[2]]
		S4n200.zmix.pp[[2]]

		# Estimated parameters
		S1n100.zmix.pp[[1]]
		S2n100.zmix.pp[[1]]
		S3n100.zmix.pp[[1]]
		S4n100.zmix.pp[[1]]

		S1n200.zmix.pp[[1]]
		S2n200.zmix.pp[[1]]
		S3n200.zmix.pp[[1]]
		S4n200.zmix.pp[[1]]

		# CASE STUDIES
		#
		set.seed(1)
		data(Galaxy)
		Galaxy.zmix <- Zmix_univ_tempered(Galaxy , tau=1, iter=50000, k=10)
		Galaxy.zmix.pp<-Process_Output_Zmix(runGalaxy2.zmix, LineUp=1, Pred_Reps=1000,
		Zswitch_Sensitivity=0.01, isSim=FALSE, Plot_Title="Galaxy", SaveFileName="Galaxy_zmix", Burn=20000)
		Galaxy.zmix.pp[[2]];Galaxy.zmix.pp[[1]]

		data(Enzyme)
		Enzyme.zmix <- Zmix_univ_tempered(Enzyme , tau=1, iter=50000, k=10)
		Enzyme.zmix.pp<-Process_Output_Zmix(runEnzyme2.zmix, LineUp=1, Pred_Reps=1000,
		Zswitch_Sensitivity=0.01, isSim=FALSE, Plot_Title="Enzyme", SaveFileName="Enzyme_zmix", Burn=20000)
		Enzyme.zmix.pp[[2]];Enzyme.zmix.pp[[1]]

		data(Acidity)
		Acidity.zmix <- Zmix_univ_tempered(Acidity , tau=1, iter=50000, k=10)
		Acidity.zmix.pp<-Process_Output_Zmix(runAcidity2.zmix, LineUp=1, Pred_Reps=1000,
		Zswitch_Sensitivity=0.01, isSim=FALSE, Plot_Title="Acidity", SaveFileName="Acidity_zmix", Burn=20000)
		Acidity.zmix.pp[[2]];Acidity.zmix.pp[[1]]


		# secondary Run for Galaxy with smaller tau:
		set.seed(1)
		Galaxy.zmix2 <- Zmix_univ_tempered(Galaxy , tau=0.01, iter=50000, k=10)
		Galaxy.zmix2.pp<-Process_Output_Zmix(runGalaxy2.zmix, LineUp=1, Pred_Reps=1000,
		 Zswitch_Sensitivity=0.01, isSim=FALSE, Plot_Title="Galaxy, tau=0.01", SaveFileName="Galaxy_zmix2", Burn=20000)
		Galaxy.zmix2.pp[[2]];Galaxy.zmix2.pp[[1]]