		

# Install package:
		
		library(devtools)
		library(roxygen2)
		install_github('zoevanhavre/ZmixHMM')
		library(ZmixHMM)


#This package is in development. A working example is provided below to illustrate its use.



# simulate an HMM with n=500

N200_Sim1<-FunkSim1(200)


# run  Gibbs sampler with PPT (J=30 chains, M=20000 iterations, and alphamin= 1/n, type=1 is the column prior)

zmixHMM_N200_Sim1<-gibbsHMM_Main(YZ=N200_Sim1, M=20000, K=10 ,alphaMin=1/500, alphaMAX=1,  type=1, J=30,  SuppressAll=FALSE)

# process results (includes label switching)

PP_zmixHMM_N200_Sim1<-Zhmm_PP( zmixHMM_N200_Sim1 , burn=1000, prep=1000, isSim=TRUE,  simlabel="sim1 small example", MakePlots=TRUE)


## ALTERNATIVE PRIORS

	# Diagonal Prior
	# set type=2

	# Mixture prior
	# set type=3

## Other available simulations:

#Use this function to simulate any HMM
# inputs: Q= transition matrix in matrix format, Mu=MEANS, n=sample size. optional: q0=Stationary distribution. If NA will compute from Q.

MyHMM<-SimHMM(Q,Mu, n=100)