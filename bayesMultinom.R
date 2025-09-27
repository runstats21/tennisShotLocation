# Build Bayesian Multinomial Model
# Estimating 64 parameters
# 4x4 x 2x2
# hit.toxlands x stroke.handxhit.type

# dependencies
library(tidyverse) # for pipes and data wrangling
library(nimble) # for MCMC
library(coda) # for MCMC evaluation
library(ggpubr) # for nice presentation functionality w/ ggplot

# source data cleaning
source("Cleaning.R")

# source cpmap functions
source("MultinomialProbs.R")

adat_returns = adat_clean_simple %>% 
  group_by(rally_num) %>%
  filter(row_number() == 1) %>%
  select(Player,Ball.lands,Ball.hit.to,rally_num)

# now, lets try this with a multinomial of our data dist
#table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="F"])
# data: counts of each zone
fed_hc_zct_data = table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="F"])
nad_hc_zct_data = table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="N"])

multiCode = nimbleCode({
  pi[1:N] ~ ddirch(alpha[1:N]) 
  # note, dirichlet is conjugate with multinomial, which is nice
  y[1:N] ~ dmulti(pi[1:N],n)
})

# define prior parms
# flat prior is 0.25, ..., 0.25 for all 4 alphas
# can set the alphas to weights (don't need to sum to 1, but can for simplicity)
alpha = c(0.3,0.2,0.2,0.3)
# can mess with these priors,
# and see if that makes a difference
# doesn't seem to make a huge difference

#N = 4 # number of classes (zones)
N = length(fed_hc_zct_data)
# get sample sizes
n_fed = sum(fed_hc_zct_data)
n_nad = sum(nad_hc_zct_data)
# define data
fed_mult_data = list(y = as.vector(fed_hc_zct_data))
nad_mult_data = list(y = as.vector(nad_hc_zct_data))
# define constants
fed_consts = list(n=n_fed,N=N,alpha = alpha)
nad_consts = list(n=n_nad,N=N,alpha = alpha)

fed_multi_out = nimbleMCMC(code=multiCode,
                            constants = fed_consts,
                            data = fed_mult_data,
                            nchains = 3, niter = 11000,
                            nburnin=1000, thin = 1,
                            samplesAsCodaMCMC = TRUE,
                            summary = TRUE, WAIC = TRUE,
                            #showCompilerOutput = TRUE,
                            monitors = c('pi'))

nadal_multi_out = nimbleMCMC(code=multiCode,
                              constants = nad_consts,
                              data = nad_mult_data,
                              nchains = 3, niter = 11000,
                              nburnin=1000, thin = 1, # can likely change burn in to 500
                              samplesAsCodaMCMC = TRUE,
                              summary = TRUE, WAIC = TRUE,
                              #showCompilerOutput = TRUE,
                              monitors = c('pi'))

fed_multi_out$summary
fed_multi_out$WAIC
allsamps_fed = as.matrix(fed_multi_out$samples)
raftery.diag(allsamps_fed)
effectiveSize(allsamps_fed)
plot(allsamps_fed[,1],type = 'l')
# par(mfrow = c(2,2))

# plot posterior density distributions for fed
plot(density(allsamps_fed[,1]),lwd = 2, xlim = c(0.1,0.4), ylim = c(0,30),
     col = 1, main = "Federer")
lines(density(allsamps_fed[,2]),lwd = 2,lty = 1,col = 2,)
lines(density(allsamps_fed[,3]),lwd = 2, lty = 1, col = 3)
lines(density(allsamps_fed[,4]),lwd = 2, lty = 1, col = 4)
legend("topright",
       legend = str_c("Z",2:5), col = c(1:4), lty = c(1), lwd = 2)


nadal_multi_out$summary
nadal_multi_out$WAIC
allsamps_nad = as.matrix(nadal_multi_out$samples)
raftery.diag(allsamps_nad)
effectiveSize(allsamps_nad)
plot(allsamps_nadal[,1],type = 'l')
plot(density(allsamps_nad[,1]),lwd = 1)

# nadal posterior dists
plot(density(allsamps_nad[,1]),lwd = 2, xlim = c(0.05,0.55), ylim = c(0,35),
     col = 1, lty =3, main = "Nadal")
lines(density(allsamps_nad[,2]),lwd = 2,lty = 3,col = 2,)
lines(density(allsamps_nad[,3]),lwd = 2, lty = 3, col = 3)
lines(density(allsamps_nad[,4]),lwd = 2, lty = 3, col = 4)
legend("topright",
       legend = str_c("Z",2:5), col = c(1:4), lty = c(3), lwd = 2)



plot(density(allsamps_nad[,1]),lty = 2, lwd = 2, xlim = c(0.05,0.55), ylim = c(0,35),
     col = 1, main = "Posterior Distributions for Both Players")
lines(density(allsamps_fed[,1]),lwd = 2, lty = 1, col = 1)
legend("topleft", legend=c("Fed","Nadal"), col = c(1),
       lty = c(1,3), title="Player", lwd = 2, cex = 0.6)
legend("topright", legend = str_c("Z",2:5), col = c(1:4),
       lty = c(1), lwd = 2, cex = 0.8)
lines(density(allsamps_nad[,2]),lwd = 2,lty = 2,col = 2,)
lines(density(allsamps_fed[,2]),lwd = 2,lty = 1,col = 2)
lines(density(allsamps_nad[,3]),lwd = 2, lty = 2, col = 3)
lines(density(allsamps_fed[,3]),lwd = 2, lty = 1, col = 3)
lines(density(allsamps_nad[,4]),lwd = 2, lty = 2, col = 4)
lines(density(allsamps_fed[,4]),lwd = 2, lty = 1, col = 4)


# Ignoring all other covariates,
# Z3 appears to overlap, but others appear significantly different
# does this change with flat priors?

# Z2
plot(density(allsamps_nad[,1]),lty = 3, lwd = 2, xlim = c(0.15,0.4), ylim = c(0,25),
     col = "red", "Z2 Posterior Distribution")
lines(density(allsamps_fed[,1]), col = "royalblue",lwd = 2)
# Add legend to top right, outside plot region (as shown in stackoverflow post)
legend("topright", legend=c("Fed","Nadal"), col = c("red", "green"),
       lty = c(1,3), title="Player", lwd = 2, cex = 0.8)

# Z3
plot(density(allsamps_nad[,2]),lty = 3, lwd = 2, xlim = c(0.05,0.3), ylim = c(0,35),
     col = "red", "Z3 Posterior Distribution")
lines(density(allsamps_fed[,2]), col = "royalblue",lwd = 2)
# Add legend to top right, outside plot region (as shown in stackoverflow post)
legend("topright", legend=c("Fed","Nadal"), col = c("red", "green"),
       lty = c(1,3), title="Player", lwd = 2, cex = 0.8)

# Z4
plot(density(allsamps_nad[,3]),lty = 3, lwd = 2, xlim = c(0.05,0.25), ylim = c(0,30),
     col = "red", "Z4 Posterior Distribution")
lines(density(allsamps_fed[,3]), col = "royalblue",lwd = 2)
# Add legend to top right, outside plot region (as shown in stackoverflow post)
legend("topright", legend=c("Fed","Nadal"), col = c("red", "green"),
       lty = c(1,3), title="Player", lwd = 2, cex = 0.8)

# Z5
plot(density(allsamps_nad[,4]),lty = 3, lwd = 2, xlim = c(0.05,0.55), ylim = c(0,25),
     col = "red", "Z5 Posterior Distribution")
lines(density(allsamps_fed[,4]), col = "royalblue",lwd = 2)
# Add legend to top right, outside plot region (as shown in stackoverflow post)
legend("topright", legend=c("Fed","Nadal"), col = c("red", "green"),
       lty = c(1,3), title="Player", lwd = 2, cex = 0.8)



# include received location (Ball.lands) ####
# could try hierarchical model
# or can try just splitting up the data by the zones
zct_hr = table(adat_clean_simple[,c("Ball.lands","Ball.hit.to")])
fed_hc_zct_hr = table(adat_clean_simple[adat_clean_simple$Player=="F",
                                        c("Ball.lands","Ball.hit.to")])
nad_hc_zct_hr = table(adat_clean_simple[adat_clean_simple$Player=="N",
                                        c("Ball.lands","Ball.hit.to")])

# include Stroke.Hand
fed_hc_zct_hr_sh = table(adat_clean_simple[adat_clean_simple$Player=="F",
                                        c("Ball.lands","Ball.hit.to","Stroke.Hand")])
nad_hc_zct_hr_sh = table(adat_clean_simple[adat_clean_simple$Player=="N",
                                        c("Ball.lands","Ball.hit.to","Stroke.Hand")])


# try analysis again when recieved from Z2
fed_hc_zct_data = fed_hc_zct_hr["Z2",]
nad_hc_zct_data = nad_hc_zct_hr["Z2",]
#N = 4 # number of classes (zones)
N = length(fed_hc_zct_data)
# get sample sizes
n_fed = sum(fed_hc_zct_data)
n_nad = sum(nad_hc_zct_data)
# define data
fed_mult_data = list(y = as.vector(fed_hc_zct_data))
nad_mult_data = list(y = as.vector(nad_hc_zct_data))
# define constants
fed_consts = list(n=n_fed,N=N,alpha = alpha)
nad_consts = list(n=n_nad,N=N,alpha = alpha)

fed_multi_out = nimbleMCMC(code=multi2Code,
                            constants = fed_consts,
                            data = fed_mult_data,
                            nchains = 3, niter = 11000,
                            nburnin=1000, thin = 1,
                            samplesAsCodaMCMC = TRUE,
                            summary = TRUE, WAIC = TRUE,
                            #showCompilerOutput = TRUE,
                            monitors = c('pi'))

nadal_multi_out = nimbleMCMC(code=multi2Code,
                              constants = nad_consts,
                              data = nad_mult_data,
                              nchains = 3, niter = 11000,
                              nburnin=1000, thin = 1, # can likely change burn in to 500
                              samplesAsCodaMCMC = TRUE,
                              summary = TRUE, WAIC = TRUE,
                              #showCompilerOutput = TRUE,
                              monitors = c('pi'))

fed_multi_out$summary
fed_multi_out$WAIC
allsamps_fedr2 = as.matrix(fed_multi_out$samples)
raftery.diag(allsamps_fedr2)
effectiveSize(allsamps_fedr2)
plot(allsamps_fedr2[,3],type = 'l')
# par(mfrow = c(2,2))

# plot posterior density distributions
plot(density(allsamps_fedr2[,1]),lwd = 2, xlim = c(0.05,0.5), ylim = c(0,30),
     col = 1, main = "Federer")
lines(density(allsamps_fedr2[,2]),lwd = 2,lty = 1,col = 2,)
lines(density(allsamps_fedr2[,3]),lwd = 2, lty = 1, col = 3)
lines(density(allsamps_fedr2[,4]),lwd = 2, lty = 1, col = 4)
legend("topright",
       legend = str_c("Z",2:5), col = c(1:4), lty = c(1), lwd = 2)


nadal_multi_out$summary
nadal_multi_out$WAIC
allsamps_nadr2 = as.matrix(nadal_multi_out$samples)
raftery.diag(allsamps_nadr2)
effectiveSize(allsamps_nadr2)
plot(allsamps_nadr2[,3],type = 'l')
plot(density(allsamps_nadr2[,3]),lwd = 1)

plot(density(allsamps_nadr2[,1]),lty = 2, lwd = 2, xlim = c(0.05,0.55), ylim = c(0,20),
     col = 1, main = "Posterior Distributions for Both Players (Received Z2)")
lines(density(allsamps_fedr2[,1]),lwd = 2, lty = 1, col = 1)
legend("topleft", legend=c("Fed","Nadal"), col = c(1),
       lty = c(1,3), title="Player", lwd = 2, cex = 0.7)
legend("topright", legend = str_c("Z",2:5), col = c(1:4),
       lty = c(1), lwd = 2, cex = 0.8)
lines(density(allsamps_nadr2[,2]),lwd = 2,lty = 2,col = 2,)
lines(density(allsamps_fedr2[,2]),lwd = 2,lty = 1,col = 2)
lines(density(allsamps_nadr2[,3]),lwd = 2, lty = 2, col = 3)
lines(density(allsamps_fedr2[,3]),lwd = 2, lty = 1, col = 3)
lines(density(allsamps_nadr2[,4]),lwd = 2, lty = 2, col = 4)
lines(density(allsamps_fedr2[,4]),lwd = 2, lty = 1, col = 4)
# legend("topright", legend = str_c("Z",2:5), col = c(1:4),
#        lty = c(1), lwd = 2, cex = 0.8)
# legend("topleft", legend=c("Fed","Nadal"), col = c(1),
#        lty = c(1,3), title="Player", lwd = 2, cex = 0.7)


# build this work into a function
# try analysis again when recieved from Z2
fed_nad_post_plot = function(fed_dat,nad_dat,plot_title_add,
                             n_chains = 3) {
  require(nimble)
  multiCode = nimbleCode({
    pi[1:N] ~ ddirch(alpha[1:N]) 
    # note, dirichlet is conjugate with multinomial, which is nice
    y[1:N] ~ dmulti(pi[1:N],n)
  })
  
  fed_hc_zct_data = fed_dat
  nad_hc_zct_data = nad_dat
  #N = 4 # number of classes (zones)
  N = length(fed_hc_zct_data)
  # get sample sizes
  n_fed = sum(fed_hc_zct_data)
  n_nad = sum(nad_hc_zct_data)
  # define data
  fed_mult_data = list(y = as.vector(fed_hc_zct_data))
  nad_mult_data = list(y = as.vector(nad_hc_zct_data))
  # define constants
  fed_consts = list(n=n_fed,N=N,alpha = alpha)
  nad_consts = list(n=n_nad,N=N,alpha = alpha)
  
  fed_multi_out = nimbleMCMC(code=multiCode,
                              constants = fed_consts,
                              data = fed_mult_data,
                              nchains = n_chains, niter = 11000,
                              nburnin=1000, thin = 1,
                              samplesAsCodaMCMC = TRUE,
                              summary = TRUE, WAIC = TRUE,
                              #showCompilerOutput = TRUE,
                              monitors = c('pi'))
  
  nadal_multi_out = nimbleMCMC(code=multiCode,
                                constants = nad_consts,
                                data = nad_mult_data,
                                nchains = n_chains, niter = 11000,
                                nburnin=1000, thin = 1, # can likely change burn in to 500
                                samplesAsCodaMCMC = TRUE,
                                summary = TRUE, WAIC = TRUE,
                                #showCompilerOutput = TRUE,
                                monitors = c('pi'))
  
  fed_multi_out$summary
  fed_multi_out$WAIC
  allsamps_fedr2 = as.matrix(fed_multi_out$samples)
  raftery.diag(allsamps_fedr2)
  effectiveSize(allsamps_fedr2)
  plot(allsamps_fedr2[,3],type = 'l')
  # par(mfrow = c(2,2))
  
  # plot posterior density distributions
  plot(density(allsamps_fedr2[,1]),lwd = 2, xlim = c(0.05,0.5), ylim = c(0,30),
       col = 1, main = "Federer")
  lines(density(allsamps_fedr2[,2]),lwd = 2,lty = 1,col = 2,)
  lines(density(allsamps_fedr2[,3]),lwd = 2, lty = 1, col = 3)
  lines(density(allsamps_fedr2[,4]),lwd = 2, lty = 1, col = 4)
  legend("topright",
         legend = str_c("Z",2:5), col = c(1:4), lty = c(1), lwd = 2)
  
  
  nadal_multi_out$summary
  nadal_multi_out$WAIC
  allsamps_nadr2 = as.matrix(nadal_multi_out$samples)
  raftery.diag(allsamps_nadr2)
  effectiveSize(allsamps_nadr2)
  plot(allsamps_nadr2[,3],type = 'l')
  plot(density(allsamps_nadr2[,3]),lwd = 1)
  
  plot(density(allsamps_nadr2[,1]),lty = 2, lwd = 2, xlim = c(0.05,0.65), ylim = c(0,20),
       col = 1, main = paste0("Posterior Distributions for Both Players ",plot_title_add))
  lines(density(allsamps_fedr2[,1]),lwd = 2, lty = 1, col = 1)
  legend("topleft", legend=c("Fed","Nadal"), col = c(1),
         lty = c(1,3), title="Player", lwd = 2, cex = 0.7)
  legend("topright", legend = str_c("Z",2:5), col = c(1:4),
         lty = c(1), lwd = 2, cex = 0.8)
  lines(density(allsamps_nadr2[,2]),lwd = 2,lty = 2,col = 2,)
  lines(density(allsamps_fedr2[,2]),lwd = 2,lty = 1,col = 2)
  lines(density(allsamps_nadr2[,3]),lwd = 2, lty = 2, col = 3)
  lines(density(allsamps_fedr2[,3]),lwd = 2, lty = 1, col = 3)
  lines(density(allsamps_nadr2[,4]),lwd = 2, lty = 2, col = 4)
  lines(density(allsamps_fedr2[,4]),lwd = 2, lty = 1, col = 4)
  # legend("topright", legend = str_c("Z",2:5), col = c(1:4),
  #        lty = c(1), lwd = 2, cex = 0.8)
  # legend("topleft", legend=c("Fed","Nadal"), col = c(1),
  #        lty = c(1,3), title="Player", lwd = 2, cex = 0.7)
}

fed_nad_post_plot(fed_dat = fed_hc_zct_hr["Z3",],
                  nad_dat = nad_hc_zct_hr["Z3",],
                  plot_title_add = "(Received Z3)",
                  n_chains = 5)

# this takes approximately 60-90 seconds to run?


# try to make a hierarchical model within nimble ####
# add hierarchical piece for different players
# hard court data first
y = table(adat_clean_simple$Ball.hit.to)
y_fed = table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="F"])
y_nad = table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="N"])
N = length(y)
n = sum(y)
n1 = sum(y_fed)
n2 = sum(y_nad)
# n==n1+n2 # sample sizes add to total? Yes

alpha = c(0.25,0.25,0.25,0.25)
# can mess with these priors,
# and see if that makes a difference
# doesn't seem to make a huge 

# define initial values
piinit = c(.28,.18,.18,.36) # sort of empirical bayes with these inits
u1init = c(0,0,0,0)
u2init = c(0,0,0,0)
# interestingly, without initial values, some of the us are huge
# and some of the pis are small and even negative
ssubinit = 0.1

multi_hcp_Code1= nimbleCode({
  pi[1:N] ~ ddirch(alpha[1:N])
  y[1:N] ~ dmulti(pi[1:N],n)
  
  ssub ~ dunif(0,0.3)
  # first try using binomials and u random effects:
  # works with initial values
  for (i in 1:(N)){
    y1[i] ~ dbin(pi1[i],n1)
    u1[i] ~ dnorm(0,sd=ssub) # random effect for player 1
    pi1[i] <- pi[i] + u1[i]

    y2[i] ~ dbin(pi2[i],n2)
    u2[i] ~ dnorm(0,sd=ssub)
    pi2[i] <- pi[i] + u2[i]
  }
  # pi1[1:N] ~ ddirch(pi[1:N])
  # y1[1:N] ~ dmulti(pi1[1:N],n1)
  # 
  # pi2[1:N] ~ ddirch(pi[1:N])
  # y2[1:N] ~ dmulti(pi2[1:N],n2)
  # this was takes quite longer
})

multi_hcp_Code= nimbleCode({
  pi[1:N] ~ ddirch(alpha[1:N])
  y[1:N] ~ dmulti(pi[1:N],n)
  
  #ssub ~ dunif(0,0.3)
  # first try using binomials and u random effects:
  # works with initial values
  # for (i in 1:(N)){
  #   y1[i] ~ dbin(pi1[i],n1)
  #   u1[i] ~ dnorm(0,sd=ssub) # random effect for player 1
  #   pi1[i] <- pi[i] + u1[i]
  #   
  #   y2[i] ~ dbin(pi2[i],n2)
  #   u2[i] ~ dnorm(0,sd=ssub)
  #   pi2[i] <- pi[i] + u2[i]
  # }
  pi1[1:N] ~ ddirch(pi[1:N])
  y1[1:N] ~ dmulti(pi1[1:N],n1)
  
  pi2[1:N] ~ ddirch(pi[1:N])
  y2[1:N] ~ dmulti(pi2[1:N],n2)
  # this was takes quite longer
})

multi_hcp_data = list(y=y,y1=y_fed,y2=y_nad)
multi_hcp_consts = list(n=n,n1=n1,n2=n2,N=N,alpha = alpha)
multi_hcp_inits = list(pi=piinit,u1=u1init,u2=u2init,ssub=ssubinit)

multi_hcp_out1 = nimbleMCMC(code=multi_hcp_Code1,
                        constants = multi_hcp_consts,
                        data = multi_hcp_data,
                        # modeling player spec data with binomials 
                        # requires initial values
                        inits = multi_hcp_inits, 
                        nchains = 5, niter = 21000,
                        nburnin=1000, thin = 4,
                        samplesAsCodaMCMC = TRUE,
                        summary = TRUE, WAIC = TRUE,
                        #showCompilerOutput = TRUE,
                        monitors = c('pi','pi1','pi2','u1','u2','ssub'))

multi_hcp_out = nimbleMCMC(code=multi_hcp_Code,
                        constants = multi_hcp_consts,
                        data = multi_hcp_data,
                        # modeling player spec data with binomials 
                        # requires initial values
                        #inits = multi_hcp_inits, 
                        nchains = 5, niter = 22000,
                        nburnin=2000, thin = 4,
                        samplesAsCodaMCMC = TRUE,
                        summary = TRUE, WAIC = TRUE,
                        #showCompilerOutput = TRUE,
                        monitors = c('pi','pi1','pi2'))#'u1','u2','ssub'))

# compare estimates (binomial vs dirichlet)
cbind(
multi_hcp_out1$summary$all.chains%>% round(4) %>% .[1:12,1],
multi_hcp_out$summary$all.chains %>% round(4) %>% .[,1])
# results are fairly comparable
multi_hcp_out$WAIC
allsamps_hcp = as.matrix(multi_hcp_out$samples)
colnames(allsamps_hcp)
dim(allsamps_hcp)
raftery.diag(allsamps_hcp)
effectiveSize(allsamps_hcp)

plot(allsamps_hcp[,5],type = 'l') # appears some more burn in may be beneficial
plot(allsamps_hcp[,6],type = 'l')
plot(allsamps_hcp[,9],type = 'l')
acf(allsamps_hcp[,8])
# overall probabilities (posterior densities)
# compared with player-specific posteriors

# Z2
z2hc = ggplot(data = as.data.frame(allsamps_hcp))+
  stat_density(aes(x = `pi[1]`,col = "Overall"),lty = 2)+
  stat_density(aes(x = `pi1[1]`,col="Federer"))+
  stat_density(aes(x = `pi2[1]`,col = "Nadal"))+
  scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                     values = c("Overall"="black","Federer"="red","Nadal"="green"))+
  theme_minimal()+
  xlim(c(0.05,0.55))+
  labs(x = "Z2")

z3hc = ggplot(data = as.data.frame(allsamps_hcp))+
  stat_density(aes(x = `pi[2]`,col = "Overall"),lty = 2)+
  stat_density(aes(x = `pi1[2]`,col="Federer"))+
  stat_density(aes(x = `pi2[2]`,col = "Nadal"))+
  scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                     values = c("Overall"="black","Federer"="red","Nadal"="green"))+
  theme_minimal()+
  xlim(c(0.05,0.55))+
  labs(x = "Z3")

z4hc = ggplot(data = as.data.frame(allsamps_hcp))+
  stat_density(aes(x = `pi[3]`,col = "Overall"),lty = 2)+
  stat_density(aes(x = `pi1[3]`,col="Federer"))+
  stat_density(aes(x = `pi2[3]`,col = "Nadal"))+
  scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                     values = c("Overall"="black","Federer"="red","Nadal"="green"))+
  theme_minimal()+
  xlim(c(0.05,0.55))+
  labs(x = "Z4")

z5hc = ggplot(data = as.data.frame(allsamps_hcp))+
  stat_density(aes(x = `pi[4]`,col = "Overall"),lty = 2)+
  stat_density(aes(x = `pi1[4]`,col="Federer"))+
  stat_density(aes(x = `pi2[4]`,col = "Nadal"))+
  scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                     values = c("Overall"="black","Federer"="red","Nadal"="green"))+
  theme_minimal()+
  xlim(c(0.05,0.55))+
  labs(x = "Z5")

hm_plots = ggpubr::ggarrange(z2hc,z3hc,z4hc,z5hc,common.legend = T,
                             label.y = "density",align = "hv",
                             legend = "bottom")
annotate_figure(hm_plots,
                        top = "Posterior distributions from Hierarchical Model")

par(mfrow = c(2,2))
plot(density(allsamps_hcp[,"pi[1]"]),lwd = 1, xlim = c(0.1,0.6),
     main = "Z2 posteriors")
lines(density(allsamps_hcp[,"pi1[1]"]), col = "red",lty = 2,lwd = 2)
lines(density(allsamps_hcp[,"pi2[1]"]), col = "green",lty = 2,lwd = 2)
legend("topright", legend = c("Overall","Fed","Nadal"),
       lty = c(1,3,3), col = c("black","red","green"),
       lwd = c(1,2,2), cex = 0.85)

# Z3
plot(density(allsamps_hcp[,"pi[2]"]),lwd = 1, xlim = c(0.05,0.35),
     main = "Z3 posteriors")
lines(density(allsamps_hcp[,"pi1[2]"]), col = "red",lty = 2,lwd = 2)
lines(density(allsamps_hcp[,"pi2[2]"]), col = "green",lty = 2,lwd = 2)
legend("topright", legend = c("Overall","Fed","Nadal"),
       lty = c(1,3,3), col = c("black","red","green"),
       lwd = c(1,2,2), cex = 0.85)

# Z4
plot(density(allsamps_hcp[,"pi[3]"]),lwd = 1, xlim = c(0.05,0.35),
     main = "Z4 posteriors")
lines(density(allsamps_hcp[,"pi1[3]"]), col = "red",lty = 2,lwd = 2)
lines(density(allsamps_hcp[,"pi2[3]"]), col = "green",lty = 2,lwd = 2)
legend("topright", legend = c("Overall","Fed","Nadal"),
       lty = c(1,3,3), col = c("black","red","green"),
       lwd = c(1,2,2), cex = 0.85)


# define this hm as a function
fed_nad_hm_post = function(y,y_fed,y_nad,
                           n_chains=5,prior_vals = c(0.25,0.25,0.25,0.25),
                           plot_title_add = "",plot_xlims = c(0.05,0.55)) {
  require(tidyverse)
  require(ggpubr)
  N = length(y)
  n = sum(y)
  n1 = sum(y_fed)
  n2 = sum(y_nad)
  # n==n1+n2 # sample sizes add to total? Yes
  
  alpha = prior_vals
  # can mess with these priors,
  # and see if that makes a difference
  # doesn't seem to make a huge 
  
  # define initial values
  piinit = c(.28,.18,.18,.36) # sort of empirical bayes with these inits
  u1init = c(0,0,0,0)
  u2init = c(0,0,0,0)
  # interestingly, without initial values, some of the us are huge
  # and some of the pis are small and even negative
  ssubinit = 0.1
  
  multi_hcp_Code= nimbleCode({
    pi[1:N] ~ ddirch(alpha[1:N])
    y[1:N] ~ dmulti(pi[1:N],n)
    ssub ~ dunif(0,0.3)
    for (i in 1:(N)){
      y1[i] ~ dbin(pi1[i],n1)
      u1[i] ~ dnorm(0,sd=ssub) # random effect for player 1
      pi1[i] <- pi[i] + u1[i]
      # equivalently: pi1[i] ~ dnorm(pi[i],sd = ssub)
      
      
      y2[i] ~ dbin(pi2[i],n2)
      u2[i] ~ dnorm(0,sd=ssub)
      pi2[i] <- pi[i] + u2[i]
      # pi1[i] ~ dnorm(pi[i],sd = ssub)
    }
  })
  
  multi_hcp_data = list(y=y,y1=y_fed,y2=y_nad)
  multi_hcp_consts = list(n=n,n1=n1,n2=n2,N=N,alpha = alpha)
  multi_hcp_inits = list(pi=piinit,u1=u1init,u2=u2init,ssub=ssubinit)
  
  multi_hcp_out = nimbleMCMC(code=multi_hcp_Code,
                          constants = multi_hcp_consts,
                          data = multi_hcp_data,
                          #inits = multi_hcp_inits,
                          nchains = n_chains, niter = 21000,
                          nburnin=1000, thin = 4,
                          samplesAsCodaMCMC = TRUE,
                          summary = TRUE, WAIC = TRUE,
                          #showCompilerOutput = TRUE,
                          monitors = c('pi','pi1','pi2','u1','u2','ssub'))
  
  multi_hcp_out$summary$all.chains %>% round(4)
  multi_hcp_out$WAIC
  allsamps_hcp = as.matrix(multi_hcp_out$samples)
  colnames(allsamps_hcp)
  dim(allsamps_hcp)
  raftery.diag(allsamps_hcp)
  effectiveSize(allsamps_hcp)
  
  # plot(allsamps_hcp[,5],type = 'l')
  # plot(allsamps_hcp[,6],type = 'l')
  # plot(allsamps_hcp[,"ssub"],type = 'l')
  # overall probabilities (posterior densities)
  # compared with player-specific posteriors
  
  # Z2
  z2hc = ggplot(data = as.data.frame(allsamps_hcp))+
    stat_density(aes(x = `pi[1]`,col = "Overall"),lty = 2,geom = "line")+
    stat_density(aes(x = `pi1[1]`,col="Federer"),geom = "line")+
    stat_density(aes(x = `pi2[1]`,col = "Nadal"),geom = "line")+
    scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                       values = c("Overall"="black","Federer"="red","Nadal"="green"))+
    theme_minimal()+
    xlim(plot_xlims)+
    labs(x = "Z2")
  
  z3hc = ggplot(data = as.data.frame(allsamps_hcp))+
    stat_density(aes(x = `pi[2]`,col = "Overall"),lty = 2,geom = "line")+
    stat_density(aes(x = `pi1[2]`,col="Federer"),geom = "line")+
    stat_density(aes(x = `pi2[2]`,col = "Nadal"),geom = "line")+
    scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                       values = c("Overall"="black","Federer"="red","Nadal"="green"))+
    theme_minimal()+
    xlim(plot_xlims)+
    labs(x = "Z3")
  
  z4hc = ggplot(data = as.data.frame(allsamps_hcp))+
    stat_density(aes(x = `pi[3]`,col = "Overall"),lty = 2,geom = "line")+
    stat_density(aes(x = `pi1[3]`,col="Federer"),geom = "line")+
    stat_density(aes(x = `pi2[3]`,col = "Nadal"),geom = "line")+
    scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                       values = c("Overall"="black","Federer"="red","Nadal"="green"))+
    theme_minimal()+
    xlim(plot_xlims)+
    labs(x = "Z4")
  
  z5hc = ggplot(data = as.data.frame(allsamps_hcp))+
    stat_density(aes(x = `pi[4]`,col = "Overall"),lty = 2,geom = "line")+
    stat_density(aes(x = `pi1[4]`,col="Federer"),geom = "line")+
    stat_density(aes(x = `pi2[4]`,col = "Nadal"),geom = "line")+
    scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                       values = c("Overall"="black","Federer"="red","Nadal"="green"))+
    theme_minimal()+
    xlim(plot_xlims)+
    labs(x = "Z5")
  
  hm_plots = ggpubr::ggarrange(z2hc,z3hc,z4hc,z5hc,common.legend = T,
                               label.y = "density",align = "hv",
                               legend = "bottom")
  final_plot = annotate_figure(hm_plots,
                               top = paste("Posterior distributions from Hierarchical Model",
                                           plot_title_add))
  
  return(list(plot = final_plot,
              hm_out = multi_hcp_out,
              samples = as.matrix(multi_hcp_out$samples)))
}

no_r_results = fed_nad_hm_post(y = table(adat_clean_simple$Ball.hit.to),
                               y_fed = table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="F"]),
                               y_nad = table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="N"]))

no_r_results$plot
no_r_results$hm_out$summary$all.chains

zct_hr = table(adat_clean_simple[,c("Ball.lands","Ball.hit.to")])
fed_zct_hr = table(adat_clean_simple[adat_clean_simple$Player=="F",
                                     c("Ball.lands","Ball.hit.to")])
nad_zct_hr = table(adat_clean_simple[adat_clean_simple$Player=="N",
                                     c("Ball.lands","Ball.hit.to")])

rz2_hmp_results = fed_nad_hm_post(y = zct_hr["Z2",],
                                  y_fed = fed_zct_hr["Z2",],
                                  y_nad = nad_zct_hr["Z2",],
                                  plot_title_add = "(Received Z2)")
rz2_hmp_results$plot
rz2_hmp_results$hm_out$summary$all.chains
# plot(rz2_hmp_results$hm_out$samples[,"ssub"])

rz3_hmp_results = fed_nad_hm_post(y = zct_hr["Z3",],
                                  y_fed = fed_zct_hr["Z3",],
                                  y_nad = nad_zct_hr["Z3",],
                                  plot_title_add = "(Received Z3)",
                                  plot_xlims = c(0,0.65))
rz3_hmp_results$plot
rz3_hmp_results$hm_out$summary$all.chains

# will likely need to adjust bounds for this plot
rz4_hmp_results = fed_nad_hm_post(y = zct_hr["Z4",],
                                  y_fed = fed_zct_hr["Z4",],
                                  y_nad = nad_zct_hr["Z4",],
                                  plot_title_add = "(Received Z4)",
                                  plot_xlims = c(0.05,0.7))
rz4_hmp_results$plot
rz4_hmp_results$hm_out$summary$all.chains

rz5_hmp_results = fed_nad_hm_post(y = zct_hr["Z5",],
                                  y_fed = fed_zct_hr["Z5",],
                                  y_nad = nad_zct_hr["Z5",],
                                  plot_title_add = "(Received Z5)",
                                  plot_xlims = c(0,0.7))
rz5_hmp_results$plot
rz5_hmp_results$hm_out$summary$all.chains

# loop #####
# through all recieved from zones 
# then through the 3 surfaces
# data
# hc
zct_hc_hr = table(adat_clean_simple[,c("Ball.lands","Ball.hit.to")])
fed_hc_zct_hr = table(adat_clean_simple[adat_clean_simple$Player=="F",
                                        c("Ball.lands","Ball.hit.to")])
nad_hc_zct_hr = table(adat_clean_simple[adat_clean_simple$Player=="N",
                                        c("Ball.lands","Ball.hit.to")])
samps_cur = matrix()
dim(as.matrix(res$samples))
colnames(as.matrix(res$samples))
cbind(samps_cur,as.matrix(res$samples))

for (r in 2:5) {
  res = fed_nad_hm_post(y = zct__hc_hr[paste0("Z",r),],
                  y_fed = fed_hc_zct_hr[paste0("Z",r),],
                  y_nad = nad_hc_zct_hr[paste0("Z",r),])
  samps_cur = as.matrix(res$samples)
  print(paste0("Received in Z",r," MCMC complete"))
}
# continued in bayesMultinomLoop.R

# see results based on stroke hand and hit type ####
# stroke hand
zct_stroke_hand = table(adat_clean_simple[,c("Stroke.Hand","Ball.hit.to")])
fed_zct_stroke_hand = table(adat_clean_simple[adat_clean_simple$Player=="F",
                                     c("Stroke.Hand","Ball.hit.to")])
nad_zct_stroke_hand = table(adat_clean_simple[adat_clean_simple$Player=="N",
                                     c("Stroke.Hand","Ball.hit.to")])

# hit type
zct_hit_type = table(adat_clean_simple[,c("Hit.Type","Ball.hit.to")])
fed_zct_hit_type = table(adat_clean_simple[adat_clean_simple$Player=="F",
                                     c("Hit.Type","Ball.hit.to")])
nad_zct_hit_type = table(adat_clean_simple[adat_clean_simple$Player=="N",
                                     c("Hit.Type","Ball.hit.to")])

allsamps_srp = readRDS(file = "./saved_data/allsamps_srploop.Rdata")
# plot posteriors and heat map of bayes estimates (together) ####
# overall
overall_fitted = round(c(0,no_r_results$hm_out$summary$all.chains[,1][1:4],0),2)
fed_fitted = round(c(0,no_r_results$hm_out$summary$all.chains[,1][5:8],0),2)
nad_fitted = round(c(0,no_r_results$hm_out$summary$all.chains[,1][9:12],0),2)
# can add results for further split (by received location)
# e.g., z2
rz2_fitted = round(c(0,rz2_hmp_results$hm_out$summary$all.chains[,1][1:4],0),2)
fed_rz2_fitted = round(c(0,rz2_hmp_results$hm_out$summary$all.chains[,1][5:8],0),2)
nad_rz2_fitted = round(c(0,rz2_hmp_results$hm_out$summary$all.chains[,1][9:12],0),2)

# z3
rz3_fitted = round(c(0,rz3_hmp_results$hm_out$summary$all.chains[,1][1:4],0),2)
fed_rz3_fitted = round(c(0,rz3_hmp_results$hm_out$summary$all.chains[,1][5:8],0),2)
nad_rz3_fitted = round(c(0,rz3_hmp_results$hm_out$summary$all.chains[,1][9:12],0),2)

# z4
rz4_fitted = round(c(0,rz4_hmp_results$hm_out$summary$all.chains[,1][1:4],0),2)
fed_rz4_fitted = round(c(0,rz4_hmp_results$hm_out$summary$all.chains[,1][5:8],0),2)
nad_rz4_fitted = round(c(0,rz4_hmp_results$hm_out$summary$all.chains[,1][9:12],0),2)

# z5
rz5_fitted = round(c(0,rz5_hmp_results$hm_out$summary$all.chains[,1][1:4],0),2)
fed_rz5_fitted = round(c(0,rz5_hmp_results$hm_out$summary$all.chains[,1][5:8],0),2)
nad_rz5_fitted = round(c(0,rz5_hmp_results$hm_out$summary$all.chains[,1][9:12],0),2)

# get bayes estimates from all samps
s1_rz5_fitted = round(c(0,colMeans(allsamps_srp[,c("1rZ5_pi[1]","1rZ5_pi[2]","1rZ5_pi[3]","1rZ5_pi[4]")]),0),2)
s1_fed_rz5_fitted = round(c(0,colMeans(allsamps_srp[,c("1rZ5_pi1[1]","1rZ5_pi1[2]","1rZ5_pi1[3]","1rZ5_pi1[4]")]),0),2)
s1_nad_rz5_fitted = round(c(0,colMeans(allsamps_srp[,c("1rZ5_pi2[1]","1rZ5_pi2[2]","1rZ5_pi2[3]","1rZ5_pi2[4]")]),0),2)

# all posteriors for both players, with heat maps on right side
ggarrange(no_r_results$plot,
          ggarrange(court_cpmap(fed_fitted)+labs(title="Federer"),
                    court_cpmap(nad_fitted)+labs(title="Nadal"),nrow = 2))

ggarrange(rz2_hmp_results$plot,
          ggarrange(court_cpmap(fed_rz2_fitted)+labs(title="Federer"),
                    court_cpmap(nad_rz2_fitted)+labs(title="Nadal"),nrow = 2))

ggarrange(rz5_hmp_results$plot,
          ggarrange(court_cpmap(fed_rz5_fitted)+labs(title="Federer"),
                    court_cpmap(nad_rz5_fitted)+labs(title="Nadal"),nrow = 2))

# try posteriors for players directly next to heat maps
# plot posterior density distributions
plot_4post_dists = function(allsamps_cur,parms4,
                            plot_title,plot_xlims,
                            legend_labels,x_label = "P(shot hit to given location)") {
  ggplot(data = as.data.frame(allsamps_cur))+
    stat_density(aes(x = allsamps_cur[,parms4[1]],col = "Z2"),lwd = 1.1, geom = "line")+
    stat_density(aes(x = allsamps_cur[,parms4[2]],col="Z3"),lwd = 1.1, geom = "line")+
    stat_density(aes(x = allsamps_cur[,parms4[3]],col = "Z4"),lwd = 1.1, geom = "line")+
    stat_density(aes(x = allsamps_cur[,parms4[4]],col = "Z5"),lwd = 1.1, geom = "line")+
    scale_color_manual(name=NULL,breaks = legend_labels,
                       values = c(1:4))+
    theme_minimal()+
    xlim(plot_xlims)+
    labs(title = plot_title, x = x_label)+
    theme(plot.title = element_text(hjust = 0.5)) # center title
  
  # base R version
  # plot(density(allsamps_cur[,parms4[1]]),lwd = 2, xlim = c(0.05,0.5), ylim = c(0,30),
  #      col = 1, main = paste(plot_title))
  # lines(density(allsamps_cur[,parms4[2]]),lwd = 2,lty = 1,col = 2,)
  # lines(density(allsamps_cur[,parms4[3]]),lwd = 2, lty = 1, col = 3)
  # lines(density(allsamps_cur[,parms4[4]]),lwd = 2, lty = 1, col = 4)
  # legend("topright",
  #        legend = legend_labels, col = c(1:4), lty = c(1), lwd = 2,cex = 0.85)
}

posts_and_heatmaps = function(allsamps_srp,setting,title) {
  fed_posts = plot_4post_dists(allsamps_cur = allsamps_srp,
                   parms4 = str_c(setting,"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]")),
                   legend_labels = str_c("Z",2:5), plot_xlims = c(0.0,0.65),
                   plot_title = "Federer posterior distributions")
  nad_posts = plot_4post_dists(allsamps_cur = allsamps_srp,
                   parms4 = str_c(setting,"_",c("pi2[1]","pi2[2]","pi2[3]","pi2[4]")),
                   legend_labels = str_c("Z",2:5), plot_xlims = c(0.0,0.65),
                   plot_title = "Nadal posterior distributions")
  
  fed_fitted = round(c(0,colMeans(allsamps_srp[,str_c(setting,"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"))]),0),2)
  nad_fitted = round(c(0,colMeans(allsamps_srp[,str_c(setting,"_",c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"))]),0),2)
  
  # plotting post for players directly side by side
  ggarrange(ggarrange(fed_posts,nad_posts,ncol =2,legend = "bottom",
                    common.legend = T),
            ggarrange(court_cpmap(fed_fitted,cp_lims = c(0,0.57))+
                        labs(title="Federer estimates"),
                      court_cpmap(nad_fitted,cp_lims = c(0,0.57))+
                        labs(title="Nadal estimates"),ncol = 2,
                      common.legend = T,legend="bottom"),
            nrow = 2) %>% annotate_figure(top = paste0("Bayes Estimates: ",title))
}

posts_and_heatmaps(allsamps_srp,setting = "1rZ5",title = "Hard Court Received Z5")


# z2 example
ggarrange(ggarrange(plot_4post_dists(as.matrix(rz2_hmp_results$hm_out$samples),
                 parms4 = c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"),
                 legend_labels = str_c("Z",2:5), plot_xlims = c(0.05,0.55),
                 plot_title = "Federer posterior distributions"),
                 
                 plot_4post_dists(as.matrix(rz2_hmp_results$hm_out$samples),
                 parms4 = c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"),
                 legend_labels = str_c("Z",2:5), plot_xlims = c(0.05,0.55),
                 plot_title = "Nadal posterior distributions"),legend = "bottom",
                 common.legend = T),
          ggarrange(court_cpmap(fed_rz2_fitted)+labs(title="Federer estimates"),
          court_cpmap(nad_rz2_fitted)+labs(title="Nadal estimates"),ncol = 2),
          nrow = 2) %>% 
  annotate_figure(top = "Recieved Z2")

# z3 example
ggarrange(ggarrange(plot_4post_dists(as.matrix(rz3_hmp_results$hm_out$samples),
                 parms4 = c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"),
                 legend_labels = str_c("Z",2:5), plot_xlims = c(0.05,0.55),
                 plot_title = "Federer posterior distributions"),
                 
                 plot_4post_dists(as.matrix(rz3_hmp_results$hm_out$samples),
                 parms4 = c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"),
                 legend_labels = str_c("Z",2:5), plot_xlims = c(0.05,0.55),
                 plot_title = "Nadal posterior distributions"),legend = "bottom",
                 common.legend = T),
          ggarrange(court_cpmap(fed_rz3_fitted)+labs(title="Federer estimates"),
          court_cpmap(nad_rz3_fitted)+labs(title="Nadal estimates"),ncol = 2),
          nrow = 2) %>% 
  annotate_figure(top = "Recieved Z3")

# z4 example
ggarrange(ggarrange(plot_4post_dists(as.matrix(rz4_hmp_results$hm_out$samples),
                 parms4 = c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"),
                 legend_labels = str_c("Z",2:5), plot_xlims = c(0,0.65),
                 plot_title = "Federer posterior distributions"),
                 
                 plot_4post_dists(as.matrix(rz4_hmp_results$hm_out$samples),
                 parms4 = c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"),
                 legend_labels = str_c("Z",2:5), plot_xlims = c(0,0.65),
                 plot_title = "Nadal posterior distributions"),legend = "bottom",
                 common.legend = T),
          ggarrange(court_cpmap(fed_rz4_fitted)+labs(title="Federer estimates"),
          court_cpmap(nad_rz4_fitted)+labs(title="Nadal estimates"),ncol = 2),
          nrow = 2) %>% 
  annotate_figure(top = "Recieved Z4")

# z5 example
ggarrange(ggarrange(plot_4post_dists(as.matrix(rz5_hmp_results$hm_out$samples),
                 parms4 = c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"),
                 legend_labels = str_c("Z",2:5), plot_xlims = c(0,0.65),
                 plot_title = "Federer posterior distributions"),
                 
                 plot_4post_dists(as.matrix(rz5_hmp_results$hm_out$samples),
                 parms4 = c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"),
                 legend_labels = str_c("Z",2:5), plot_xlims = c(0,0.65),
                 plot_title = "Nadal posterior distributions"),
                 legend = "bottom",common.legend = T),
          ggarrange(court_cpmap(fed_rz5_fitted)+labs(title="Federer estimates"),
          court_cpmap(nad_rz5_fitted)+labs(title="Nadal estimates"),ncol = 2),
          nrow = 2) %>% 
  annotate_figure(top = "Recieved Z5")
          

# run for only returns ####
zct_returns_hr = table(adat_returns[,c("Ball.lands","Ball.hit.to")])
zct_returns_nad = table(adat_returns[adat_returns$Player=="N",c("Ball.lands","Ball.hit.to")])
zct_returns_fed = table(adat_returns[adat_returns$Player=="F",c("Ball.lands","Ball.hit.to")])
# good starting point here, but could be expanded
# (some returns not included, likely those that were an ACE,
# or returned out, or removed for some other reason)

rz2_returns_hmp_res = fed_nad_hm_post(y = zct_returns_hr["Z2",],
                                      y_fed = zct_returns_fed["Z2",],
                                      y_nad = zct_returns_nad["Z2",],
                                      plot_title_add = "(Received Z2, Returns Only)",
                                      plot_xlims = c(0,0.6))
rz2_returns_hmp_res$plot

rz5_returns_hmp_res = fed_nad_hm_post(y = zct_returns_hr["Z5",],
                                      y_fed = zct_returns_fed["Z5",],
                                      y_nad = zct_returns_nad["Z5",],
                                      plot_title_add = "(Received Z5, Returns Only)",
                                      plot_xlims = c(0,0.7))
rz5_returns_hmp_res$plot
# could automate further, giving only datasets and zone name string


# try surface fixed effect (first ignoring player) ####
# surfaces 1,2,3 = hc, clay, and grass, respectively
# totals
y_s1 = table(adat_clean_simple[,c("Ball.hit.to")])
y_s2 = table(fdat_clean_simple[,c("Ball.hit.to")])
y_s3 = table(wdat_clean_simple[,c("Ball.hit.to")])

# split up by received location
y_s1_hr = table(adat_clean_simple[,c("Ball.lands","Ball.hit.to")])
y_s2_hr = table(fdat_clean_simple[,c("Ball.lands","Ball.hit.to")])
y_s3_hr = table(wdat_clean_simple[,c("Ball.lands","Ball.hit.to")])


y = y_s1+y_s2+y_s3
n = sum(y_s1,y_s2,y_s3)
n1 = sum(y_s1)
n2 = sum(y_s2)
n3 = sum(y_s3)
# peak at sd to inform priors
sd(y_s1/n1)
sd(y_s2/n2)
sd(y_s3/n3)

N = 4
alpha = c(0.25,0.25,0.25,0.25)
# can mess with these priors,
# and see if that makes a difference
# doesn't seem to make a huge 

# define initial values
piinit = c(.41,.14,.27,.18)
# u1init = c(0,0,0,0)
# u2init = c(0,0,0,0)
# ssubinit = 0.1

multi_sf_Code= nimbleCode({
  pi[1:N] ~ ddirch(alpha[1:N])
  y[1:N] ~ dmulti(pi[1:N],n)
  #ssub ~ dunif(0,0.3)
  for (i in 1:(N)){
    y1[i] ~ dbin(pi1[i],n1)
    #u1[i] ~ dnorm(0,sd=ssub) # random effect for player 1
    # rather than a random effect, I need a fixed effect
    # with a fixed and known prior (rather than hierarchical)
    # so, take out pi[i] + u1[i], and replace with a fixed prior
    # my prior: centered around 0.25, 
    # but with good allowance for uncertainty (large variance)
    s1_pi[i] ~ dbeta(.3,.9) # very diffuse prior (Expected value 0.25, expected SD 0.29)
    
    y2[i] ~ dbin(s2_pi[i],n2)
    #u2[i] ~ dnorm(0,sd=ssub)
    s2_pi[i] ~ dbeta(.5,1.5) # slightly smaller sd for s2 than s1
    
    y3[i] ~ dbin(s3_pi[i],n3)
    s3_pi[i] ~ dbeta(.5,1.5) # slightly smaller sd for s3 than s1
  }
  # solution when trying the first N-1 outputs
  # y1[4] <- n1-(y1[1]+y1[2]+y1[3])
  # y2[4] <- n2-(y2[1]+y2[2]+y2[3])
  # pi1[4] <- 1-(pi1[1]+pi1[2]+pi1[3])
  # pi2[4] <- 1-(pi2[1]+pi2[2]+pi2[3])
})

multi_sf_data = list(y=y,y1=y_s1,y2=y_s2,y3=y_s3)
multi_sf_consts = list(n=n,n1=n1,n2=n2,n3=n3,N=N,alpha = alpha)
multi_sf_inits = list(pi=piinit)#,u1=u1init,u2=u2init,ssub=ssubinit)

multi_sf_out = nimbleMCMC(code=multi_sf_Code,
                        constants = multi_sf_consts,
                        data = multi_sf_data,
                        inits = multi_sf_inits,
                        nchains = 3, niter = 21000,
                        nburnin=1000, thin = 4,
                        samplesAsCodaMCMC = TRUE,
                        summary = TRUE, WAIC = TRUE,
                        #showCompilerOutput = TRUE,
                        monitors = c('pi','s1_pi','s2_pi','s3_pi'))#,'u1','u2','ssub'))

multi_sf_out$summary$all.chains %>% round(4)
allsamps_sf = as.matrix(multi_sf_out$samples)
colnames(allsamps_sf)
dim(allsamps_sf)
raftery.diag(allsamps_sf)
effectiveSize(allsamps_sf) # why is effective size so big for one of these pis?

ggplot(data = as.data.frame(allsamps_sf))+
    stat_density(aes(x = `pi[4]`,col = "Overall"),lty = 2,geom = "line")+
    stat_density(aes(x = `s1_pi[4]`,col="Hard Court"),geom = "line")+
    stat_density(aes(x = `s2_pi[4]`,col = "Clay"),geom = "line")+
    stat_density(aes(x = `s3_pi[4]`,col="Grass"),geom = "line")+
    scale_color_manual(name=NULL,breaks = c("Overall","Hard Court","Clay","Grass"),
                       values = c("Overall"="black","Hard Court"="royalblue",
                                  "Clay"="orangered","Grass"="green3"))+
    theme_minimal()+
    xlim(c(0,0.5))+
    labs(x = "Z5")




# # try to make a hierarchical model for stroke type ####
# add hierarchical piece for different players
y = table(adat_clean_simple$Ball.hit.to)
y_fed = table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="F"])
y_nad = table(adat_clean_simple$Ball.hit.to[adat_clean_simple$Player=="N"])
N = length(y)
n = sum(y)
n1 = sum(y_fed)
n2 = sum(y_nad)
# n==n1+n2 # sample sizes add to total? Yes

alpha = c(0.25,0.25,0.25,0.25)
# can mess with these priors,
# and see if that makes a difference
# doesn't seem to make a huge 

# define initial values
piinit = c(.28,.18,.18,.36) # sort of empirical bayes with these inits
u1init = c(0,0,0,0)
u2init = c(0,0,0,0)
# interestingly, without initial values, some of the us are huge
# and some of the pis are small and even negative
ssubinit = 0.1

multi_hcp_Code= nimbleCode({
  pi[1:N] ~ ddirch(alpha[1:N])
  y[1:N] ~ dmulti(pi[1:N],n)
  ssub ~ dunif(0,0.3)
  ssurface ~ dgamma(1,1) # should this be dunif instead?
  for (i in 1:N){
    y1[i] ~ dbin(pi1[i],n1)
    u1[i] ~ dnorm(0,sd=ssub) # random effect for player 1
    pi1[i] <- pi[i] + u1[i] #+ surface[]
    
    y2[i] ~ dbin(pi2[i],n2)
    u2[i] ~ dnorm(0,sd=ssub)
    pi2[i] <- pi[i] + u2[i] #+surface[]
  }
  for (j in 1:S) {
    surface[j] ~ dnorm(0,sd=ssurface)
  }
  # solution when trying the first N-1 outputs
  # y1[4] <- n1-(y1[1]+y1[2]+y1[3])
  # y2[4] <- n2-(y2[1]+y2[2]+y2[3])
  # pi1[4] <- 1-(pi1[1]+pi1[2]+pi1[3])
  # pi2[4] <- 1-(pi2[1]+pi2[2]+pi2[3])
})

multi_hcp_data = list(y=y,y1=y_fed,y2=y_nad)
multi_hcp_consts = list(n=n,n1=n1,n2=n2,N=N,alpha = alpha)
multi_hcp_inits = list(pi=piinit,u1=u1init,u2=u2init,ssub=ssubinit)

multi_hcp_out = nimbleMCMC(code=multi_hcp_Code,
                        constants = multi_hcp_consts,
                        data = multi_hcp_data,
                        inits = multi_hcp_inits,
                        nchains = 5, niter = 21000,
                        nburnin=1000, thin = 4,
                        samplesAsCodaMCMC = TRUE,
                        summary = TRUE, WAIC = TRUE,
                        #showCompilerOutput = TRUE,
                        monitors = c('pi','pi1','pi2','u1','u2','ssub'))

multi_hcp_out$summary$all.chains %>% round(4)
multi_hcp_out$WAIC
allsamps_hcp = as.matrix(multi_hcp_out$samples)
colnames(allsamps_hcp)
dim(allsamps_hcp)
raftery.diag(allsamps_hcp)
effectiveSize(allsamps_hcp)



# # try out bart ####
# library(BART)
# # hc_xtrain = as.data.frame(adat_clean_simple %>% select(Ball.lands, Player)) #,Stroke.Type,Stroke.Hand,Hit.Type
# hc_ytrain = adat_clean_simple$Ball.hit.to
# hc_xtrain = model.matrix(Ball.hit.to ~ -1 + Ball.lands + Player,data = adat_clean_simple %>% select(Ball.lands, Player, Ball.hit.to)) %>% 
#   as.matrix
# dim(hc_xtrain)
# length(hc_ytrain)
# 
# bart_fit = mbart(x.train = hc_xtrain,y.train = hc_ytrain)
