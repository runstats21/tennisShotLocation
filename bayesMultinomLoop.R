# iterative Bayes multinomial sampling

# libraries
library(tidyverse)
library(nimble)
library(coda)
library(ggpubr)

# source data cleaning
source("Cleaning.R")

# source cpmap functions
source("MultinomialProbs.R")


# hierarchical multinomial Bayes model ####
hp_fednad_post_samples = function(y,y_fed,y_nad,n_chains=5,
                                  prior_vals = c(1,1,1,1)) {
  require(tidyverse)
  require(nimble)
 
  N = length(y_fed)
  n1 = sum(y_fed)
  n2 = sum(y_nad)
  
  # prior values
  alpha = prior_vals
  # this is classic noninformative prior
  # can mess with these priors, and see if that makes a difference
  # Jeffrey's prior would likely be vector of 1/N_classes (= 0.25)
  
  
  multi_hcp_Code= nimbleCode({
    pi[1:N] ~ ddirch(alpha[1:N])
    # y[1:N] ~ dmulti(pi[1:N],n)
    
    pi1[1:N] ~ ddirch(mp[1:N])
    y1[1:N] ~ dmulti(pi1[1:N],n1)
    
    pi2[1:N] ~ ddirch(mp[1:N])
    y2[1:N] ~ dmulti(pi2[1:N],n2)
    
    # constant to allow for deviations for each player
    mp[1:N] <- pi[1:N]*m
    m ~ dgamma(1,1)
  })
  
  multi_hcp_data = list(y1=y_fed,y2=y_nad)
  multi_hcp_consts = list(n1=n1,n2=n2,N=N,alpha = alpha)
  # multi_hcp_inits = list(pi=piinit,u1=u1init,u2=u2init,ssub=ssubinit)
  
  multi_hcp_out = nimbleMCMC(code=multi_hcp_Code,
                          constants = multi_hcp_consts,
                          data = multi_hcp_data,
                          # modeling player spec data with binomials 
                          # requires initial values
                          #inits = multi_hcp_inits, 
                          nchains = n_chains, niter = 22000,
                          nburnin=2000, thin = 5,
                          samplesAsCodaMCMC = TRUE,
                          summary = TRUE, WAIC = TRUE,
                          #showCompilerOutput = TRUE,
                          monitors = c('pi','pi1','pi2','m'))
  
  return(list(samples = as.matrix(multi_hcp_out$samples),
              hm_out = multi_hcp_out))
}

# make tables of counts for data of interest ####
overall_zct_hr = list()
fed_zct_hr = list()
nad_zct_hr = list()
overall_zct_hr$hard.court = table(adat_clean_simple[,c("Ball.lands","Ball.hit.to")])
overall_zct_hr$clay = table(fdat_clean_simple[,c("Ball.lands","Ball.hit.to")])
overall_zct_hr$grass = table(wdat_clean_simple[,c("Ball.lands","Ball.hit.to")])

fed_zct_hr$hard.court = table(adat_clean_simple[adat_clean_simple$Player=="F",
                                        c("Ball.lands","Ball.hit.to")])
fed_zct_hr$clay = table(fdat_clean_simple[fdat_clean_simple$Player=="F",
                                        c("Ball.lands","Ball.hit.to")])
fed_zct_hr$grass = table(wdat_clean_simple[wdat_clean_simple$Player=="F",
                                        c("Ball.lands","Ball.hit.to")])

nad_zct_hr$hard.court = table(adat_clean_simple[adat_clean_simple$Player=="N",
                                        c("Ball.lands","Ball.hit.to")])
nad_zct_hr$clay = table(fdat_clean_simple[fdat_clean_simple$Player=="N",
                                        c("Ball.lands","Ball.hit.to")])
nad_zct_hr$grass = table(wdat_clean_simple[wdat_clean_simple$Player=="N",
                                        c("Ball.lands","Ball.hit.to")])
# test out list functionality

# test
r = 2
s = "grass"
test_res = hp_fednad_post_samples(y = overall_zct_hr[[s]][paste0("Z",r),],
                               y_fed = fed_zct_hr[[s]][paste0("Z",r),],
                               y_nad = nad_zct_hr[[s]][paste0("Z",r),],
                               n_chains = 5)
# check names
rownames(test_res$hm_out$summary$all.chains)
# check summary
test_res$hm_out$summary$all.chains
# check sample dims
dim(test_res$samples)
colnames(test_res$samples)
# check convergence diagnostics/ess
raftery.diag(test_res$samples) # raftery diag
effectiveSize(test_res$samples) # ess greater than 19000 (out of 20,000 samples) for 

# check trace plots
par(mfrow = c(3,4))
plot(test_res$samples[,2],type = 'l')
plot(test_res$samples[,3],type = 'l')
plot(test_res$samples[,4],type = 'l')
plot(test_res$samples[,5],type = 'l')

plot(test_res$samples[,6],type = 'l')
plot(test_res$samples[,7],type = 'l')
plot(test_res$samples[,8],type = 'l')
plot(test_res$samples[,9],type = 'l')

plot(test_res$samples[,10],type = 'l')
plot(test_res$samples[,11],type = 'l')
plot(test_res$samples[,12],type = 'l')
plot(test_res$samples[,13],type = 'l')
# trace plots look great

acf(test_res$samples[,2])
acf(test_res$samples[,3])
acf(test_res$samples[,4])
acf(test_res$samples[,5])

acf(test_res$samples[,6])
acf(test_res$samples[,7])
acf(test_res$samples[,8])
acf(test_res$samples[,9])

acf(test_res$samples[,10])
acf(test_res$samples[,11])
acf(test_res$samples[,12])
acf(test_res$samples[,13])
par(mfrow = c(1,1))
# some autocorrelation seen in to z2 overall draws with thin = 4

# rbind(test_res$hm_out$summary$all.chains,test_res$hm_out$summary$all.chains)
# NOTE: appears that overall parameters may yet have some autocorr
# but player specific estimates do not appear to have any significant autocorr
# this is shown also by effective sample size

# ggplot(as.data.frame(as.matrix(test_res$samples)))+
#   stat_density(aes(x = `pi[4]`,col = "Overall"),lty = 2,geom = "line")+
#   stat_density(aes(x = `pi1[4]`,col="Federer"),geom = "line")+
#   stat_density(aes(x = `pi2[4]`,col = "Nadal"),geom = "line")+
#   scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
#                      values = c("Overall"="black","Federer"="royalblue","Nadal"="orange"))+
#   theme_minimal()+
#   xlim(c(0.05,0.75))+
#   theme(plot.title = element_text(hjust = 0.5),
#         #legend.key.size = unit(1, 'cm'),
#         legend.text = element_text(size = 10))+
#   labs(x = "P(Hit to Z5)",
#        title = "Hard Court Received Z5")
  

# loop through all surfaces #####
surfaces = c("hard.court","clay","grass")
n_parms = dim(test_res$samples)[2] # number of parameters per loop (13 currently)
samps_cur = NULL
summary_cur = NULL
start.time = Sys.time()
# loop through all surfaces
# NOTE/TODO: could change naming to be name of surface rather than number
# to avoid confusion
for (s in 1:length(surfaces)){
  surfaces = c("hard.court","clay","grass")
  # loop through all received from (`Ball.lands`) zones
  for (r in 2:5) {
    res = hp_fednad_post_samples(y = overall_zct_hr[[surfaces[s]]][paste0("Z",r),],
                                 y_fed = fed_zct_hr[[surfaces[s]]][paste0("Z",r),],
                                 y_nad = nad_zct_hr[[surfaces[s]]][paste0("Z",r),],
                                 n_chains = 3)
    cat(paste0("MCMC complete for ",surfaces[s]," received in Z",r),"\n")
    # save samples and summaries and bind them together as we loop
    if (s == "hard.court" & r == 2) {
      samps_cur = res$samples
      colnames(samps_cur)[(1:n_parms)+(n_parms*(r-2))+(52*(s-1))] = 
        paste0(s,"r",paste0("Z",r),"_",colnames(samps_cur)[(1:n_parms)+(n_parms*(r-2))+(52*(s-1))])
      
      summary_cur = res$hm_out$summary$all.chains
      rownames(summary_cur)[(1:n_parms)+(n_parms*(r-2))+(52*(s-1))] = 
        paste0(s,"r",paste0("Z",r),"_",rownames(summary_cur)[(1:n_parms)+(n_parms*(r-2))+(52*(s-1))])
    } else {
      samps_cur = cbind(samps_cur,res$samples)
      colnames(samps_cur)[(1:n_parms)+(n_parms*(r-2))+(52*(s-1))] = 
        paste0(s,"r",paste0("Z",r),"_",colnames(samps_cur)[(1:n_parms)+(n_parms*(r-2))+(52*(s-1))])
      
      summary_cur = rbind(summary_cur,res$hm_out$summary$all.chains)
      rownames(summary_cur)[(1:n_parms)+(n_parms*(r-2))+(52*(s-1))] = 
        paste0(s,"r",paste0("Z",r),"_",rownames(summary_cur)[(1:n_parms)+(n_parms*(r-2))+(52*(s-1))])
    }
  }
  cat(surfaces[s], "complete\n")
}
end.time = Sys.time()
cat("total time to run 4x3x4x3 = 144 posteriors:",
    round((end.time-start.time),2),"minutes.\n")
# approx: 7 minutes
colnames(samps_cur)
dim(samps_cur)
rownames(summary_cur)
# 52 for each surface
summary_cur[1:52,]
summary_cur[52+1:52,]
summary_cur[96+1:52,]
# save above samples
saveRDS(samps_cur,file = "./saved_data/allsamps_srploop_3chains.Rdata")
# read samples back in
allsamps_srp = readRDS("./saved_data/allsamps_srploop.Rdata") # (20k samps)
colnames(allsamps_srp)
samps_cur = allsamps_srp

# test plotting these samples ####
r=4
p =0
colnames(samps_cur)[(52+1+4*(p+1)+13*(r-1))]
ggarrange(
ggplot(data = as.data.frame(samps_cur))+
  #stat_density(aes(x = samps_cur[,(52+1+4*(p+1))],col = "Overall"),lty = 2,geom = "line")+
  stat_density(aes(x = `2rZ5_pi[4]`,col = "Overall"),lty = 2,geom = "line")+
  stat_density(aes(x = `2rZ5_pi1[4]`,col="Federer"),geom = "line")+
  stat_density(aes(x = `2rZ5_pi2[4]`,col = "Nadal"),geom = "line")+
  scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                     values = c("Overall"="black","Federer"="royalblue","Nadal"="orange"))+
  theme_minimal()+
  xlim(c(0,0.75))+
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10))+
  labs(x = "P(Hit to Z5)",
       title = "Clay Received Z5"),

ggplot(data = as.data.frame(samps_cur))+
  stat_density(aes(x = `1rZ5_pi[4]`,col = "Overall"),lty = 2,geom = "line")+
  stat_density(aes(x = `1rZ5_pi1[4]`,col="Federer"),geom = "line")+
  stat_density(aes(x = `1rZ5_pi2[4]`,col = "Nadal"),geom = "line")+
  scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                     values = c("Overall"="black","Federer"="royalblue","Nadal"="orange"))+
  theme_minimal()+
  xlim(c(0,0.75))+
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10))+
  labs(x = "P(Hit to Z5)",
       title = "Hard Court Received Z5"),

ggplot(data = as.data.frame(samps_cur))+
  stat_density(aes(x = `3rZ5_pi[4]`,col = "Overall"),lty = 2,geom = "line")+
  stat_density(aes(x = `3rZ5_pi1[4]`,col="Federer"),geom = "line")+
  stat_density(aes(x = `3rZ5_pi2[4]`,col = "Nadal"),geom = "line")+
  scale_color_manual(name=NULL,breaks = c("Overall","Federer","Nadal"),
                     values = c("Overall"="black","Federer"="royalblue","Nadal"="orange"))+
  theme_minimal()+
  xlim(c(0,0.75))+
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10))+
  labs(x = "P(Hit to Z5)",
       title = "Grass Received Z5"),
ncol = 3,common.legend = T,legend = "bottom")
  

# With all these samples, what parameters/info about them are we interested in?
# Differences?
# - zone to zone comparision
# - forehand to forehand comparison
# Probabilities related to certain parameters?

# lets get MV differences to start
# MV differences ####
# format: recieved from _ Hit to _ (r_h_)
ratio_of_odds = function(pi1,pi2){
  log( (pi1/(1-pi1))/(pi2/(1-pi2)) )
}

# in terms of log ratio of odds
fedvsnad_clay_pis = cbind(r2h2 = ratio_of_odds(samps_cur[,"2rZ2_pi1[1]"],samps_cur[,"2rZ2_pi2[1]"]),
                          r2h3 = ratio_of_odds(samps_cur[,"2rZ2_pi1[2]"],samps_cur[,"2rZ2_pi2[2]"]),
                          r2h4 = ratio_of_odds(samps_cur[,"2rZ2_pi1[3]"],samps_cur[,"2rZ2_pi2[3]"]),
                          r2h5 = ratio_of_odds(samps_cur[,"2rZ2_pi1[4]"],samps_cur[,"2rZ2_pi2[4]"]),
  
                          r3h2 = ratio_of_odds(samps_cur[,"2rZ3_pi1[1]"],samps_cur[,"2rZ3_pi2[1]"]),
                          r3h3 = ratio_of_odds(samps_cur[,"2rZ3_pi1[2]"],samps_cur[,"2rZ3_pi2[2]"]),
                          r3h4 = ratio_of_odds(samps_cur[,"2rZ3_pi1[3]"],samps_cur[,"2rZ3_pi2[3]"]),
                          r3h5 = ratio_of_odds(samps_cur[,"2rZ3_pi1[4]"],samps_cur[,"2rZ3_pi2[4]"]),
  
                          r4h2 = ratio_of_odds(samps_cur[,"2rZ4_pi1[1]"],samps_cur[,"2rZ4_pi2[1]"]),
                          r4h3 = ratio_of_odds(samps_cur[,"2rZ4_pi1[2]"],samps_cur[,"2rZ4_pi2[2]"]),
                          r4h4 = ratio_of_odds(samps_cur[,"2rZ4_pi1[3]"],samps_cur[,"2rZ4_pi2[3]"]),
                          r4h4 = ratio_of_odds(samps_cur[,"2rZ4_pi1[4]"],samps_cur[,"2rZ4_pi2[4]"]),
                          
                          r5h2 = ratio_of_odds(samps_cur[,"2rZ5_pi1[1]"],samps_cur[,"2rZ5_pi2[1]"]),
                          r5h3 = ratio_of_odds(samps_cur[,"2rZ5_pi1[2]"],samps_cur[,"2rZ5_pi2[2]"]),
                          r5h4 = ratio_of_odds(samps_cur[,"2rZ5_pi1[3]"],samps_cur[,"2rZ5_pi2[3]"]),
                          r5h5 = ratio_of_odds(samps_cur[,"2rZ5_pi1[4]"],samps_cur[,"2rZ5_pi2[4]"]))

fedvsnad_hc_pis = cbind(r2h2 = ratio_of_odds(samps_cur[,"1rZ2_pi1[1]"],samps_cur[,"1rZ2_pi2[1]"]),
                        r2h3 = ratio_of_odds(samps_cur[,"1rZ2_pi1[2]"],samps_cur[,"1rZ2_pi2[2]"]),
                        r2h4 = ratio_of_odds(samps_cur[,"1rZ2_pi1[3]"],samps_cur[,"1rZ2_pi2[3]"]),
                        r2h5 = ratio_of_odds(samps_cur[,"1rZ2_pi1[4]"],samps_cur[,"1rZ2_pi2[4]"]),

                        r3h2 = ratio_of_odds(samps_cur[,"1rZ3_pi1[1]"],samps_cur[,"1rZ3_pi2[1]"]),
                        r3h3 = ratio_of_odds(samps_cur[,"1rZ3_pi1[2]"],samps_cur[,"1rZ3_pi2[2]"]),
                        r3h4 = ratio_of_odds(samps_cur[,"1rZ3_pi1[3]"],samps_cur[,"1rZ3_pi2[3]"]),
                        r3h5 = ratio_of_odds(samps_cur[,"1rZ3_pi1[4]"],samps_cur[,"1rZ3_pi2[4]"]),

                        r4h2 = ratio_of_odds(samps_cur[,"1rZ4_pi1[1]"],samps_cur[,"1rZ4_pi2[1]"]),
                        r4h3 = ratio_of_odds(samps_cur[,"1rZ4_pi1[2]"],samps_cur[,"1rZ4_pi2[2]"]),
                        r4h4 = ratio_of_odds(samps_cur[,"1rZ4_pi1[3]"],samps_cur[,"1rZ4_pi2[3]"]),
                        r4h4 = ratio_of_odds(samps_cur[,"1rZ4_pi1[4]"],samps_cur[,"1rZ4_pi2[4]"]),
                        
                        r5h2 = ratio_of_odds(samps_cur[,"1rZ5_pi1[1]"],samps_cur[,"1rZ5_pi2[1]"]),
                        r5h3 = ratio_of_odds(samps_cur[,"1rZ5_pi1[2]"],samps_cur[,"1rZ5_pi2[2]"]),
                        r5h4 = ratio_of_odds(samps_cur[,"1rZ5_pi1[3]"],samps_cur[,"1rZ5_pi2[3]"]),
                        r5h5 = ratio_of_odds(samps_cur[,"1rZ5_pi1[4]"],samps_cur[,"1rZ5_pi2[4]"]))

fedvsnad_grass_pis = cbind(r2h2 = ratio_of_odds(samps_cur[,"3rZ2_pi1[1]"],samps_cur[,"3rZ2_pi2[1]"]),
                          r2h3 = ratio_of_odds(samps_cur[,"3rZ2_pi1[2]"],samps_cur[,"3rZ2_pi2[2]"]),
                          r2h4 = ratio_of_odds(samps_cur[,"3rZ2_pi1[3]"],samps_cur[,"3rZ2_pi2[3]"]),
                          r2h5 = ratio_of_odds(samps_cur[,"3rZ2_pi1[4]"],samps_cur[,"3rZ2_pi2[4]"]),
  
                          r3h2 = ratio_of_odds(samps_cur[,"3rZ3_pi1[1]"],samps_cur[,"3rZ3_pi2[1]"]),
                          r3h3 = ratio_of_odds(samps_cur[,"3rZ3_pi1[2]"],samps_cur[,"3rZ3_pi2[2]"]),
                          r3h4 = ratio_of_odds(samps_cur[,"3rZ3_pi1[3]"],samps_cur[,"3rZ3_pi2[3]"]),
                          r3h5 = ratio_of_odds(samps_cur[,"3rZ3_pi1[4]"],samps_cur[,"3rZ3_pi2[4]"]),
  
                          r4h2 = ratio_of_odds(samps_cur[,"3rZ4_pi1[1]"],samps_cur[,"3rZ4_pi2[1]"]),
                          r4h3 = ratio_of_odds(samps_cur[,"3rZ4_pi1[2]"],samps_cur[,"3rZ4_pi2[2]"]),
                          r4h4 = ratio_of_odds(samps_cur[,"3rZ4_pi1[3]"],samps_cur[,"3rZ4_pi2[3]"]),
                          r4h4 = ratio_of_odds(samps_cur[,"3rZ4_pi1[4]"],samps_cur[,"3rZ4_pi2[4]"]),
                          
                          r5h2 = ratio_of_odds(samps_cur[,"3rZ5_pi1[1]"],samps_cur[,"3rZ5_pi2[1]"]),
                          r5h3 = ratio_of_odds(samps_cur[,"3rZ5_pi1[2]"],samps_cur[,"3rZ5_pi2[2]"]),
                          r5h4 = ratio_of_odds(samps_cur[,"3rZ5_pi1[3]"],samps_cur[,"3rZ5_pi2[3]"]),
                          r5h5 = ratio_of_odds(samps_cur[,"3rZ5_pi1[4]"],samps_cur[,"3rZ5_pi2[4]"]))

# subtraction
fedvsnad_clay_pis_sub = cbind(r2h2 = samps_cur[,"2rZ2_pi1[1]"]-samps_cur[,"2rZ2_pi2[1]"],
                          r2h3 = samps_cur[,"2rZ2_pi1[2]"]-samps_cur[,"2rZ2_pi2[2]"],
                          r2h4 = samps_cur[,"2rZ2_pi1[3]"]-samps_cur[,"2rZ2_pi2[3]"],
                          r2h5 = samps_cur[,"2rZ2_pi1[4]"]-samps_cur[,"2rZ2_pi2[4]"],
  
                          r3h2 = samps_cur[,"2rZ3_pi1[1]"]-samps_cur[,"2rZ3_pi2[1]"],
                          r3h3 = samps_cur[,"2rZ3_pi1[2]"]-samps_cur[,"2rZ3_pi2[2]"],
                          r3h4 = samps_cur[,"2rZ3_pi1[3]"]-samps_cur[,"2rZ3_pi2[3]"],
                          r3h5 = samps_cur[,"2rZ3_pi1[4]"]-samps_cur[,"2rZ3_pi2[4]"],
  
                          r4h2 = samps_cur[,"2rZ4_pi1[1]"]-samps_cur[,"2rZ4_pi2[1]"],
                          r4h3 = samps_cur[,"2rZ4_pi1[2]"]-samps_cur[,"2rZ4_pi2[2]"],
                          r4h4 = samps_cur[,"2rZ4_pi1[3]"]-samps_cur[,"2rZ4_pi2[3]"],
                          r4h4 = samps_cur[,"2rZ4_pi1[4]"]-samps_cur[,"2rZ4_pi2[4]"],
                          
                          r5h2 = samps_cur[,"2rZ5_pi1[1]"]-samps_cur[,"2rZ5_pi2[1]"],
                          r5h3 = samps_cur[,"2rZ5_pi1[2]"]-samps_cur[,"2rZ5_pi2[2]"],
                          r5h4 = samps_cur[,"2rZ5_pi1[3]"]-samps_cur[,"2rZ5_pi2[3]"],
                          r5h5 = samps_cur[,"2rZ5_pi1[4]"]-samps_cur[,"2rZ5_pi2[4]"])

fedvsnad_hc_pis_sub = cbind(r2h2 = samps_cur[,"1rZ2_pi1[1]"]-samps_cur[,"1rZ2_pi2[1]"],
                        r2h3 = samps_cur[,"1rZ2_pi1[2]"]-samps_cur[,"1rZ2_pi2[2]"],
                        r2h4 = samps_cur[,"1rZ2_pi1[3]"]-samps_cur[,"1rZ2_pi2[3]"],
                        r2h5 = samps_cur[,"1rZ2_pi1[4]"]-samps_cur[,"1rZ2_pi2[4]"],

                        r3h2 = samps_cur[,"1rZ3_pi1[1]"]-samps_cur[,"1rZ3_pi2[1]"],
                        r3h3 = samps_cur[,"1rZ3_pi1[2]"]-samps_cur[,"1rZ3_pi2[2]"],
                        r3h4 = samps_cur[,"1rZ3_pi1[3]"]-samps_cur[,"1rZ3_pi2[3]"],
                        r3h5 = samps_cur[,"1rZ3_pi1[4]"]-samps_cur[,"1rZ3_pi2[4]"],

                        r4h2 = samps_cur[,"1rZ4_pi1[1]"]-samps_cur[,"1rZ4_pi2[1]"],
                        r4h3 = samps_cur[,"1rZ4_pi1[2]"]-samps_cur[,"1rZ4_pi2[2]"],
                        r4h4 = samps_cur[,"1rZ4_pi1[3]"]-samps_cur[,"1rZ4_pi2[3]"],
                        r4h4 = samps_cur[,"1rZ4_pi1[4]"]-samps_cur[,"1rZ4_pi2[4]"],
                        
                        r5h2 = samps_cur[,"1rZ5_pi1[1]"]-samps_cur[,"1rZ5_pi2[1]"],
                        r5h3 = samps_cur[,"1rZ5_pi1[2]"]-samps_cur[,"1rZ5_pi2[2]"],
                        r5h4 = samps_cur[,"1rZ5_pi1[3]"]-samps_cur[,"1rZ5_pi2[3]"],
                        r5h5 = samps_cur[,"1rZ5_pi1[4]"]-samps_cur[,"1rZ5_pi2[4]"])

fedvsnad_grass_pis_sub = cbind(r2h2 = samps_cur[,"3rZ2_pi1[1]"]-samps_cur[,"3rZ2_pi2[1]"],
                          r2h3 = samps_cur[,"3rZ2_pi1[2]"]-samps_cur[,"3rZ2_pi2[2]"],
                          r2h4 = samps_cur[,"3rZ2_pi1[3]"]-samps_cur[,"3rZ2_pi2[3]"],
                          r2h5 = samps_cur[,"3rZ2_pi1[4]"]-samps_cur[,"3rZ2_pi2[4]"],
  
                          r3h2 = samps_cur[,"3rZ3_pi1[1]"]-samps_cur[,"3rZ3_pi2[1]"],
                          r3h3 = samps_cur[,"3rZ3_pi1[2]"]-samps_cur[,"3rZ3_pi2[2]"],
                          r3h4 = samps_cur[,"3rZ3_pi1[3]"]-samps_cur[,"3rZ3_pi2[3]"],
                          r3h5 = samps_cur[,"3rZ3_pi1[4]"]-samps_cur[,"3rZ3_pi2[4]"],
  
                          r4h2 = samps_cur[,"3rZ4_pi1[1]"]-samps_cur[,"3rZ4_pi2[1]"],
                          r4h3 = samps_cur[,"3rZ4_pi1[2]"]-samps_cur[,"3rZ4_pi2[2]"],
                          r4h4 = samps_cur[,"3rZ4_pi1[3]"]-samps_cur[,"3rZ4_pi2[3]"],
                          r4h4 = samps_cur[,"3rZ4_pi1[4]"]-samps_cur[,"3rZ4_pi2[4]"],
                          
                          r5h2 = samps_cur[,"3rZ5_pi1[1]"]-samps_cur[,"3rZ5_pi2[1]"],
                          r5h3 = samps_cur[,"3rZ5_pi1[2]"]-samps_cur[,"3rZ5_pi2[2]"],
                          r5h4 = samps_cur[,"3rZ5_pi1[3]"]-samps_cur[,"3rZ5_pi2[3]"],
                          r5h5 = samps_cur[,"3rZ5_pi1[4]"]-samps_cur[,"3rZ5_pi2[4]"])

# get Bayes estimates (BEse)
ce=apply(fedvsnad_clay_pis,2,mean)
hce=apply(fedvsnad_hc_pis,2,mean)
ge=apply(fedvsnad_grass_pis,2,mean)

# and HPD intervals
ce_quantiles = apply(fedvsnad_clay_pis,2,quantile, c(0.025,0.975))
ce_hpd = apply(as.mcmc(fedvsnad_clay_pis),2,function(x) {HPDinterval(as.mcmc(x))})
hce_hpd = apply(as.mcmc(fedvsnad_hc_pis),2,function(x) {HPDinterval(as.mcmc(x))})
ge_hpd = apply(as.mcmc(fedvsnad_grass_pis),2,function(x) {HPDinterval(as.mcmc(x))})

# matrices of bayes estimates
ce_mat = rbind(z2=ce[1:4],z3=ce[5:8],z4=ce[9:12],z5=ce[13:16])
hce_mat = rbind(z2=hce[1:4],z3=hce[5:8],z4=hce[9:12],z5=hce[13:16])
ge_mat = rbind(z2=ge[1:4],z3=ge[5:8],z4=ge[9:12],z5=ge[13:16])
colnames(ce_mat) = str_c("z",2:5)
colnames(hce_mat) = str_c("z",2:5)
colnames(ge_mat) = str_c("z",2:5)
# columns: hit to
# rows: received from

# get significance
ce_sig = apply(ce_hpd,2,function(vec) {
  (vec[1]>0 & vec[2]>0) | (vec[1]<0 & vec[2]<0)
})
hce_sig = apply(hce_hpd,2,function(vec) {
  (vec[1]>0 & vec[2]>0) | (vec[1]<0 & vec[2]<0)
})
ge_sig = apply(ge_hpd,2,function(vec) {
  (vec[1]>0 & vec[2]>0) | (vec[1]<0 & vec[2]<0)
})

ce_df = as.data.frame(as.table(ce_mat)) %>% 
  rename(Lands= Var1,Hit.to = Var2) %>% 
  mutate(sig = ce_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
  mutate(sig = ifelse(sig,sig,NA))
hce_df = as.data.frame(as.table(hce_mat)) %>% 
  rename(Lands= Var1,Hit.to = Var2) %>% 
  mutate(sig = hce_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
  mutate(sig = ifelse(sig,sig,NA))
ge_df = as.data.frame(as.table(ge_mat)) %>% 
  rename(Lands= Var1,Hit.to = Var2) %>% 
  mutate(sig = ge_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
  mutate(sig = ifelse(sig,sig,NA))

# TODO: test this on the odds scale as well

# Define the color gradient
colfunc <- colorRampPalette(c("blue", "white", "red"))
grain = 5

# Create heat maps (modified from AI generated code)
# NOTE: could also change text to intervals rather than estimates
ggarrange(
  ggplot(data = hce_df, aes(x = Lands, y = Hit.to, fill = Freq)) +
  geom_tile(aes(col = sig),lwd = 1.1)+
  geom_text(aes(label= round(Freq,2)))+
  coord_flip()+
  scale_fill_gradient2(limits = c(-2.3,2.3),n.breaks = grain,
                       high = "firebrick1",low = "royalblue1",mid = "white")+
  # scale_fill_gradientn(colours = colfunc(grain)) +
  # scale_color_discrete(type = c("green"), labels = c("Significant",NULL),
  #                      na.value = "white")+
  scale_color_manual(name = NULL,values = c("TRUE" = "green"),
                     labels = c("Significant"),na.value = "white")+
    
  theme_minimal() +
  # guides(col = FALSE) +
  theme(plot.title = element_text(hjust = 0.5),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        ) +
  labs(fill = expression("log("~O[Fed]/O[Nadal]~")"),col = NULL,
       title = "Hard Court"),
  
  ggplot(data = ce_df, aes(x = Lands, y = Hit.to, fill = Freq)) +
    geom_tile(aes(col = sig),lwd=1.1)+
    coord_flip()+
    geom_text(aes(label= round(Freq,2)))+
    scale_fill_gradient2(limits = c(-2.3,2.3),n.breaks = grain,
                         high = "firebrick1",low = "royalblue1",mid = "white")+
    # scale_fill_gradientn(colours = colfunc(grain)) +
    # scale_color_discrete(type = c("green"), labels = c("Significant"),
    #                      na.value = "white")+
    scale_color_manual(name = NULL,values = c("TRUE" = "green"),
                     labels = c("Significant"),na.value = "white")+
    theme_minimal() +
    # guides(col = FALSE) +
    theme(plot.title = element_text(hjust = 0.5),
          #axis.text.x = element_text(angle = 90, hjust = 1),
          ) +
    labs(fill = "log(Odds_Fed/Odds_Nadal)",col = NULL,
         title = "Clay"),

  ggplot(data = ge_df, aes(x = Lands, y = Hit.to, fill = Freq)) +
    geom_tile(aes(col = sig),lwd=1.1)+
    geom_text(aes(label= round(Freq,2)))+
    coord_flip()+
    scale_fill_gradient2(limits = c(-2.3,2.3),n.breaks = grain,
                         high = "firebrick1",low = "royalblue1",mid = "white")+
    # scale_fill_gradientn(colours = colfunc(grain)) +
    # scale_color_discrete(type = c("green"), labels = c("Significant"),
    #                      na.value = "white")+
    scale_color_manual(name = NULL,values = c("TRUE" = "green"),
                     labels = c("Significant"),na.value = "white")+
    theme_minimal() +
    # guides(col = FALSE) +
    theme(plot.title = element_text(hjust = 0.5),
          #axis.text.x = element_text(angle = 90, hjust = 1),
          legend.direction = "horizontal") +
    # annotate("text",x=1, y = Inf, label = "Nadal",hjust = -0.5,size = 4)+
    labs(fill = "log(Odds_Fed/Odds_Nadal)",col = NULL,
         title = "Grass"),
ncol = 3, common.legend = T,legend = "right") %>% 
  annotate_figure(top = "Federer vs. Nadal log(Odds Ratio) Bayes Estimates")


# get sig flags
ce_sig_mat = apply(ce_hpd,2,function(vec) {
  (vec[1]>0 & vec[2]>0) | (vec[1]<0 & vec[2]<0)
  }) %>% rev %>% matrix(nrow = 4,ncol = 4,byrow = T)
hce_sig_mat = apply(hce_hpd,2,function(vec) {
  (vec[1]>0 & vec[2]>0) | (vec[1]<0 & vec[2]<0)
  }) %>% rev %>% matrix(nrow = 4,ncol = 4,byrow = T)
ge_sig_mat = apply(ge_hpd,2,function(vec) {
  (vec[1]>0 & vec[2]>0) | (vec[1]<0 & vec[2]<0)
  }) %>% rev %>% matrix(nrow = 4,ncol = 4,byrow = T)
# redefine dim names
colnames(ce_sig_mat) = str_c("z",2:5);rownames(ce_sig_mat) = rev(str_c("z",2:5))
colnames(hce_sig_mat) = str_c("z",2:5);rownames(hce_sig_mat) = rev(str_c("z",2:5))
colnames(ge_sig_mat) = str_c("z",2:5);rownames(ge_sig_mat) = rev(str_c("z",2:5))
hce_sig_mat
ce_sig_mat
ge_sig_mat

# TODO:
# 2) table of which zones are most likely for each player in given situation
# and if that is significant! 
# really we just want to know if there is a recommended zone,
# or if vector of probs/plot of post dist would be more informative
# 3) could make this a heat map, and circle zone in green if it is significantly most likely
# i.e.: Table of ests and sig presence (or many heat maps with sig marks: 
# (R=4)x1, x 3 surfaces, for each player), 
# -> as **all we really care about are most significant zone(s)
# for player given hit location and surface**!
# can also do this work in presentationViz.R

settings = c(str_c("1rZ",2:5),str_c("2rZ",2:5),str_c("3rZ",2:5))

# build data frame
# 4 rows for name and sig flag for both players, 12 columns for settings
# values: sig or not
# could maybe add the estimate as well
rownames_custom = c("fed_zone_name","fed_sig","fed_hpd_l","fed_hpd_u",
                    "fed_diff_sig","fed_diff_sig_num",
                    "nadal_zone_name","nadal_sig","nadal_hpd_l","nadal_hpd_u",
                    "nad_diff_sig","nad_diff_sig_num")

max_prob_sig  = matrix(nrow = length(rownames_custom),ncol = length(settings))
colnames(max_prob_sig) = settings
rownames(max_prob_sig) = rownames_custom
for (i in 1:length(settings)) {
  # i=2
  setting = settings[i]
  cur_setting_f = str_c(setting,"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"))
  cur_setting_n = str_c(setting,"_",c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"))
  
  # get location with max expected prob for a certain setting
  m_f = which.max(apply(samps_cur[,cur_setting_f],2,mean))
  m_n = which.max(apply(samps_cur[,cur_setting_n],2,mean))
  
  # get name by pulling index off of column name and adding 1
  max_prob_sig["fed_zone_name",i] = str_c("Z",(colnames(samps_cur[,cur_setting_f])[m_f] %>%
    str_split("") %>% .[[1]] %>% .[length(.)-1] %>% as.numeric)+1)
  max_prob_sig["nadal_zone_name",i]=str_c("Z",(colnames(samps_cur[,cur_setting_n])[m_n] %>%
    str_split("") %>% .[[1]] %>% .[length(.)-1] %>% as.numeric)+1)
  
  # get HPD intervals
  # and check if HPD interval of zone with max expected prob sig > others
  # i.e., lwr > upr of all other zones
  hpd_f = apply(samps_cur[,cur_setting_f],2,function(x) {HPDinterval(as.mcmc(x))})
  max_prob_sig["fed_sig",i] = all(hpd_f[,m_f][1]>hpd_f[,-m_f][2,])
  max_prob_sig["fed_hpd_l",i] = round(hpd_f[,m_f],3)[1]
  max_prob_sig["fed_hpd_u",i] = round(hpd_f[,m_f],3)[2]
  
  hpd_n = apply(samps_cur[,cur_setting_n],2,function(x) {HPDinterval(as.mcmc(x))})
  max_prob_sig["nadal_sig",i] = all(hpd_n[,m_n][1]>hpd_n[,-m_n][2,])
  max_prob_sig["nadal_hpd_l",i] = round(hpd_n[,m_n],3)[1]
  max_prob_sig["nadal_hpd_u",i] = round(hpd_n[,m_n],3)[2]
  
  # get diffs
  maxdif1 = samps_cur[,cur_setting_f][,m_f]-samps_cur[,cur_setting_f][,-m_f][,1]
  maxdif2 = samps_cur[,cur_setting_f][,m_f]-samps_cur[,cur_setting_f][,-m_f][,2]
  maxdif3 = samps_cur[,cur_setting_f][,m_f]-samps_cur[,cur_setting_f][,-m_f][,3]
  max_prob_sig["fed_diff_sig",i] = all(c(HPDinterval(as.mcmc(maxdif1))[1]>0 | HPDinterval(as.mcmc(maxdif1))[2]<0,
                                         HPDinterval(as.mcmc(maxdif2))[1]>0 | HPDinterval(as.mcmc(maxdif2))[2]<0,
                                         HPDinterval(as.mcmc(maxdif3))[1]>0 | HPDinterval(as.mcmc(maxdif2))[2]<0)
                                        )
  
  max_prob_sig["fed_diff_sig_num",i] = sum(c(HPDinterval(as.mcmc(maxdif1))[1]>0 | HPDinterval(as.mcmc(maxdif1))[2]<0,
                                         HPDinterval(as.mcmc(maxdif2))[1]>0 | HPDinterval(as.mcmc(maxdif2))[2]<0,
                                         HPDinterval(as.mcmc(maxdif3))[1]>0 | HPDinterval(as.mcmc(maxdif2))[2]<0)
                                        )
  
  n_maxdif1 = samps_cur[,cur_setting_n][,m_n]-samps_cur[,cur_setting_n][,-m_n][,1]
  n_maxdif2 = samps_cur[,cur_setting_n][,m_n]-samps_cur[,cur_setting_n][,-m_n][,2]
  n_maxdif3 = samps_cur[,cur_setting_n][,m_n]-samps_cur[,cur_setting_n][,-m_n][,3]
  max_prob_sig["nad_diff_sig",i] = all(c(HPDinterval(as.mcmc(n_maxdif1))[1]>0 | HPDinterval(as.mcmc(n_maxdif1))[2]<0,
                                         HPDinterval(as.mcmc(n_maxdif2))[1]>0 | HPDinterval(as.mcmc(n_maxdif2))[2]<0,
                                         HPDinterval(as.mcmc(n_maxdif3))[1]>0 | HPDinterval(as.mcmc(n_maxdif2))[2]<0)
                                        )
  
  max_prob_sig["nad_diff_sig_num",i] = sum(c(HPDinterval(as.mcmc(n_maxdif1))[1]>0 | HPDinterval(as.mcmc(n_maxdif1))[2]<0,
                                         HPDinterval(as.mcmc(n_maxdif2))[1]>0 | HPDinterval(as.mcmc(n_maxdif2))[2]<0,
                                         HPDinterval(as.mcmc(n_maxdif3))[1]>0 | HPDinterval(as.mcmc(n_maxdif2))[2]<0)
                                        )
  
  
}
t(max_prob_sig)[,1:6]
t(max_prob_sig)[,-(1:6)]

# other tests showed where players were different
# these tests show where players are predictable
t(max_prob_sig) %>% xtable::xtable(align = rep("c",nrow(max_prob_sig)+1),
                                   label = "tab:max_prob_sig",
                                   caption = "Table of Most Likely Locations and corresponding significance")

# plots of bayes estimates (posterior means) with significance ####
all_bayes_ests = apply(samps_cur,2,mean)

allsamps_srp[,str_c(settings[1],"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"))][1:4,]


# can try this again
settings = c(str_c("1rZ",2:5),str_c("2rZ",2:5),str_c("3rZ",2:5))

# build data frame
# 4 rows for name and sig flag for both players, 12 columns for settings
# values: sig or not
# could maybe add the estimate as well
rownames_custom = c("fed_zone_name","fed_sig","fed_hpd_l","fed_hpd_u",
                    "fed_diff_sig","fed_diff_sig_num",
                    "nadal_zone_name","nadal_sig","nadal_hpd_l","nadal_hpd_u",
                    "nad_diff_sig","nad_diff_sig_num")

max_prob_sig  = matrix(nrow = length(rownames_custom),ncol = length(settings))
colnames(max_prob_sig) = settings
rownames(max_prob_sig) = rownames_custom
for (i in 1:length(settings)) {
  # i=2
  setting = settings[i]
  cur_setting_f = str_c(setting,"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"))
  cur_setting_n = str_c(setting,"_",c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"))

  # get location with max expected prob for a certain setting
  m_f = which.max(apply(samps_cur[,cur_setting_f],2,mean))
  m_n = which.max(apply(samps_cur[,cur_setting_n],2,mean))

  # get name by pulling index off of column name and adding 1
  max_prob_sig["fed_zone_name",i] = str_c("Z",(colnames(samps_cur[,cur_setting_f])[m_f] %>%
    str_split("") %>% .[[1]] %>% .[length(.)-1] %>% as.numeric)+1)
  max_prob_sig["nadal_zone_name",i]=str_c("Z",(colnames(samps_cur[,cur_setting_n])[m_n] %>%
    str_split("") %>% .[[1]] %>% .[length(.)-1] %>% as.numeric)+1)

  # get HPD intervals
  # and check if HPD interval of zone with max expected prob sig > others
  # i.e., lwr > upr of all other zones
  hpd_f = apply(samps_cur[,cur_setting_f],2,function(x) {HPDinterval(as.mcmc(x))})
  max_prob_sig["fed_sig",i] = all(hpd_f[,m_f][1]>hpd_f[,-m_f][2,])
  max_prob_sig["fed_hpd_l",i] = round(hpd_f[,m_f],3)[1]
  max_prob_sig["fed_hpd_u",i] = round(hpd_f[,m_f],3)[2]

  hpd_n = apply(samps_cur[,cur_setting_n],2,function(x) {HPDinterval(as.mcmc(x))})
  max_prob_sig["nadal_sig",i] = all(hpd_n[,m_n][1]>hpd_n[,-m_n][2,])
  max_prob_sig["nadal_hpd_l",i] = round(hpd_n[,m_n],3)[1]
  max_prob_sig["nadal_hpd_u",i] = round(hpd_n[,m_n],3)[2]

  # get diffs
  maxdif1 = samps_cur[,cur_setting_f][,m_f]-samps_cur[,cur_setting_f][,-m_f][,1]
  maxdif2 = samps_cur[,cur_setting_f][,m_f]-samps_cur[,cur_setting_f][,-m_f][,2]
  maxdif3 = samps_cur[,cur_setting_f][,m_f]-samps_cur[,cur_setting_f][,-m_f][,3]
  max_prob_sig["fed_diff_sig",i] = all(c(HPDinterval(as.mcmc(maxdif1))[1]>0 | HPDinterval(as.mcmc(maxdif1))[2]<0,
                                         HPDinterval(as.mcmc(maxdif2))[1]>0 | HPDinterval(as.mcmc(maxdif2))[2]<0,
                                         HPDinterval(as.mcmc(maxdif3))[1]>0 | HPDinterval(as.mcmc(maxdif2))[2]<0)
                                        )

  max_prob_sig["fed_diff_sig_num",i] = sum(c(HPDinterval(as.mcmc(maxdif1))[1]>0 | HPDinterval(as.mcmc(maxdif1))[2]<0,
                                         HPDinterval(as.mcmc(maxdif2))[1]>0 | HPDinterval(as.mcmc(maxdif2))[2]<0,
                                         HPDinterval(as.mcmc(maxdif3))[1]>0 | HPDinterval(as.mcmc(maxdif2))[2]<0)
                                        )

  n_maxdif1 = samps_cur[,cur_setting_n][,m_n]-samps_cur[,cur_setting_n][,-m_n][,1]
  n_maxdif2 = samps_cur[,cur_setting_n][,m_n]-samps_cur[,cur_setting_n][,-m_n][,2]
  n_maxdif3 = samps_cur[,cur_setting_n][,m_n]-samps_cur[,cur_setting_n][,-m_n][,3]
  max_prob_sig["nad_diff_sig",i] = all(c(HPDinterval(as.mcmc(n_maxdif1))[1]>0 | HPDinterval(as.mcmc(n_maxdif1))[2]<0,
                                         HPDinterval(as.mcmc(n_maxdif2))[1]>0 | HPDinterval(as.mcmc(n_maxdif2))[2]<0,
                                         HPDinterval(as.mcmc(n_maxdif3))[1]>0 | HPDinterval(as.mcmc(n_maxdif2))[2]<0)
                                        )

  max_prob_sig["nad_diff_sig_num",i] = sum(c(HPDinterval(as.mcmc(n_maxdif1))[1]>0 | HPDinterval(as.mcmc(n_maxdif1))[2]<0,
                                         HPDinterval(as.mcmc(n_maxdif2))[1]>0 | HPDinterval(as.mcmc(n_maxdif2))[2]<0,
                                         HPDinterval(as.mcmc(n_maxdif3))[1]>0 | HPDinterval(as.mcmc(n_maxdif2))[2]<0)
                                        )


}

settings2 = rownames(t(max_prob_sig)[(t(max_prob_sig)[,6]=="2")|(t(max_prob_sig)[,12]=="2"),])



# compare sides
settings = c(str_c("1rZ",2:5),str_c("2rZ",2:5),str_c("3rZ",2:5))

cis_f = matrix(NA,nrow = length(settings),ncol = 2)
cis_n = matrix(NA,nrow = length(settings),ncol = 2)
means_rl_f = rep(NA,length(settings))
means_rl_n = rep(NA,length(settings))
for (i in 1:length(settings)) {
  
  cur_setting_f_rs = str_c(settings[i],"_",c("pi1[1]","pi1[2]"))
  cur_setting_f_ls = str_c(settings[i],"_",c("pi1[3]","pi1[4]"))
  cur_setting_n_rs = str_c(settings[i],"_",c("pi2[1]","pi2[2]"))
  cur_setting_n_ls = str_c(settings[i],"_",c("pi2[3]","pi2[4]"))
  
  rs_f = apply(samps_cur[,c(cur_setting_f_rs)],1,sum)
  ls_f = apply(samps_cur[,c(cur_setting_f_ls)],1,sum)
  rs_n = apply(samps_cur[,c(cur_setting_n_rs)],1,sum)
  ls_n = apply(samps_cur[,c(cur_setting_n_ls)],1,sum)
  
  cis_f[i,] = quantile(rs_f-ls_f, c(0.025,0.975))
  cis_n[i,] = quantile(rs_n-ls_n, c(0.025,0.975))
  means_rl_f[i] = mean(rs_f-ls_f)
  means_rl_n[i] = mean(rs_n-ls_n)
}
# positive and doesn't contain zero: right side sig more likely
cis_f
cis_n

# logical: ci does not contain zero
sigs_f = !((cis_f[,1] < 0) & (cis_f[,2] > 0))
sigs_n = !((cis_n[,1] < 0) & (cis_n[,2] > 0))

side_tbl = cbind(cbind(cis_f,sigs_f),cbind(cis_n,sigs_n))
rownames(side_tbl) = settings
colnames(side_tbl) = c("lwr","upr","side_sig","lwr","upr","side_sig")
# get latex to output
xtable::xtable(side_tbl, digits = 3,
               caption = "95% posterior probability intervals on right-left side probabilities (pi_{Z2}+pi_{Z3}) - (pi_{Z4} + pi_{Z5})",
               label = "side_pref_cis_sig")

side_tbl_plus_means = cbind(cbind(cis_f,sigs_f,means_rl_f>0),cbind(cis_n,sigs_n,means_rl_n>0))
side_tbl_plus_means
