# extend Bayes hierarchical model to other opponents

# libraries
library(tidyverse)
library(nimble)
library(coda)
library(ggpubr)

# source cpmap functions and Cleaning functions
source("MultinomialProbs.R")

# hierarchical Multinomial Bayes model ####
# sensitivity analysis (altering these prior values) could be another good thing
# to add to shot location paper
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
    
    # constant to allow for player specific deviations from overall effects
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

# redefined function in general terms
hp_post_samples = function(y_p1,y_p2,n_chains=5,prior_vals = c(1,1,1,1)) {
  require(tidyverse)
  require(nimble)
 
  N = length(y_p1)
  n1 = sum(y_p1)
  n2 = sum(y_p2)
  
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
    
    # constant to allow for player specific deviations from overall effects
    mp[1:N] <- pi[1:N]*m
    m ~ dgamma(1,1)
  })
  
  multi_hcp_data = list(y1=y_p1,y2=y_p2)
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



# test ####
r = 5
# s = "grass"
# res = hp_fednad_post_samples(y = hr_counts_us_fdp[paste0("Z",r),],
#                              y_fed = fed_counts[paste0("Z",r),],
#                              y_nad = dp_counts[paste0("Z",r),],
#                              n_chains = 5)

res = hp_post_samples(y_p1 = fed_counts[paste0("Z",r),],
                      y_p2 = dp_counts[paste0("Z",r),],
                      n_chains = 5)
# p1 = Federer
# p2 = del Potro
# check names
rownames(res$hm_out$summary$all.chains)
# check summary
res$hm_out$summary$all.chains
# check sample dims
dim(res$samples)
colnames(res$samples)
# check convergence diagnostics/ess
raftery.diag(res$samples) # these look good (overall effects maybe slightly high)
effectiveSize(res$samples) 
# looks good:
# ess greater than 19000 for all player specific probabilities estimates

# check trace plots
plot(res$samples[,1],type = 'l')
par(mfrow = c(3,4))
plot(res$samples[,2],type = 'l')
plot(res$samples[,3],type = 'l')
plot(res$samples[,4],type = 'l')
plot(res$samples[,5],type = 'l')

plot(res$samples[,6],type = 'l')
plot(res$samples[,7],type = 'l')
plot(res$samples[,8],type = 'l')
plot(res$samples[,9],type = 'l')

plot(res$samples[,10],type = 'l')
plot(res$samples[,11],type = 'l')
plot(res$samples[,12],type = 'l')
plot(res$samples[,13],type = 'l')
# trace plots look great

acf(res$samples[,2])
acf(res$samples[,3])
acf(res$samples[,4])
acf(res$samples[,5])

acf(res$samples[,6])
acf(res$samples[,7])
acf(res$samples[,8])
acf(res$samples[,9])

acf(res$samples[,10])
acf(res$samples[,11])
acf(res$samples[,12])
acf(res$samples[,13])
par(mfrow = c(1,1))
acf(res$samples[,1])
# some autocorrelation seen in overall draws, and in m
# but non in player specific effects

## TODO: define below as a function
# a function of s, data (y1,y2 y = y1+y2)
# loop through all receipt locations and bind together ####
# tennis_hp_MCMC_receipt_loop = function(p1_counts,p2_counts,n_chains) {
surfaces = c("hard.court","clay","grass")
hp_post_samples_rloop = function(surface_num, y1, y2) {
  
  surfaces = c("hard.court","clay","grass")
  s = surface_num
  n_parms = 13 # number of parameters per loop (13 currently)
  samps_cur = NULL
  summary_cur = NULL
  start.time = Sys.time()
  # converted from loop which looped through all receipt locs and surfaces
  # loop through all received from (`Ball.lands`) zones
  for (r in 2:5) {
    res = hp_post_samples(y_p1 = y1[paste0("Z",r),],
                          y_p2 = y2[paste0("Z",r),],
                          n_chains = 5)
    cat(paste0("MCMC complete for ",surfaces[s]," received in Z",r),"\n")
    # save samples and summaries and bind them together as we loop
    if (r == 2) {
      samps_cur = res$samples
      colnames(samps_cur)[(1:n_parms)+(n_parms*(r-2))] = 
        paste0(surfaces[s],"r",paste0("Z",r),"_",colnames(samps_cur)[(1:n_parms)+(n_parms*(r-2))])
      
      summary_cur = res$hm_out$summary$all.chains
      rownames(summary_cur)[(1:n_parms)+(n_parms*(r-2))] = 
        paste0(surfaces[s],"r",paste0("Z",r),"_",rownames(summary_cur)[(1:n_parms)+(n_parms*(r-2))])
    } else {
      samps_cur = cbind(samps_cur,res$samples)
      colnames(samps_cur)[(1:n_parms)+(n_parms*(r-2))] = 
        paste0(surfaces[s],"r",paste0("Z",r),"_",colnames(samps_cur)[(1:n_parms)+(n_parms*(r-2))])
      
      summary_cur = rbind(summary_cur,res$hm_out$summary$all.chains)
      rownames(summary_cur)[(1:n_parms)+(n_parms*(r-2))] = 
        paste0(surfaces[s],"r",paste0("Z",r),"_",rownames(summary_cur)[(1:n_parms)+(n_parms*(r-2))])
    }
  }
  end.time = Sys.time()
  cat("total time to get samples for",n_parms*4,"parameters:",
      round((end.time-start.time),2),"minutes.\n")

  return(
  list(samps_cur=samps_cur,
       summary_cur=summary_cur,
       ess = effectiveSize(samps_cur))
  )
}
fed_vs_fed_res = hp_post_samples_rloop(1,
                                       y1 = fed_zct_hr$hard.court, # fed vs. nadal ao
                                       y2 = fed_counts_dp) # fed vs. dp uso

nad_vs_nad_res = hp_post_samples_rloop(surface_num = 3,
                                       y1 = nad_zct_hr$grass, # nad vs fed wim 19
                                       y2 = nad_counts_dj) # nad vs djok wim 18

# check convergence
raftery.diag(fed_vs_fed_res$samps_cur)
fed_vs_fed_res$ess
raftery.diag(nad_vs_nad_res$samps_cur)
nad_vs_nad_res$ess

# save these samples
# saveRDS(samps_cur, file = "./saved_data/samps_allrlocs_fdp_us2009.Rdata")
# # read in samples as saved (from save Rdata file)
# samps_cur = readRDS("./saved_data/samps_allrlocs_fdp_us2009.Rdata")

# saveRDS(fed_vs_fed_res$samps_cur, file = "./saved_data/samps_allrlocs_fnvsfdp_hc.Rdata")
saveRDS(nad_vs_nad_res$samps_cur, file = "./saved_data/samps_allrlocs_nfvsndj_grass.Rdata")
fed_vs_fed_res$summary_cur
nad_vs_nad_res$summary_cur


# very interesting
# could be worth while to test for significant differences 
# (players appear more similar than Federer & Nadal)

# Nadal Djokovic run
s=3 # Nad vs Djokovic wimbledon (grass) 2018
n_parms = 13 # number of parameters per loop (13 currently)
samps_cur_ndj = NULL
summary_cur_ndj = NULL
start.time = Sys.time()
# converted from loop which looped through all receipt locs and surfaces
# loop through all received from (`Ball.lands`) zones
for (r in 2:5) {
  res = hp_fednad_post_samples(y = hr_counts_wim18_ndj[paste0("Z",r),],
                               y_fed = nad_counts[paste0("Z",r),],
                               y_nad = dj_counts[paste0("Z",r),],
                               n_chains = 5)
  cat(paste0("MCMC complete for ",surfaces[s]," received in Z",r),"\n")
  # save samples and summaries and bind them together as we loop
  if (r == 2) {
    samps_cur_ndj = res$samples
    colnames(samps_cur_ndj)[(1:n_parms)+(n_parms*(r-2))] = 
      paste0(surfaces[s],"r",paste0("Z",r),"_",colnames(samps_cur_ndj)[(1:n_parms)+(n_parms*(r-2))])
    
    summary_cur_ndj = res$hm_out$summary$all.chains
    rownames(summary_cur_ndj)[(1:n_parms)+(n_parms*(r-2))] = 
      paste0(surfaces[s],"r",paste0("Z",r),"_",rownames(summary_cur_ndj)[(1:n_parms)+(n_parms*(r-2))])
  } else {
    samps_cur_ndj = cbind(samps_cur_ndj,res$samples)
    colnames(samps_cur_ndj)[(1:n_parms)+(n_parms*(r-2))] = 
      paste0(surfaces[s],"r",paste0("Z",r),"_",colnames(samps_cur_ndj)[(1:n_parms)+(n_parms*(r-2))])
    
    summary_cur_ndj = rbind(summary_cur_ndj,res$hm_out$summary$all.chains)
    rownames(summary_cur_ndj)[(1:n_parms)+(n_parms*(r-2))] = 
      paste0(surfaces[s],"r",paste0("Z",r),"_",rownames(summary_cur_ndj)[(1:n_parms)+(n_parms*(r-2))])
  }
}
end.time = Sys.time()
cat("total time to get samples for",n_parms*4,"parameters:",
    round((end.time-start.time),2),"minutes.\n")

# check convergence and mixing
effectiveSize(samps_cur_ndj)
raftery.diag(samps_cur_ndj)

# save these samples
# saveRDS(samps_cur_ndj, file = "./saved_data/samps_allrlocs_ndj_wim2018.Rdata")
# # read in samples as saved (from save Rdata file)
samps_cur_ndj = readRDS("./saved_data/samps_allrlocs_ndj_wim2018.Rdata")

# look at summary
summary_cur_ndj
rownames(summary_cur_ndj)


# plot bayes estimates (from both players!) on one plot ####
# => present at rsch meeting

# example
# plots of bayes estimates (posterior means) with receipt location ####
fdp_bayes_ests = apply(samps_cur,2,mean)

plot_ests_two_players = function(allsamps_srp, setting, player = c("None","Specified"),
                     player_titles = TRUE,
                     keep_legend_fill = "colorbar", keep_legend_col = "legend",
                     cp_lims = c(0,0.57),r_zone = 0) {

  p1_fitted = round(c(0,colMeans(allsamps_srp[,str_c(setting,"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"))]),0),2)
  p2_fitted = round(c(0,colMeans(allsamps_srp[,str_c(setting,"_",c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"))]),0),2)
  
  # # plotting post for players directly side by side
  # ggarrange(court_cpmap(fed_fitted,cp_lims = cp_lims,r_zone = r_zone)+
  #             labs(title="Federer estimates")+labs(fill=NULL),
  #           court_cpmap(nad_fitted,cp_lims = cp_lims,r_zone = r_zone)+
  #             labs(title="Nadal estimates"),ncol = 2,
  #           common.legend = T,legend="bottom")
  
  # change this to court_cp (rather than ggarrange)
  # to get one common legend among many plots
  if (player[1] == "F") {
    return(court_cpmap(fed_fitted,cp_lims = cp_lims,r_zone = r_zone)+
              labs(title="Federer estimates")+labs())
  }
  if (player[1] == "N") {
    return(court_cpmap(nad_fitted,cp_lims = cp_lims,r_zone = r_zone)+
              labs(title="Nadal estimates")+labs())
  } else if (player_titles) {
    return(ggarrange(
      court_cpmap(p1_fitted,cp_lims = cp_lims,r_zone = r_zone)+
        labs(title=paste(player[1],"estimates"))+
        guides(fill = keep_legend_fill, color = keep_legend_col),
      court_cpmap(p2_fitted,cp_lims = cp_lims,r_zone = r_zone)+
        labs(title=paste(player[2],"estimates"))+
        guides(fill = keep_legend_fill, color = keep_legend_col),
      common.legend = T,legend = "bottom")
    )
  } else {
    return(ggarrange(
        court_cpmap(p1_fitted,cp_lims = cp_lims,r_zone = r_zone)+
        #+labs(title=paste(player[1],"estimates"))
        guides(fill = keep_legend_fill, color = keep_legend_col),
        court_cpmap(p2_fitted,cp_lims = cp_lims,r_zone = r_zone)+
        #+labs(title=paste(player[2],"estimates"))
        guides(fill = keep_legend_fill, color = keep_legend_col),
      common.legend = T,legend = "bottom")
    )
    
  }
}

# Federer vs. del Potro
fdp_us09_ests_plot = ggarrange(
plot_ests_two_players(allsamps_srp = samps_cur, setting = "hard.courtrZ5",
          player = c("Federer","Del Potro"),cp_lims = c(0,0.5),
          player_titles = T, keep_legend_fill = F,keep_legend_col = F,
          # enter receipt zone
          r_zone = 5),
plot_ests_two_players(allsamps_srp = samps_cur, setting = "hard.courtrZ4",
          player = c("Federer","Del Potro"),cp_lims = c(0,0.5),
          player_titles = F, keep_legend_fill = F,keep_legend_col = F,r_zone = 4),
plot_ests_two_players(allsamps_srp = samps_cur, setting = "hard.courtrZ3",
          player = c("Federer","Del Potro"),cp_lims = c(0,0.5),
          player_titles = F, keep_legend_fill = F,keep_legend_col = F,
          # enter receipt zone
          r_zone = 3),
plot_ests_two_players(allsamps_srp = samps_cur, setting = "hard.courtrZ2",
          player = c("Federer","Del Potro"),cp_lims = c(0,0.5),
          player_titles = F,
          # enter receipt zone
          r_zone = 2),
nrow = 4, common.legend = T, legend = "bottom"
)
fdp_us09_ests_plot

# Nadal vs. Djokovic
ndj_ests_plot = ggarrange(
plot_ests_two_players(allsamps_srp = samps_cur_ndj, setting = "grassrZ5",
          player = c("Nadal","Djokovic"),cp_lims = c(0,0.5),
          player_titles = T, keep_legend_fill = F,keep_legend_col = F,
          # enter receipt zone
          r_zone = 5),
plot_ests_two_players(allsamps_srp = samps_cur_ndj, setting = "grassrZ4",
          player = c("Nadal","Djokovic"),cp_lims = c(0,0.5),
          player_titles = F, keep_legend_fill = F,keep_legend_col = F,r_zone = 4),
plot_ests_two_players(allsamps_srp = samps_cur_ndj, setting = "grassrZ3",
          player = c("Nadal","Djokovic"),cp_lims = c(0,0.5),
          player_titles = F, keep_legend_fill = F,keep_legend_col = F,
          # enter receipt zone
          r_zone = 3),
plot_ests_two_players(allsamps_srp = samps_cur_ndj, setting = "grassrZ2",
          player = c("Nadal","Djokovic"),cp_lims = c(0,0.5),
          player_titles = F,
          # enter receipt zone
          r_zone = 2),
nrow = 4, common.legend = T, legend = "bottom"
)
ndj_ests_plot

# put them together for fun/possible ease of presenting
ggarrange(ndj_ests_plot,fdp_us09_ests_plot,
          ncol = 2) %>% 
  annotate_figure(left = "Wimbledon Semi 2018 (Grass)",
                  right = "U.S. Open Final 2009 (Hard Court)")


# compare sides
side_comp = function(samps_cur, settings){

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
  latex_tbl = xtable::xtable(side_tbl, digits = 3,
                 caption = "95% posterior probability intervals on right-left side probabilities",
                 label = "side_pref_cis_sig")
  
  side_tbl_plus_means = cbind(cbind(round(cis_f,2),sigs_f,ifelse(means_rl_f>0,"right","left")),
                              cbind(cis_n,sigs_n,ifelse(means_rl_n>0,"right","left")))
  colnames(side_tbl_plus_means) = c("lwr","upr","sig","side","lwr","upr","sig","side")
  
  return(list(cis_p1 = cis_f, cis_p1 = cis_n,
              side_tbl = side_tbl,
              side_tbl_latex = latex_tbl,
              side_tbl_plus_means = side_tbl_plus_means
  ))
}

side_comp(samps_cur,settings = str_c("hard.courtrZ",2:5))
side_comp(samps_cur_ndj,settings = str_c("grassrZ",2:5))


# zone specific differences #####
# plus plotting
# format: recieved from _ Hit to _ (r_h_)
samps_cur = fed_vs_fed_res$samps_cur

ratio_of_odds = function(pi1,pi2){
  log( (pi1/(1-pi1))/(pi2/(1-pi2)) )
}

# in terms of log ratio of odds
lor_pis = cbind(r2h2 = ratio_of_odds(samps_cur[,"hard.courtrZ2_pi1[1]"],samps_cur[,"hard.courtrZ2_pi2[1]"]),
                r2h3 = ratio_of_odds(samps_cur[,"hard.courtrZ2_pi1[2]"],samps_cur[,"hard.courtrZ2_pi2[2]"]),
                r2h4 = ratio_of_odds(samps_cur[,"hard.courtrZ2_pi1[3]"],samps_cur[,"hard.courtrZ2_pi2[3]"]),
                r2h5 = ratio_of_odds(samps_cur[,"hard.courtrZ2_pi1[4]"],samps_cur[,"hard.courtrZ2_pi2[4]"]),

                r3h2 = ratio_of_odds(samps_cur[,"hard.courtrZ3_pi1[1]"],samps_cur[,"hard.courtrZ3_pi2[1]"]),
                r3h3 = ratio_of_odds(samps_cur[,"hard.courtrZ3_pi1[2]"],samps_cur[,"hard.courtrZ3_pi2[2]"]),
                r3h4 = ratio_of_odds(samps_cur[,"hard.courtrZ3_pi1[3]"],samps_cur[,"hard.courtrZ3_pi2[3]"]),
                r3h5 = ratio_of_odds(samps_cur[,"hard.courtrZ3_pi1[4]"],samps_cur[,"hard.courtrZ3_pi2[4]"]),

                r4h2 = ratio_of_odds(samps_cur[,"hard.courtrZ4_pi1[1]"],samps_cur[,"hard.courtrZ4_pi2[1]"]),
                r4h3 = ratio_of_odds(samps_cur[,"hard.courtrZ4_pi1[2]"],samps_cur[,"hard.courtrZ4_pi2[2]"]),
                r4h4 = ratio_of_odds(samps_cur[,"hard.courtrZ4_pi1[3]"],samps_cur[,"hard.courtrZ4_pi2[3]"]),
                r4h4 = ratio_of_odds(samps_cur[,"hard.courtrZ4_pi1[4]"],samps_cur[,"hard.courtrZ4_pi2[4]"]),
                
                r5h2 = ratio_of_odds(samps_cur[,"hard.courtrZ5_pi1[1]"],samps_cur[,"hard.courtrZ5_pi2[1]"]),
                r5h3 = ratio_of_odds(samps_cur[,"hard.courtrZ5_pi1[2]"],samps_cur[,"hard.courtrZ5_pi2[2]"]),
                r5h4 = ratio_of_odds(samps_cur[,"hard.courtrZ5_pi1[3]"],samps_cur[,"hard.courtrZ5_pi2[3]"]),
                r5h5 = ratio_of_odds(samps_cur[,"hard.courtrZ5_pi1[4]"],samps_cur[,"hard.courtrZ5_pi2[4]"]))

# get Bayes estimates (BEse) of log odds ratios
ests=apply(lor_pis,2,mean)

# and HPD intervals
ests_quantiles = apply(lor_pis,2,quantile, c(0.025,0.975))
ests_hpd = apply(as.mcmc(lor_pis),2,function(x) {HPDinterval(as.mcmc(x))})

# matrices of bayes estimates
ests_mat = rbind(z2=ests[1:4],z3=ests[5:8],z4=ests[9:12],z5=ests[13:16])
# hce_mat = rbind(z2=hce[1:4],z3=hce[5:8],z4=hce[9:12],z5=hce[13:16])
# ge_mat = rbind(z2=ge[1:4],z3=ge[5:8],z4=ge[9:12],z5=ge[13:16])
colnames(ests_mat) = str_c("z",2:5)
# colnames(hce_mat) = str_c("z",2:5)
# colnames(ge_mat) = str_c("z",2:5)
# columns: hit to
# rows: received from

# get significance
ests_sig = apply(ests_hpd,2,function(vec) {
  (vec[1]>0 & vec[2]>0) | (vec[1]<0 & vec[2]<0)
})


ests_df = as.data.frame(as.table(ests_mat)) %>% 
  rename(Lands= Var1,Hit.to = Var2) %>% 
  mutate(sig = ests_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
  mutate(sig = ifelse(sig,sig,NA))
# hce_df = as.data.frame(as.table(hce_mat)) %>% 
#   rename(Lands= Var1,Hit.to = Var2) %>% 
#   mutate(sig = hce_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
#   mutate(sig = ifelse(sig,sig,NA))
# ge_df = as.data.frame(as.table(ge_mat)) %>% 
#   rename(Lands= Var1,Hit.to = Var2) %>% 
#   mutate(sig = ge_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
#   mutate(sig = ifelse(sig,sig,NA))

# TODO: test this on the odds scale as well

# Define the color gradient
colfunc <- colorRampPalette(c("blue", "white", "red"))
grain = 5

# Create heat maps (modified from AI generated code)
# NOTE: could also change text to intervals rather than estimates
ggplot(data = ests_df, aes(x = Lands, y = Hit.to, fill = Freq)) +
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
          plot.caption = element_text(hjust = 0.5)) +
    labs(fill = expression("log("~O[FedvNad]/O[FedvDP]~")"),col = NULL,
         title = "Fed vs Fed log(Odds Ratio) estimates and significance",
         caption = "Comparing Federer playing Nadal in AO vs. playing del Potro in US Open")


# Nadal vs. Nadal
samps_cur = nad_vs_nad_res$samps_cur

ratio_of_odds = function(pi1,pi2){
  log( (pi1/(1-pi1))/(pi2/(1-pi2)) )
}

# in terms of log ratio of odds
lor_pis = cbind(r2h2 = ratio_of_odds(samps_cur[,"grassrZ2_pi1[1]"],samps_cur[,"grassrZ2_pi2[1]"]),
                r2h3 = ratio_of_odds(samps_cur[,"grassrZ2_pi1[2]"],samps_cur[,"grassrZ2_pi2[2]"]),
                r2h4 = ratio_of_odds(samps_cur[,"grassrZ2_pi1[3]"],samps_cur[,"grassrZ2_pi2[3]"]),
                r2h5 = ratio_of_odds(samps_cur[,"grassrZ2_pi1[4]"],samps_cur[,"grassrZ2_pi2[4]"]),

                r3h2 = ratio_of_odds(samps_cur[,"grassrZ3_pi1[1]"],samps_cur[,"grassrZ3_pi2[1]"]),
                r3h3 = ratio_of_odds(samps_cur[,"grassrZ3_pi1[2]"],samps_cur[,"grassrZ3_pi2[2]"]),
                r3h4 = ratio_of_odds(samps_cur[,"grassrZ3_pi1[3]"],samps_cur[,"grassrZ3_pi2[3]"]),
                r3h5 = ratio_of_odds(samps_cur[,"grassrZ3_pi1[4]"],samps_cur[,"grassrZ3_pi2[4]"]),

                r4h2 = ratio_of_odds(samps_cur[,"grassrZ4_pi1[1]"],samps_cur[,"grassrZ4_pi2[1]"]),
                r4h3 = ratio_of_odds(samps_cur[,"grassrZ4_pi1[2]"],samps_cur[,"grassrZ4_pi2[2]"]),
                r4h4 = ratio_of_odds(samps_cur[,"grassrZ4_pi1[3]"],samps_cur[,"grassrZ4_pi2[3]"]),
                r4h4 = ratio_of_odds(samps_cur[,"grassrZ4_pi1[4]"],samps_cur[,"grassrZ4_pi2[4]"]),
                
                r5h2 = ratio_of_odds(samps_cur[,"grassrZ5_pi1[1]"],samps_cur[,"grassrZ5_pi2[1]"]),
                r5h3 = ratio_of_odds(samps_cur[,"grassrZ5_pi1[2]"],samps_cur[,"grassrZ5_pi2[2]"]),
                r5h4 = ratio_of_odds(samps_cur[,"grassrZ5_pi1[3]"],samps_cur[,"grassrZ5_pi2[3]"]),
                r5h5 = ratio_of_odds(samps_cur[,"grassrZ5_pi1[4]"],samps_cur[,"grassrZ5_pi2[4]"]))

# get Bayes estimates (BEse) of log odds ratios
ests=apply(lor_pis,2,mean)

# and HPD intervals
ests_quantiles = apply(lor_pis,2,quantile, c(0.025,0.975))
ests_hpd = apply(as.mcmc(lor_pis),2,function(x) {HPDinterval(as.mcmc(x))})

# matrices of bayes estimates
ests_mat = rbind(z2=ests[1:4],z3=ests[5:8],z4=ests[9:12],z5=ests[13:16])
# hce_mat = rbind(z2=hce[1:4],z3=hce[5:8],z4=hce[9:12],z5=hce[13:16])
# ge_mat = rbind(z2=ge[1:4],z3=ge[5:8],z4=ge[9:12],z5=ge[13:16])
colnames(ests_mat) = str_c("z",2:5)
# colnames(hce_mat) = str_c("z",2:5)
# colnames(ge_mat) = str_c("z",2:5)
# columns: hit to
# rows: received from

# get significance
ests_sig = apply(ests_hpd,2,function(vec) {
  (vec[1]>0 & vec[2]>0) | (vec[1]<0 & vec[2]<0)
})


ests_df = as.data.frame(as.table(ests_mat)) %>% 
  rename(Lands= Var1,Hit.to = Var2) %>% 
  mutate(sig = ests_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
  mutate(sig = ifelse(sig,sig,NA))
# hce_df = as.data.frame(as.table(hce_mat)) %>% 
#   rename(Lands= Var1,Hit.to = Var2) %>% 
#   mutate(sig = hce_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
#   mutate(sig = ifelse(sig,sig,NA))
# ge_df = as.data.frame(as.table(ge_mat)) %>% 
#   rename(Lands= Var1,Hit.to = Var2) %>% 
#   mutate(sig = ge_sig[c(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)]) %>% 
#   mutate(sig = ifelse(sig,sig,NA))

# TODO: test this on the odds scale as well

# Define the color gradient
colfunc <- colorRampPalette(c("blue", "white", "red"))
grain = 5

# Create heat maps (modified from AI generated code)
# NOTE: could also change text to intervals rather than estimates
ggplot(data = ests_df, aes(x = Lands, y = Hit.to, fill = Freq)) +
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
          plot.caption = element_text(hjust = 0.5)) +
    labs(fill = expression("log("~O[NadvFed]/O[NadvDj]~")"),col = NULL,
         title = "Nadal vs Nadal log(Odds Ratio) estimates and significance",
         caption = "Comparing Nadal playing Federer in Wimbledon 2019 vs. playing Djokovic in Wimbledon 2018")

