# Functions for Posterior Inference via Multinomial Dirichlet Model

# libraries
library(tidyverse)
library(nimble)
library(coda)

# helper functions #
ratio_of_odds = function(pi1,pi2){
  return( (pi1/(1-pi1))/(pi2/(1-pi2)) )
}

remove_trailing_slash <- function(str) {
  sub("/$", "", str)
}


# Inference functions #
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


post_full_conditional = function(gam = cbind(1,1,1,1), pi2, pi_0,m,x1) {
  # default = null values for odds ratios (vector of 1s)
  detJ = ( (pi2[,1]/(1-pi2[,1]))/(1+(gam[,1]*pi2[,1]/(1-pi2[,1])))^2 )*
          ( (pi2[,2]/(1-pi2[,2]))/(1+(gam[,2]*pi2[,2]/(1-pi2[,2])))^2 )*
          ( (pi2[,3]/(1-pi2[,3]))/(1+(gam[,3]*pi2[,3]/(1-pi2[,3])))^2 )*
          ( (pi2[,4]/(1-pi2[,4]))/(1+(gam[,4]*pi2[,4]/(1-pi2[,4])))^2 )
        
  # ratio of gamma functions
  gam_constants = gamma(m*rowSums(pi_0)+sum(x1))/
    # product of (H/J = num hit.to zones = 4) 4 gamma functions
    ( gamma(m*pi_0[,1] + x1[1])*gamma(m*pi_0[,2] + x1[2])*
        gamma(m*pi_0[,3] + x1[3])*gamma(m*pi_0[,4] + x1[4]))
  
  # ISSUE: for large values of m*(pi_0)+x1 gamma function returns Inf
  # so, will use lgamma, and then back transform
  lgam_constants = lgamma(m*rowSums(pi_0)+sum(x1))-
    # product of (H/J = num hit.to zones = 4) 4 gamma functions:
    # = the sum of the log gammas
    ( lgamma(m*pi_0[,1] + x1[1])+lgamma(m*pi_0[,2] + x1[2])+
        lgamma(m*pi_0[,3] + x1[3])+lgamma(m*pi_0[,4] + x1[4]))
  
  # product of 4 transformed x variables
  dir_kernel = (
    (( (gam[,1]*pi2[,1]/(1-pi2[,1]))/( 1+(gam[,1]*pi2[,1]/(1-pi2[,1])) ) )^(m*pi_0[,1] + x1[1]-1))*
      (( (gam[,2]*pi2[,2]/(1-pi2[,2]))/( 1+(gam[,2]*pi2[,2]/(1-pi2[,2])) ) )^(m*pi_0[,2] + x1[2]-1))*
      (( (gam[,3]*pi2[,3]/(1-pi2[,3]))/( 1+(gam[,3]*pi2[,3]/(1-pi2[,3])) ) )^(m*pi_0[,3] + x1[3]-1))*
      (( (gam[,4]*pi2[,4]/(1-pi2[,4]))/( 1+(gam[,4]*pi2[,4]/(1-pi2[,4])) ) )^(m*pi_0[,4] + x1[4]-1))
  ) 
  
  detJ * exp(lgam_constants) * dir_kernel
}

mv_test_all_rlocs = function(samps, p1_counts_mat, surface_name, players_abbrev,
                             zones = c(2:5), save_dir = "./saved_data") {
  save_dir_clean = remove_trailing_slash(save_dir)
  s = surface_name
  
  for (r_zone in zones) { # todo: change to 2:5
    cur_setting = paste0(s,"r",paste0("Z",r_zone))
    cat("current setting:", paste0(cur_setting,players_abbrev), "\n")
    
    x1_cur = p1_counts_mat[paste0("Z",r_zone),]
    # x1_cur = fed_counts[paste0("Z",r_zone),]
    # redefine samps, and then use them
    pi2_cur = samps[,c(paste0(cur_setting,"_pi2[1]"),paste0(cur_setting,"_pi2[2]"),
                       paste0(cur_setting,"_pi2[3]"),paste0(cur_setting,"_pi2[4]"))]
    pi1_cur = samps[,c(paste0(cur_setting,"_pi1[1]"),paste0(cur_setting,"_pi1[2]"),
                       paste0(cur_setting,"_pi1[3]"),paste0(cur_setting,"_pi1[4]"))]
    # for fed vs. fed, could use average p1s, or average of pi0s for both settings? not sure
    pi0_cur = samps[,c(paste0(cur_setting,"_pi[1]"),paste0(cur_setting,"_pi[2]"),
                       paste0(cur_setting,"_pi[3]"),paste0(cur_setting,"_pi[4]"))]
    m_cur = samps[,c(paste0(cur_setting,"_m"))]
    
    # get example case of gams
    ratio_of_odds = function(pi1,pi2){
      return( (pi1/(1-pi1))/(pi2/(1-pi2)) )
    }
    gams_cur = ratio_of_odds(pi1_cur,pi2_cur) # N_MCMC x 4
    
    # get null values (gam = vector of 1s)
    null_vals_cur = post_full_conditional(
      gam = cbind(1,1,1,1),
      pi2 = pi2_cur,m=m_cur,x1=x1_cur,pi_0=pi0_cur
    )
    # save null values vector and median value
    saveRDS(null_vals_cur, file = paste0(save_dir_clean, "/null_values_s",cur_setting,players_abbrev,".Rdata"))
    saveRDS(median(null_vals_cur), file = paste0(save_dir_clean, "/null_median_s",cur_setting,players_abbrev,".Rdata"))
    
    # build NxN matrix of posterior density values for gamma_r=1
    # in order to test joint of all (4) gammas for a given r
    # and, via product, test joint of all 16 gammas for a given surface
    N_MCMC = dim(samps)[1]
    gam_mat = matrix(NA,nrow = N_MCMC,ncol = N_MCMC)
    start_time = Sys.time()
    for (i in 1:N_MCMC) {
    
      gam_mat[i,] = post_full_conditional(
        gam = rbind(gams_cur[i,]),
        pi2 = pi2_cur,m=m_cur,x1=x1_cur,pi_0=pi0_cur
      )
      cat(i,"\n")
    }
    end_time = Sys.time()
    cat(end_time - start_time, "to complete", N_MCMC, "iterations\n")
    # took approximately 17-20 minutes as background job
    
    saveRDS(gam_mat, file = paste0(save_dir_clean, "/gam_mat_s",cur_setting,players_abbrev,".Rdata"))
    
    # beepr::beep()
    
    # testing out product of multiple matrices
    # eventually: want to product matrices from all r = 1,...,4 options
    # (gam_mat_load[1:10,1:10]*gam_mat_load[1:10,1:10])[1:10,1:10] 
    
    #gam_mat_load = readRDS("gam_mat_s1r2.Rdata")
    # get medians
    # apply(gam_mat_load[1:100,], 1, median)
    
    # contour probability (Bayesian p-value)
    N = 20000
    contour_prob = (1/N)*sum(apply(gam_mat[1:N,1:N], 1,
                                   function(x) {median(x)<median(null_vals_cur)}))
    
    # save contour probability
    saveRDS(contour_prob, file = paste0(save_dir_clean, "/ctr_prob_s",cur_setting,players_abbrev,".Rdata"))
    cat("contour prob saved (value:", contour_prob, ")\n")
    
  }
  beepr::beep()
}