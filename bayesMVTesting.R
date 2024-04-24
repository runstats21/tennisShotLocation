# Significance Testing

# Is the odds ratio of Federer:Nadal significantly different from 1
# Separately for each surface
# I) Overall: Multivariate, product of four 4 dimensional vectors
# II) By received location (AKA "by row"), 4 dimensional MV test
# III) Cell by Cell (16 cells for each surface to compare)

# Practical for performing computation: Start with II,
# then expand to overall test

# but in experiment: sequential (I, II, III),
# only proceeding if previous set of tests is significant


# load libraries
library(tidyverse)

# load MCMC samples
samps = readRDS("./saved_data/allsamps_srploop.Rdata")
fdp_samps_hc = readRDS("./saved_data/samps_allrlocs_fdp_us2009.Rdata")
ndj_samps_grass = readRDS("./saved_data/")
fvf_samps_hc = readRDS("./saved_data/samps_allrlocs_fnvsfdp_hc.Rdata")

# dim(samps)
# n_samps = dim(samps)[1]
# colnames(samps) 
# 13 (m+12 pis) parms for each of the 4 r locations by 3 surfaces:
# 13x4X3 = 156

# load cleaned data + tables of counts
source("Cleaning.R")

# set up test II for surface 1 received in zone 2 ####
# example
# specify surface here (1 = hc, 2 = clay, 3 = grass)
s = 1 # numeric number
# hard court given receive zone (in quotes or numeric)
r_zone = 2
cur_setting = paste0(s,"r",paste0("Z",r_zone))
paste0(cur_setting,"_pi2[1]")
x1_cur = fed_zct_hr[[s]][paste0("Z",r_zone),] # access Z2 row = counts went lands in Z2
pi2_cur = samps[,c(paste0(cur_setting,"_pi2[1]"),paste0(cur_setting,"_pi2[2]"),
                   paste0(cur_setting,"_pi2[3]"),paste0(cur_setting,"_pi2[4]"))]
pi1_cur = samps[,c(paste0(cur_setting,"_pi1[1]"),paste0(cur_setting,"_pi1[2]"),
                   paste0(cur_setting,"_pi1[3]"),paste0(cur_setting,"_pi1[4]"))]
pi0_cur = samps[,c(paste0(cur_setting,"_pi[1]"),paste0(cur_setting,"_pi[2]"),
                   paste0(cur_setting,"_pi[3]"),paste0(cur_setting,"_pi[4]"))]
m_cur = samps[,c(paste0(cur_setting,"_m"))]

# get example case of gams
ratio_of_odds = function(pi1,pi2){
  return( (pi1/(1-pi1))/(pi2/(1-pi2)) )
}
gams_cur = ratio_of_odds(pi1_cur,pi2_cur)

# for a single receipt location (k), p(gamma|eta, y)
# may want to change y's to x's for consistency
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
# get null values (gam = vector of 1s)
null_vals_cur = post_full_conditional(
  gam = cbind(1,1,1,1),
  pi2 = pi2_cur,m=m_cur,x1=x1_cur,pi_0=pi0_cur
)
# save null values vector and median value
saveRDS(null_vals_cur, file = paste0("./saved_data/null_values_s",cur_setting,".Rdata"))
saveRDS(median(null_vals_cur), file = paste0("./saved_data/null_median_s",cur_setting,".Rdata"))

# build NxN matrix of posterior density values for gamma_r=1
# in order to test joint of all (4) gammas for a given r
# and, via product, test joint of all 16 gammas for a given surface
N_MCMC = dim(samps)[1]
gam_mat = matrix(NA,nrow = N_MCMC,ncol = N_MCMC)
N_MCMC = 1 # for testing
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

saveRDS(gam_mat, file = paste0("./saved_data/gam_mat_s",cur_setting,".Rdata"))

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
saveRDS(contour_prob, file = paste0("./saved_data/ctr_prob_s",cur_setting,".Rdata"))
cat("contour prob saved (value:", contour_prob, ")\n")