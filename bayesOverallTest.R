# work for overall test

# testing out product of multiple matrices
# eventually: want to product matrices from all r = 1,...,4 options

# read in matrices
gam_mat_s1r2 = readRDS("./saved_data/gam_mat_s1rZ2.Rdata")
gam_mat_s1r3 = readRDS("./saved_data/gam_mat_s1rZ3.Rdata")
gam_mat_s1r4 = readRDS("./saved_data/gam_mat_s1rZ4.Rdata")
gam_mat_s1r5 = readRDS("./saved_data/gam_mat_s1rZ5.Rdata")

gam_mat_s2r2 = readRDS("./saved_data/gam_mat_s2rZ2.Rdata")
gam_mat_s2r3 = readRDS("./saved_data/gam_mat_s2rZ3.Rdata")
gam_mat_s2r4 = readRDS("./saved_data/gam_mat_s2rZ4.Rdata")
gam_mat_s2r5 = readRDS("./saved_data/gam_mat_s2rZ5.Rdata")

gam_mat_s3r2 = readRDS("./saved_data/gam_mat_s3rZ2.Rdata")
gam_mat_s3r3 = readRDS("./saved_data/gam_mat_s3rZ3.Rdata")
gam_mat_s3r4 = readRDS("./saved_data/gam_mat_s3rZ4.Rdata")
gam_mat_s3r5 = readRDS("./saved_data/gam_mat_s3rZ5.Rdata")

# and vectors of null values
null_values_s1r2 = readRDS("./saved_data/null_values_s1rZ2.Rdata")
null_values_s1r3 = readRDS("./saved_data/null_values_s1rZ3.Rdata")
null_values_s1r4 = readRDS("./saved_data/null_values_s1rZ4.Rdata")
null_values_s1r5 = readRDS("./saved_data/null_values_s1rZ5.Rdata")

null_values_s2r2 = readRDS("./saved_data/null_values_s2rZ2.Rdata")
null_values_s2r3 = readRDS("./saved_data/null_values_s2rZ3.Rdata")
null_values_s2r4 = readRDS("./saved_data/null_values_s2rZ4.Rdata")
null_values_s2r5 = readRDS("./saved_data/null_values_s2rZ5.Rdata")

null_values_s3r2 = readRDS("./saved_data/null_values_s3rZ2.Rdata")
null_values_s3r3 = readRDS("./saved_data/null_values_s3rZ3.Rdata")
null_values_s3r4 = readRDS("./saved_data/null_values_s3rZ4.Rdata")
null_values_s3r5 = readRDS("./saved_data/null_values_s3rZ5.Rdata")

# hard court ###

# get product of gam mats
s1_overall_gam = gam_mat_s1r2*gam_mat_s1r3*gam_mat_s1r4*gam_mat_s1r5
s2_overall_gam = gam_mat_s2r2*gam_mat_s2r3*gam_mat_s2r4*gam_mat_s2r5
s3_overall_gam = gam_mat_s3r2*gam_mat_s3r3*gam_mat_s3r4*gam_mat_s3r5

# get product of null values
s1_overall_null_vals = null_values_s1r2*null_values_s1r3*null_values_s1r4*null_values_s1r5
s2_overall_null_vals = null_values_s2r2*null_values_s2r3*null_values_s2r4*null_values_s2r5
s3_overall_null_vals = null_values_s3r2*null_values_s3r3*null_values_s3r4*null_values_s3r5

# get medians of product of gam mats
# test
median(s1_overall_null_vals)
apply(s1_overall_gam[1:10,],1,median)

N = nrow(s1_overall_gam)
overall_ctr_prob_s1 = (1/N)*sum(apply(s1_overall_gam[1:N,1:N],1,function(x) {median(x)<median(s1_overall_null_vals)}))

overall_ctr_prob_s2 = (1/N)*sum(apply(s2_overall_gam[1:N,1:N],1,function(x) {median(x)<median(s2_overall_null_vals)}))

overall_ctr_prob_s3 = (1/N)*sum(apply(s3_overall_gam[1:N,1:N],1,function(x) {median(x)<median(s3_overall_null_vals)}))

# save values
saveRDS(overall_ctr_prob_s1, file = "./saved_data/ctr_prob_overall_s1.Rdata")
cat("contour prob saved (value:", overall_ctr_prob_s1, ")\n")
saveRDS(overall_ctr_prob_s2, file = "./saved_data/ctr_prob_overall_s2.Rdata")
cat("contour prob saved (value:", overall_ctr_prob_s2, ")\n")
saveRDS(overall_ctr_prob_s3, file = "./saved_data/ctr_prob_overall_s3.Rdata")
cat("contour prob saved (value:", overall_ctr_prob_s3, ")\n")