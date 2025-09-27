# Vizualizations for SRC
# and other Research Presentations

library(tidyverse)
library(ggpubr)

# read in cp heat map functionality
source("MultinomialProbs.R")

# motivating example(s): ####
# Nadal
ggarrange(
  court_cpmap(rh_cp(adat_clean)[[1]][2,1:6],lz_size = 3,rz_size = 4,
              cp_lims = c(0,0.56))+labs(fill=NULL),
  court_cpmap(rh_cp(adat_clean)[[1]][5,1:6],lz_size = 3,rz_size = 4,
              cp_lims = c(0,0.56))+labs(fill=NULL),
  ncol = 2,common.legend = T,legend = "bottom"
)

# Fed
ggarrange(
  court_cpmap(rh_cp(adat_clean)[[2]][2,1:6],lz_size = 3,rz_size = 4,
              cp_lims = c(0,0.56))+labs(fill=NULL),
  court_cpmap(rh_cp(adat_clean)[[2]][5,1:6],lz_size = 3,rz_size = 4,
              cp_lims = c(0,0.56))+labs(fill=NULL),
  ncol = 2,common.legend = T,legend = "bottom"
)

# add receipt zones
# "this is real data, with given receipt zones"
ggarrange(
  court_cpmap(rh_cp(adat_clean)[[1]][2,1:6],lz_size = 3,rz_size = 4,
              r_zone = 2,cp_lims = c(0,0.56))+labs(fill=NULL),
  
  court_cpmap(rh_cp(adat_clean)[[1]][5,1:6],lz_size = 3,rz_size = 4,
              r_zone = 5,cp_lims = c(0,0.56)),
  ncol = 2,common.legend = T,legend = "bottom"
) %>% annotate_figure(top = "Conditional Shot Location Probabilites for Nadal Given Receipt Location")

# comparing given rz
rz = 3
ggarrange(
  court_cpmap(rh_cp(adat_clean)[[1]][rz,1:6],lz_size = 3,rz_size = 4,
              r_zone = rz,cp_lims = c(0,0.56))+labs(fill=NULL),
  
  court_cpmap(rh_cp(adat_clean)[[2]][rz,1:6],lz_size = 3,rz_size = 4,
              r_zone = rz,cp_lims = c(0,0.56)),
  ncol = 2,common.legend = T,legend = "bottom"
) %>% annotate_figure("Nadal (left) vs. Fed (right) on Hard Court")
# very interesting how different the players are in different situations,
# such as 1rz3

# different for 2rz3
ggarrange(
  court_cpmap(rh_cp(fdat_clean)[[1]][rz,1:6],lz_size = 3,rz_size = 4,
              r_zone = rz,cp_lims = c(0,0.52))+labs(fill=NULL),
  
  court_cpmap(rh_cp(fdat_clean)[[2]][rz,1:6],lz_size = 3,rz_size = 4,
              r_zone = rz,cp_lims = c(0,0.52)),
  ncol = 2,common.legend = T,legend = "bottom"
)%>% annotate_figure("Nadal (left) vs. Fed (right) on Clay")


# Results ####
# read in posterior samples
allsamps_srp = readRDS("./saved_data/allsamps_srploop.Rdata")

# functions for plotting results, like side by side posterior dists
# and bayes estimate heatmaps
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

posts_and_heatmaps = function(allsamps_srp,setting,title,r_zone=0,
                              xlims=c(0.0,0.7),cp_lims = c(0,0.57)) {
  fed_posts = plot_4post_dists(allsamps_cur = allsamps_srp,
                   parms4 = str_c(setting,"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]")),
                   legend_labels = str_c("Z",2:5), plot_xlims = xlims,
                   plot_title = "Federer posterior distributions")
  nad_posts = plot_4post_dists(allsamps_cur = allsamps_srp,
                   parms4 = str_c(setting,"_",c("pi2[1]","pi2[2]","pi2[3]","pi2[4]")),
                   legend_labels = str_c("Z",2:5), plot_xlims = xlims,
                   plot_title = "Nadal posterior distributions")
  
  fed_fitted = round(c(0,colMeans(allsamps_srp[,str_c(setting,"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"))]),0),2)
  nad_fitted = round(c(0,colMeans(allsamps_srp[,str_c(setting,"_",c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"))]),0),2)
  
  # plotting post for players directly side by side
  ggarrange(ggarrange(fed_posts,nad_posts,ncol =2,legend = "bottom",
                    common.legend = T),
            ggarrange(court_cpmap(fed_fitted,cp_lims = cp_lims,r_zone = r_zone)+
                        labs(title="Federer estimates")+labs(fill=NULL),
                      court_cpmap(nad_fitted,cp_lims = cp_lims,r_zone = r_zone)+
                        labs(title="Nadal estimates"),ncol = 2,
                      common.legend = T,legend="bottom"),
            nrow = 2) %>% annotate_figure(top = paste0("Bayes Estimates: ",title))
}

# rz5 (s1)
posts_and_heatmaps(allsamps_srp,setting = "1rZ5",r_zone = 5,
                   title = "Hard Court Received Z5")
# rz2 (s1)
posts_and_heatmaps(allsamps_srp,setting = "1rZ2",r_zone = 2,xlims = c(0,0.6),
                   title = "Hard Court Received Z2",cp_lims = c(0,0.42))



# plots of bayes estimates (posterior means) with significance ####
all_bayes_ests = apply(allsamps_srp,2,mean)

plot_ests = function(allsamps_srp, setting, player = "F",
                     cp_lims = c(0,0.57),r_zone = 0) {

  fed_fitted = round(c(0,colMeans(allsamps_srp[,str_c(setting,"_",c("pi1[1]","pi1[2]","pi1[3]","pi1[4]"))]),0),2)
  nad_fitted = round(c(0,colMeans(allsamps_srp[,str_c(setting,"_",c("pi2[1]","pi2[2]","pi2[3]","pi2[4]"))]),0),2)
  
  # # plotting post for players directly side by side
  # ggarrange(court_cpmap(fed_fitted,cp_lims = cp_lims,r_zone = r_zone)+
  #             labs(title="Federer estimates")+labs(fill=NULL),
  #           court_cpmap(nad_fitted,cp_lims = cp_lims,r_zone = r_zone)+
  #             labs(title="Nadal estimates"),ncol = 2,
  #           common.legend = T,legend="bottom")
  
  # change this to court_cp (rather than ggarrange)
  # to get one common legend among many plots
  if (player == "F") {
    return(court_cpmap(fed_fitted,cp_lims = cp_lims,r_zone = r_zone)+
              labs(title="Federer estimates")+labs())
  }
  if (player == "N") {
    return(court_cpmap(nad_fitted,cp_lims = cp_lims,r_zone = r_zone)+
              labs(title="Nadal estimates")+labs())
  }
}

# Federer
ggarrange(
  plot_ests(allsamps_srp,setting = "1rZ5",cp_lims =c(0,0.45), r_zone = 5)+
    labs(title = "Hard Court")+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "2rZ5",cp_lims =c(0,0.45), r_zone = 5)+labs(title = "Clay")+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "3rZ5",cp_lims =c(0,0.45), r_zone = 5)+labs(title = "Grass")+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "1rZ4",cp_lims =c(0,0.45), r_zone = 4)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "2rZ4",cp_lims =c(0,0.45), r_zone = 4)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "3rZ4",cp_lims =c(0,0.45), r_zone = 4)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "1rZ3",cp_lims =c(0,0.45), r_zone = 3)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "2rZ3",cp_lims =c(0,0.45), r_zone = 3)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "3rZ3",cp_lims =c(0,0.45), r_zone = 3)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "1rZ2",cp_lims =c(0,0.45), r_zone = 2)+
    labs(title = NULL)+geom_path(aes(x = c(11.985, 18.4), y = c(5.485, 9.61),
                      col = "Significant"),lty = 1,lwd = 1.4) + # z1/z2 line
    geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 8.55),
                col = "Significant"),
              lty = 1,lwd = 1.4) + # z2/z3 line
    scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
                                              "Significant"="green"))+
    labs(fill = expression(hat(pi)[ks1r])),
  plot_ests(allsamps_srp,setting = "2rZ2",cp_lims =c(0,0.45), r_zone = 2)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "3rZ2",cp_lims =c(0,0.45), r_zone = 2)+labs(title = NULL)+guides(color = F,fill=F),
  nrow = 4,ncol = 3,legend = "bottom",common.legend = T
) %>% annotate_figure(top = "Federer Estimates")

# Nadal
ggarrange(
  plot_ests(allsamps_srp,setting = "1rZ5",player = "N", r_zone = 5)+
    labs(title = "Hard Court")+ 
    geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 2.42),
                            col = "Significant"),
                        lty = 1,lwd = 1.4) + # z4/z5 line
    geom_path(aes(x = c(11.985,18.4), y = c(5.485, 1.37),
                  col = "Significant"),
              lty = 1,lwd = 1.4) + # z5/z6 line
    scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
                                              "Significant"="green"))+
    labs(fill = expression(hat(pi)[ks2r])),
  plot_ests(allsamps_srp,setting = "2rZ5",player = "N", r_zone = 5)+labs(title = "Clay")+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "3rZ5",player = "N", r_zone = 5)+
    labs(title = "Grass")+ 
    geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 2.42),
                            col = "Significant"),
                        lty = 1,lwd = 1.4) + # z4/z5 line
    geom_path(aes(x = c(11.985,18.4), y = c(5.485, 1.37),
                  col = "Significant"),
              lty = 1,lwd = 1.4) + # z5/z6 line
    scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
                                              "Significant"="green")),
  plot_ests(allsamps_srp,setting = "1rZ4",player = "N", r_zone = 4)+
    labs(title = NULL)+ 
    geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 2.42),
                            col = "Significant"),
                        lty = 1,lwd = 1.4) + # z4/z5 line
    geom_path(aes(x = c(11.985,18.4), y = c(5.485, 1.37),
                  col = "Significant"),
              lty = 1,lwd = 1.4) + # z5/z6 line
    scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
                                              "Significant"="green")),
  plot_ests(allsamps_srp,setting = "2rZ4",player = "N", r_zone = 4)+
    labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "3rZ4",player = "N", r_zone = 4)+
    labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "1rZ3",player = "N", r_zone = 3)+
    labs(title = NULL)+ 
    geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 2.42),
                            col = "Significant"),
                        lty = 1,lwd = 1.4) + # z4/z5 line
    geom_path(aes(x = c(11.985,18.4), y = c(5.485, 1.37),
                  col = "Significant"),
              lty = 1,lwd = 1.4) + # z5/z6 line
    scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
                                              "Significant"="green")),
  plot_ests(allsamps_srp,setting = "2rZ3",player = "N", r_zone = 3)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "3rZ3",player = "N", r_zone = 3)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "1rZ2",player = "N", r_zone = 2)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "2rZ2",player = "N", r_zone = 2)+labs(title = NULL)+guides(color = F,fill=F),
  plot_ests(allsamps_srp,setting = "3rZ2",player = "N", r_zone = 2)+labs(title = NULL)+guides(color = F,fill=F),
  nrow = 4,ncol = 3,common.legend = T,legend = "bottom"
) %>% annotate_figure(top = "Nadal Estimates")


# after we see all plots for both players, this is/can be a great segway into player diffs!

  
