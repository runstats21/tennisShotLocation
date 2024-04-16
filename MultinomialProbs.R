# EDA of different multinomial probabilities 
#(for ideating in preparation for Bayesian Hierarchical Model) 

# imports
library(tidyverse)
library(ggpubr)

# data of interest
source("Cleaning.R")

# clay results (french open 2005)
clay_fn = read.csv("FrenchOpen/french_open_clean.csv") %>% mutate_if(is_character,as.factor)

# hard court results
hc_fdp = read.csv("USOpen/usopen_clean.csv") %>% mutate_if(is_character,as.factor)


# CPs: ####
#hit to given received from, by player
rh_cp = function(df) {
  player_strings = as.character(unique(df$Player))
  player_strings = player_strings[!is.na(player_strings)]
  
  p1_rh = table(df$Ball.lands[df$Player == player_strings[1]],
                df$Ball.hit.to[df$Player == player_strings[1]])
  cp1 = cbind(round(p1_rh /(p1_rh  %>% rowSums()),2),row_total = (p1_rh  %>% rowSums()))
  # repeat for player 2
  p2_rh = table(df$Ball.lands[df$Player == player_strings[2]],
                df$Ball.hit.to[df$Player == player_strings[2]])
  cp2 = cbind(round(p2_rh/(p2_rh %>% rowSums()),2),row_total = (p2_rh %>% rowSums()))
  
  cat("Players:", player_strings,"\n")
  list(cp1,cp2)
  
}

# received and stroke type
rs_cp = function(df) {
  player_strings = as.character(unique(df$Player))
  player_strings = player_strings[!is.na(player_strings)]
  
  p1_rh = table(df$Ball.lands[df$Player == player_strings[1]],
                df$Stroke.Type[df$Player == player_strings[1]])
  cp1 = cbind(round(p1_rh /(p1_rh  %>% rowSums()),2),row_total = (p1_rh  %>% rowSums()))
  # repeat for player 2
  p2_rh = table(df$Ball.lands[df$Player == player_strings[2]],
                df$Stroke.Type[df$Player == player_strings[2]])
  cp2 = cbind(round(p2_rh/(p2_rh %>% rowSums()),2),row_total = (p2_rh %>% rowSums()))
  cat("Players:", player_strings,"\n")
  list(cp1,cp2)
  
}

# hit to given stroke type
sh_cp = function(df) {
  player_strings = as.character(unique(df$Player))
  player_strings = player_strings[!is.na(player_strings)]
  
  p1_rh = table(df$Stroke.Type[df$Player == player_strings[1]],
                df$Ball.hit.to[df$Player == player_strings[1]])
  cp1 = cbind(round(p1_rh /(p1_rh  %>% rowSums()),2),row_total = (p1_rh  %>% rowSums()))
  # repeat for player 2
  p2_rh = table(df$Stroke.Type[df$Player == player_strings[2]],
                df$Ball.hit.to[df$Player == player_strings[2]])
  cp2 = cbind(round(p2_rh/(p2_rh %>% rowSums()),2),row_total = (p2_rh %>% rowSums()))
  cat("Players:", player_strings,"\n")
  list(cp1,cp2)
  
}

# clay
rh_cp(clay_fn)
rs_cp(clay_fn)
sh_cp(clay_fn)

rh_cp(clay_fn[clay_fn$Ball.hit.to.lag2 == "Z2",])
rh_cp(clay_fn[clay_fn$Ball.hit.to.lag2 == "Z3",])
rh_cp(clay_fn[clay_fn$Ball.hit.to.lag2 == "Z4",])
rh_cp(clay_fn[clay_fn$Ball.hit.to.lag2 == "Z5",])

# hc
rh_cp(hc_fdp)
rs_cp(hc_fdp)
sh_cp(hc_fdp)

rh_cp(hc_fdp[hc_fdp$Ball.hit.to.lag2 == "Z2",])


# Vizualize
court_cpmap = function(cps,lz_size = 3, rz_size = 3,
                       cp_lims = rep(NA,2),prob_labels = T,
                       r_zone = 0,sig_zone = 0) {
  # define court settings and zone polygon vertices
  # coordinates of the baseline + side line 
  base_side_line <- data.frame(
    x = c(0, 0, 23.77, 23.77, 0),
    y = c(0, 10.97, 10.97, 0, 0)
  )
  # coordinates of the service line and center line
  serves_center_line <- data.frame(
    x = c(5.585, 5.585, 5.585, 18.385, 18.385, 18.385),
    y = c(1.37, 9.6, 5.485, 5.485, 1.37, 9.6)
  )
  # zone coordinates
  z1_poly = data.frame(
    x = c(11.985,11.985,18.4),
    y = c(5.485,9.6,9.6)
  )
  
  z2_poly_orig = data.frame(
    x = c(11.985,18.4,27.770),
    y = c(5.485,9.6,9.6)
  
  )
  
  z2_poly2 = data.frame(
    x = c(11.985,18.4,23.770,23.770),
    y = c(5.485,9.6,9.6,8.55)
  
  )
  
  z3_poly = data.frame(
    x = c(11.985,23.770,23.770),
    y = c(5.485,8.55,5.485)
  )
  
  z4_poly = data.frame(
    x = c(11.985,23.770,23.770),
    y = c(5.485,5.485,5.485-(8.55-5.485))
  )
  
  z5_poly = data.frame(
    x = c(11.985,18.4,23.770,23.770),
    y = c(5.485,1.37,1.37,2.42)
  )
  
  z6_poly = data.frame(
    x = c(11.985,11.985,18.4),
    y = c(5.485,1.37,1.37)
  )
  # define limits for heat map gradient
  heatmap_grad_limit_low = ifelse(is.na(cp_lims),range(cps)[1],cp_lims)
  heatmap_grad_limit_high = ifelse(is.na(cp_lims),range(cps)[2],cp_lims)
  hm_grad_range = c(heatmap_grad_limit_low[1],heatmap_grad_limit_high[2])

  if (sum(cps) < 0.98) {stop("conditional probabilities must sum to 1")}
  court <- 
    ggplot() +
    # base court
    geom_path(data = base_side_line, aes(x = x, y = y)) +
    geom_path(data = serves_center_line, aes(x = x, y = y),alpha = 0.25) +
    geom_path(aes(x = c(23.77, 0), y = c(1.37, 1.37))) + # include lower singles lines
    geom_path(aes(x = c(23.77, 0), y = c(9.6, 9.6))) + # include upper singles lines
    # right side
    # zone lines
    geom_path(aes(x = c(11.985, 11.985), y = c(0, 10.97)), lty = 2) + # include dotted net line
    geom_path(aes(x = c(11.985, 18.4), y = c(5.485, 9.6)), lty = 2) + # include dotted z1/z2 line
    geom_path(aes(x = c(11.985, 23.77), y = c(5.485, 8.55)), lty = 2) + # include dotted z2/z3 line
    geom_path(aes(x = c(11.985, 23.77), y = c(5.485, 5.485)), lty = 2) + # include dotted z3/z4 line
    geom_path(aes(x = c(11.985, 23.77), y = c(5.485, 5.485-(8.55-5.485))), lty = 2)+ # z4/z5
    geom_path(aes(x = c(11.985,18.4), y = c(5.485, 1.37)), lty = 2)+ # z5/z6
    # fill
    geom_polygon(data = z1_poly, aes(x = x, y = y, fill = cps[1]), alpha=0.6)+
    # geom_polygon(data = z2_poly_orig, aes(x = x, y = y), fill="blue", alpha=0.6)+
    geom_polygon(data = z2_poly2, aes(x = x, y = y,fill = cps[2]), alpha=0.6)+
    geom_polygon(data = z3_poly, aes(x = x, y = y,fill = cps[3]), alpha=0.6)+
    geom_polygon(data = z4_poly, aes(x = x, y = y,fill = cps[4]), alpha=0.6)+
    geom_polygon(data = z5_poly, aes(x = x, y = y,fill = cps[5]), alpha=0.6)+
    geom_polygon(data = z6_poly, aes(x = x, y = y,fill = cps[6]), alpha=0.6)+
    # label zones
    # can edit the size, color, text, position of these labels as needed
    geom_text(aes(x=14, y=8,label = "Z1"),size = rz_size)+
    geom_text(aes(x=16.5, y=7.5,label = "Z2"),size = rz_size)+
    geom_text(aes(x=17.5, y=6.15,label= "Z3"),size = rz_size)+
    geom_text(aes(x=17.5, y=4.82,label= "Z4"),size = rz_size)+
    geom_text(aes(x=16.5, y=3.47,label = "Z5"),size = rz_size)+
    geom_text(aes(x=14, y=3, label = "Z6"),size = rz_size)+
    # add labels with cps (and still with gradient fill)
    geom_label(aes(x=14+1, y=8+1,label=ifelse(if_else(prob_labels,cps[1],NA)==0,NA,if_else(prob_labels,cps[1],NA)),
                   fill = cps[1]),size = rz_size)+
    geom_label(aes(x=16.5+1.75, y=7.5+0.65,label=ifelse(if_else(prob_labels,cps[2],NA)==0,NA,if_else(prob_labels,cps[2],NA)),
                   fill = cps[2]),size = rz_size)+
    geom_label(aes(x=17.5+2, y=6.15+0.1,label=ifelse(if_else(prob_labels,cps[3],NA)==0,NA,if_else(prob_labels,cps[3],NA)),
                   fill = cps[3]),size = rz_size)+
    geom_label(aes(x=17.5+2, y=4.82-0.1,label=ifelse(if_else(prob_labels,cps[4],NA)==0,NA,if_else(prob_labels,cps[4],NA)),
                   fill = cps[4]),size = rz_size)+
    geom_label(aes(x=16.5+1.75, y=3.47-0.65,label=ifelse(if_else(prob_labels,cps[5],NA)==0,NA,if_else(prob_labels,cps[5],NA)),
                   fill = cps[5]),size = rz_size)+
    geom_label(aes(x=14+1, y=3-1,label = ifelse(if_else(prob_labels,cps[6],NA)==0,NA,if_else(prob_labels,cps[6],NA)),
                  fill = cps[6]),size = rz_size)+
    # left side (DEV)
    # 11.985-(23.77-11.985) = 0.2
    geom_path(aes(x = c(11.985, 11.985-(18.4-11.985)), y = c(5.485, 9.6)), lty = 2, alpha = 0.5) + # include dotted z5/z6 line
    geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 8.55)), lty = 2, alpha = 0.5) + # include dotted z4/z5 line
    geom_path(aes(x = c(11.985, 11.985-(23.77-11.985)), y = c(5.485, 5.485)), lty = 2, alpha = 0.5) + # include dotted z3/z4 line
    geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 5.485-(8.55-5.485))), lty = 2, alpha = 0.5)+ # z2/z3
    geom_path(aes(x = c(11.985,5.57), y = c(5.485, 1.37)), lty = 2, alpha = 0.5)+ # z1/z2
    
    # geom_polygon(data = data.frame(x = c(11.985, 5.57,0.2,0.2),
    #                                y = c(5.485,1.37,1.37,2.42)),
    #              aes(x = x, y=y), alpha = 0.2,fill = "royalblue")
    
    geom_text(aes(x=11.985-(14-11.985), y=8,label = "Z6"),size = lz_size)+
    geom_text(aes(x=11.985-(16.5-11.985), y=3.47,label = "Z2"),size = lz_size)+
    geom_text(aes(x=11.985-(17.5-11.985), y=6.15,label= "Z4"),size = lz_size)+
    geom_text(aes(x=11.985-(17.5-11.985), y=4.82,label= "Z3"),size = lz_size)+
    geom_text(aes(x=11.985-(16.5-11.985), y=7.5,label = "Z5"),size = lz_size)+
    geom_text(aes(x=9.97, y=3, label = "Z1"),size = lz_size)+
    # zoom out
    #ylim(c(-1, 11.97)) + xlim(c(-4, 27.77)) +
    # add gradient
    # scale_fill_gradient2(low="white",mid = 'yellow1',high = 'red', midpoint = 0.25,
    #                     limits=c(0,0.5))+
    # try using range instead of 0,0.5
    scale_fill_gradient2(low="white",mid = 'yellow',high = 'red1',
                         midpoint = ifelse(sum(is.na(cp_lims))==2,(max(cps)+min(cps))/2, (max(cp_lims)+min(cp_lims))/2),
                         limits=hm_grad_range)+
    geom_path(aes(x = c(23.77, 0), y = c(9.6, 9.6))) + # include upper singles lines (again)
    theme_void()+ # remove grid and axes
    # labs(fill=expression("P(Hit to" ~ Z[h]~ ")"))+
         #title = "Hit Location Conditional Probability Map")+
    labs(fill = expression(hat(pi)[kspr]))+
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
  if (r_zone == 2) {
    # if (sig_zone == 2) {
    #   
    #   court = court + 
    #     geom_polygon(data = data.frame(x = c(11.985, 5.57,0.05,0.05),
    #                                    y = c(5.485,1.37,1.37,2.42)),
    #                  aes(x = x, y=y), alpha = 0.2,fill = "royalblue2")+
    #     geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 2.42),
    #                   col = "Zone Received"),
    #               lty = 1)+ # z2/z3
    #     geom_path(aes(x = c(11.985,5.57), y = c(5.485, 1.37),
    #                     col = "Zone Received"),
    #                 lty = 1)+ # z1/z2
    #     geom_path(aes(x = c(11.985, 18.4), y = c(5.485, 9.61),
    #                   col = "Significant"),
    #               lty = 1,lwd = 1.2) + # include dotted z1/z2 line
    #     geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 8.55),
    #                   col = "Significant"),
    #               lty = 1,lwd = 2) +
    #     scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
    #                                               "Significant"="green"))
    # }
    # else {
      court = court + 
      geom_polygon(data = data.frame(x = c(11.985, 5.57,0.05,0.05),
                                     y = c(5.485,1.37,1.37,2.42)),
                   aes(x = x, y=y), alpha = 0.2,fill = "royalblue2")+
        geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 2.42),
                      col = "Zone Received"),
                  lty = 1)+ # z2/z3
        geom_path(aes(x = c(11.985,5.57), y = c(5.485, 1.37),
                      col = "Zone Received"),
                  lty = 1)+ # z1/z2
        scale_color_manual(name = NULL,values = c("Zone Received"="royalblue"))
    #}
      
  }
  if (r_zone == 3) {
    court = court + 
    geom_polygon(data = data.frame(x = c(11.985,0.05,0.05),
                                    y = c(5.485,2.42,5.485)),
                 aes(x = x, y=y), alpha = 0.2,fill = "royalblue2")+
      geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 5.485),
                    col = "Zone Received"),
                lty = 1)+ # include dotted z3/z4 line
      geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 5.485-(8.55-5.485)),
                    col = "Zone Received"),
                lty = 1)+ # z2/z3
      scale_color_manual(name = NULL,values = c("Zone Received"="royalblue"))
  }
  if (r_zone == 4) {
    court = court + 
    geom_polygon(data = data.frame(x = c(11.985,0.05,0.05),
                                   y = c(5.485,8.55,5.485)),
                 aes(x = x, y=y), alpha = 0.2,fill = "royalblue2")+
      geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 5.485),
                    col = "Zone Received"),
                lty = 1)+ # include dotted z3/z4 line
      geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 8.55),
                    col = "Zone Received"),
                lty = 1)+ # left z4/z5 line
      scale_color_manual(name = NULL,values = c("Zone Received"="royalblue"))
  }
  if (r_zone == 5) {
    court = court + 
    geom_polygon(data = data.frame(x = c(11.985, 5.57,0.05,0.05),
                                   y = c(5.485,9.6,9.6,8.55)),
                 aes(x = x, y=y), alpha = 0.2,fill = "royalblue2")+
      geom_path(aes(x = c(11.985, 5.57), y = c(5.485, 9.6),
                    col = "Zone Received"),
                lty = 1) + # include dotted z5/z6 line
      geom_path(aes(x = c(11.985, 0.05), y = c(5.485, 8.55),
                    col = "Zone Received"),
                lty = 1)+ # include dotted z4/z5 line
      scale_color_manual(name = NULL,values = c("Zone Received"="royalblue"))
  }
  
  # This didn't work, but this functionality of adding green sig line
  # will work outside of function
  
  # if (sig_zone == 2) {
  #   count = court +
  #     geom_path(aes(x = c(11.985, 18.4), y = c(5.485, 9.61),
  #                         col = "Significant"),
  #                     lty = 1,lwd = 1.4) + # include dotted z1/z2 line
  #     geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 8.55),
  #                   col = "Significant"),
  #               lty = 1,lwd = 1.4) +
  #     scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
  #                                               "Significant"="green"))
  # }
  # 
  # if (sig_zone == 5) {
  #   count = court +
  #     geom_path(aes(x = c(11.985, 23.77), y = c(5.485, 2.42),
  #                         col = "Significant"),
  #                     lty = 1,lwd = 1.4) + # z4/z5 line
  #     geom_path(aes(x = c(11.985,18.4), y = c(5.485, 1.37),
  #                   col = "Significant"),
  #               lty = 1,lwd = 1.4) + # z5/z6 line
  #     scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
  #                                               "Significant"="green"))
  # }
  
  return(court)
}

# test
court_cpmap(rh_cp(clay_fn)[[1]][3,1:6],lz_size = 3,rz_size = 4,r_zone = 3)+
  labs(title = "Nadal CPs when received in Z3")
court_cpmap(rh_cp(adat_clean)[[2]][2,1:6],lz_size = 3,rz_size = 4,
            sig_zone = 2,r_zone = 2)+
  labs(title = "Federer CPs when received in Z2")+
  geom_path(aes(x = c(11.985, 18.4), y = c(5.485, 9.61),
                      col = "Significant"),
            lty = 1,lwd = 1.4) + # include dotted z1/z2 line
  geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 8.55),
                col = "Significant"),
            lty = 1,lwd = 1.4) +
  scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
                                            "Significant"="green"))

court_cpmap(rh_cp(adat_clean)[[1]][5,1:6],lz_size = 3,rz_size = 4,
            sig_zone = 5,r_zone = 5)+
  labs(title = "Nadal CPs when received in Z5") + 
  geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 2.42),
                          col = "Significant"),
                      lty = 1,lwd = 1.4) + # z4/z5 line
  geom_path(aes(x = c(11.985,18.4), y = c(5.485, 1.37),
                col = "Significant"),
            lty = 1,lwd = 1.4) + # z5/z6 line
  scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
                                            "Significant"="green"))

add_z5_sig = function(plot) {
  new_plot = plot + geom_path(aes(x = c(11.985, 23.78), y = c(5.485, 2.42),
                          col = "Significant"),
                      lty = 1,lwd = 1.4) + # z4/z5 line
  geom_path(aes(x = c(11.985,18.4), y = c(5.485, 1.37),
                col = "Significant"),
            lty = 1,lwd = 1.4) + # z5/z6 line
  scale_color_manual(name = NULL,values = c("Zone Received"="royalblue",
                                            "Significant"="green"))
  return(new_plot)
}


# fed z2 vs z5
ggpubr::ggarrange(court_cpmap(rh_cp(clay_fn)[[2]][2,1:6],lz_size = 3,cp_lims = c(0.0,0.4))+
  labs(title = "Federer CPs when received in Z2"),
  court_cpmap(rh_cp(clay_fn)[[2]][5,1:6],lz_size = 3,cp_lims = c(0.0,0.4))+
  labs(title = "Federer CPs when received in Z5"),nrow = 2
  )
# nadal z2 vs z5
ggpubr::ggarrange(court_cpmap(rh_cp(clay_fn)[[1]][2,1:6],lz_size = 3,cp_lims = c(0.0,0.51))+
  labs(title = "Nadal CPs when received in Z2"),
  court_cpmap(rh_cp(clay_fn)[[1]][5,1:6],lz_size = 3,cp_lims = c(0.0,0.51))+
  labs(title = "Nadal CPs when received in Z5"),nrow = 2
  )
# nadal, empirically, appears to be much more predictable for this match
# when hit to his (left side) forehand

court_cpmap(rh_cp(clay_fn)[[2]][2,1:6],lz_size = 3)+
  labs(title = "Federer CPs when received in Z2")

# # Federer Z5 Backhand: top spin vs. slice
# ggpubr::ggarrange(
# court_cpmap(rh_cp(clay_fn[clay_fn$Stroke.Type == "BT",])[[2]][5,1:6],lz_size = 3,cp_lims = c(0.0,0.4))+
#   labs(title = "Federer CPs when received in Z5: BT"),
# court_cpmap(rh_cp(clay_fn[clay_fn$Stroke.Type == "BS",])[[2]][5,1:6],lz_size = 3,cp_lims = c(0.0,0.4))+
#   labs(title = "Federer CPs when received in Z5: BS")
# )

# Federer rec Z5 Forehand: top spin vs. slice
ggpubr::ggarrange(
court_cpmap(rh_cp(clay_fn[clay_fn$Stroke.Type == "FT",])[[1]][5,1:6],lz_size = 3,cp_lims = c(0.0,0.53))+
  labs(title = "Federer CPs when received in Z5: FT"),
court_cpmap(rh_cp(clay_fn[clay_fn$Stroke.Type == "FS",])[[2]][5,1:6],lz_size = 3,cp_lims = c(0.0,0.53))+
  labs(title = "Federer CPs when received in Z5: FS")
)

# # Federer rec Z2 Forehand: top spin vs. slice
# rh_cp(clay_fn[clay_fn$Stroke.Type == "FS",])
# not many forehand slices for either player in this match
# ggpubr::ggarrange(
# court_cpmap(rh_cp(clay_fn[clay_fn$Stroke.Type == "FT",])[[1]][2,1:6],lz_size = 3,cp_lims = c(0.0,0.45))+
#   labs(title = "Federer CPs when received in Z2: FT"),
# court_cpmap(rh_cp(clay_fn[clay_fn$Stroke.Type == "FS",])[[2]][2,1:6],lz_size = 3,cp_lims = c(0.0,0.45))+
#   labs(title = "Federer CPs when received in Z2: FS")
# )

# add labels with given probabilities ####
# test
# prob_labels = T
# court_cpmap(rh_cp(clay_fn)[[2]][2,1:6],lz_size = 3,rz_size = 3)+
#   labs(title = "Federer CPs when received in Z2")+
#   # geom_label(aes(x=14+1, y=8+1,label = rh_cp(clay_fn)[[2]][2,1],
#   #                fill = rh_cp(clay_fn)[[2]][2,1]),size = rz_size)+
#   geom_label(aes(x=16.5+1.5, y=7.5+0.5,label = if_else(prob_labels,rh_cp(clay_fn)[[2]][2,2],NA),
#                  fill = rh_cp(clay_fn)[[2]][2,2]),size = rz_size)+
#   geom_label(aes(x=17.5+1.5, y=6.15+0.1,label= "Z3"),size = rz_size)+
#   geom_label(aes(x=17.5+1.5, y=4.82-0.1,label= "Z4"),size = rz_size)+
#   geom_label(aes(x=16.5+1.5, y=3.47-0.5,label = if_else(prob_labels,rh_cp(clay_fn)[[2]][2,5],NA)),
#              fill = rh_cp(clay_fn)[[2]][2,5],size = rz_size)#+
#   #geom_label(aes(x=14, y=3, label = rh_cp(clay_fn)[[2]][2,6],
#   #               fill = rh_cp(clay_fn)[[2]][2,6]),size = rz_size)+
court_cpmap(rh_cp(clay_fn)[[2]][2,1:6],lz_size = 3,rz_size = 3)+
  labs(title = "Federer CPs when received in Z2")

# explore signal available in opposing stroke type
adat_clean_simple$Opp.Stroke.Type %>% table()
rh_cp(adat_clean_simple[adat_clean_simple$Opp.Stroke.Type == "FT",])
rh_cp(adat_clean_simple[adat_clean_simple$Opp.Stroke.Type == "FS",])
rh_cp(adat_clean_simple[adat_clean_simple$Opp.Stroke.Type == "BT",])
rh_cp(adat_clean_simple[adat_clean_simple$Opp.Stroke.Type == "BS",])
ob_cp = rh_cp(adat_clean_simple[adat_clean_simple$Opp.Stroke.Hand == "B",])
of_cp = rh_cp(adat_clean_simple[adat_clean_simple$Opp.Stroke.Hand == "F",])
os_cp = rh_cp(adat_clean_simple[adat_clean_simple$Opp.Hit.Type == "S",])
ot_cp = rh_cp(adat_clean_simple[adat_clean_simple$Opp.Hit.Type == "T",])

# compare receiving forehand and receiving backhand on backhand side for both players
ggarrange(
  # fed, received z5
  ggarrange(court_cpmap(c(0,ob_cp[[1]][4,1:4],0))+labs(title = "Opp used Forehand"),
            court_cpmap(c(0,of_cp[[1]][4,1:4],0))+labs(title = "Opp used Backhand"),
            nrow = 2) %>% annotate_figure(fig.lab = "Federer CPs",
                                          fig.lab.pos = "top.left",
                                          fig.lab.face = "italic"),
  
  # nadal, received z2
  ggarrange(court_cpmap(c(0,ob_cp[[2]][1,1:4],0))+labs(title = "Opp used Forehand"),
            court_cpmap(c(0,of_cp[[2]][1,1:4],0))+labs(title = "Opp used Backhand"),
            nrow = 2) %>% annotate_figure(fig.lab = "Nadal CPs",
                                          fig.lab.pos = "top.right",
                                          fig.lab.face = "italic"),
ncol = 2)

# compare receiving topspin and receiving slice on backhand side for both players
ggarrange(
  ggarrange(court_cpmap(c(0,ot_cp[[1]][4,1:4],0))+labs(title = "Opp used Topspin"),
            court_cpmap(c(0,os_cp[[1]][4,1:4],0))+labs(title = "Opp used Slice"),
            nrow = 2) %>% annotate_figure(fig.lab = "Federer CPs",
                                          fig.lab.pos = "top.left",
                                          fig.lab.face = "italic"),
  
  # nadal, received z2
  ggarrange(court_cpmap(c(0,ot_cp[[2]][1,1:4],0))+labs(title = "Opp used Topspin"),
            court_cpmap(c(0,os_cp[[2]][1,1:4],0))+labs(title = "Opp used Slice"),
            nrow = 2) %>% annotate_figure(fig.lab = "Nadal CPs",
                                          fig.lab.pos = "top.right",
                                          fig.lab.face = "italic"),
ncol = 2) %>% 
  annotate_figure(top = "Received Backhand Side")

# compare receiving topspin and receiving slice on forehand side for both players
ggarrange(
  ggarrange(court_cpmap(c(0,ot_cp[[1]][1,1:4],0))+labs(title = "Opp used Topspin"),
            court_cpmap(c(0,os_cp[[1]][4,1:4],0))+labs(title = "Opp used Slice"),
            nrow = 2) %>% annotate_figure(fig.lab = "Federer CPs",
                                          fig.lab.pos = "top.left",
                                          fig.lab.face = "italic"),
  
  # nadal, received z2
  ggarrange(court_cpmap(c(0,ot_cp[[2]][4,1:4],0))+labs(title = "Opp used Topspin"),
            court_cpmap(c(0,os_cp[[2]][4,1:4],0))+labs(title = "Opp used Slice"),
            nrow = 2) %>% annotate_figure(fig.lab = "Nadal CPs",
                                          fig.lab.pos = "top.right",
                                          fig.lab.face = "italic"),
ncol = 2) %>% 
  annotate_figure(top = "Received Forehand Side")


# and hit type
adat_clean_simple$Hit.Type %>% table()
rh_cp(adat_clean_simple[adat_clean_simple$Hit.Type == "S",])
rh_cp(adat_clean_simple[adat_clean_simple$Hit.Type == "T",])
sh_cp
# and Stroke.Hand
adat_clean_simple$Stroke.Hand %>% table()
rh_cp(adat_clean_simple[adat_clean_simple$Stroke.Hand == "F",])
rh_cp(adat_clean_simple[adat_clean_simple$Stroke.Hand == "B",])





