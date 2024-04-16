# clean tennis data
# Build cleaning into a function
# Note: Opp Stroke Type could still use some work validating correct creation

tennis_data_clean = function(dat, player_labels = c("F","N")) {
  require(tidyverse)
  
  # new cleaning process
  # dat <- read.table("./Wimbledon/Wimbledon 2019.csv", sep=",", header=TRUE)
  # all(colnames(dat) ==c("Player"  ,        "Ball.lands" ,     "Ball.hit.from"  , "Stroke.Type"  ,   "Ball.hit.to"  ,   "In.out",          "Forced.Unforced",
  #                   "Double.Fault" ,   "End.of.Point" ,   "End.of.Game"    , "End.of.Set",      "End.of.Match"))
  req_col_names = c("Player","Ball.lands","Ball.hit.from","Stroke.Type","Ball.hit.to",
                    "In.out","Forced.Unforced","Double.Fault","End.of.Point",
                    "End.of.Game", "End.of.Set","End.of.Match")
  
  if (all(colnames(dat)!= req_col_names)) {
    colnames(dat) = req_col_names
  }
  
  dat <- data.frame(sapply(dat, trimws)) # remove leading/trailing whitespace
  
  ## some of these cleans below are not needed, as data was collected cleaner than first datasets
  #dat$Ball.hit.to[dat$Ball.hit.to == "Z4`"] <- "Z4" # remove unecessary "`" marks
  dat$In.out[dat$In.out %in% c("in","iN","In","IN")] <- "In" # clean up In
  dat$In.out[dat$In.out %in% c("long","Long")] <- "Long" # Clean up Long
  dat$In.out[dat$In.out %in% c("No Hit","No HIt", "no hit")] <- "No Hit" # Clean "No Hit"
  dat$In.out[dat$In.out %in% c("net", "Net")] <- "Net" # Clean "Net"
  dat$In.out[dat$In.out %in% c("wide", "Wide")] <- "Wide" # Clean "Wide"
  dat$Forced.Unforced[dat$Forced.Unforced %in% c("unforced","Unforced")] <- "Unforced" # Clean "Unforced"
  dat$Forced.Unforced[dat$Forced.Unforced %in% c("forced","forced")] <- "Forced" # Clean "Unforced"
  
  # add preliminary id column
  dat = dat %>% mutate(id0 = row_number())
  # I need to create the rallys with these data and combine them in some way
  
  
  # First find out length of rallys using our dataset
  # Rally length begins counting with serve that put
  # ball in play and includes last swing, regardless
  # of whether the swing was successful or not
  
  # # redefine rally_num if needed (e.g., rally defined incorrectly)
  dat$rally_num <- cumsum((as.numeric(as.factor(dat$End.of.Point)))-1)
  # dat$rally_num <- c(0, dat$rally_num[-length(dat$rally_num)])  # set first row of rallynum to 0, then remove last row so length is correct
  
  dat$rally_num2 <- cumsum((as.numeric(as.factor(dat$Stroke.Type == "S")))-1) 
  # dat$rally_num2 <- c(0, dat$rally_num[-length(dat$rally_num)])  # set first row of rallynum to 0, then remove last row so length is correct
  
  # # redefine rally_num if needed (e.g., rally defined incorrectly)
  # dat$rally_num <- cumsum((as.numeric(as.factor(dat$End.of.Point)))-1)
  # dat$rally_num <- c(0, dat$rally_num[-length(dat$rally_num)])  # set first row of rallynum to 0, then remove last row so length is correct
  
  
  # Remove double faults as they aren't important to what I am 
  # trying to do.
  df_rally <- dat$rally_num2[dat$Double.Fault == "Yes"]
  dat <- dat[!dat$rally_num2 %in% df_rally,] # remove whole "rally" that ended in Double Fault
  
  # Remove the "No Hit" as they don't provide any more
  # information about the sequence 
  ### however, they do contain info on end of point, game, and match
  # will need to end up doing same thing removing shots hit into the net or Wide.
  # (as not actually hit to a zone at all, no matter where they bounce over to)
  dat <- dat[dat$In.out != "No Hit",]
  dat = dat[!dat$Stroke.Type == "-",] # stroke Type = "-" = no return hit
  
  # If there are two serves, I need to combine these into one, so
  # that serve is only counted once in a rally.  Or should I just 
  # keep the serve that begins the rally?  Yes, I will do that
  
  # tmp <- (dat$Stroke.Type[-nrow(dat)] == dat$Stroke.Type[-1]) # original code
  #table(dat$Stroke.Type)
  tmp <- ((dat$Stroke.Type[-nrow(dat)] == dat$Stroke.Type[-1]) & (dat$Stroke.Type[-1] == 'S')) 
  ## above adds filter to only remove duplicate serves, not back to back hits of same stoke type
  dat <- dat[!c(tmp, FALSE),]
  
  # Rally's based solely on where the ball was struck
  bhf_rally <- tapply(dat$"Ball.hit.from", dat$rally_num2, function(x) paste(x, collapse="_"))
  # ... based on where the ball landed
  bht_rally <- tapply(dat$"Ball.hit.to", dat$rally_num2, function(x) paste(x, collapse="_"))
  
  # code to reseparate this string (e.g., for time series):
  # match_ts = str_split(bht_rally,pattern = "_") %>% unlist()
  
  # bind rally strings to dataset
  #summary(dat$rally_num)
  bhf_strings = stack(bhf_rally)
  names(bhf_strings) = c("hit.from.string", "rally_num2")
  bht_strings = stack(bht_rally)
  names(bht_strings) = c("hit.to.string", "rally_num2")
  
  dat = dat %>% mutate(rally_num = as.factor(rally_num),rally_num2=as.factor(rally_num2))# convert rally_num to factor for correct joining
  dat = dat %>% left_join(bhf_strings, by = "rally_num2") %>% left_join(bht_strings, by = "rally_num2") # join rally_seq strings
  dat = dat %>% mutate_if(is.character,as.factor) # convert all variables to factors
  
  # add id column
  dat = dat %>% mutate(id = row_number(),
                       # *change rally num to new definiton*
                       rally_num = as.numeric(rally_num2)) 
  
  # generate one.before column for Stroke.Type (i.e., Oppenent Stroke Type)
  within_rally_s_lag = tapply(dat$Stroke.Type, dat$rally_num, FUN = function(x) lag(x,1))
  # convert list with elements of differing length to data frame using rapply
  new.l.s = rapply(within_rally_s_lag, function(x) paste(x, collapse = "|") %>% as.list(), how = "replace")
  
  dt.s = data.table::rbindlist(new.l.s) # form data table
  dt.s$rally_num = names(new.l.s) # create rally_num column
  opst_tbl = dt.s %>% tidyr::separate_rows(V1, sep = "\\|") %>% # separate by row
    rename(Opp.Stroke.Type = "V1") %>%
    mutate(rally_num = as.numeric(rally_num)) %>% 
    mutate(id = row_number())   # add id column for joining
  
  ## generate two.before, three.before, and four.before column for ball.hit.to
  
  # lag 2 within rally
  within_rally_lag = tapply(dat$Ball.hit.to, dat$rally_num, FUN = function(x) lag(x,2))
  # convert list with elements of differing length to data frame using rapply
  new.l = rapply(within_rally_lag, function(x) paste(x, collapse = "|") %>% as.list(), how = "replace")
  
  dt = data.table::rbindlist(new.l) # form data table
  dt$rally_num = names(new.l) # create rally_num column
  bht_lag2_tbl = dt %>% tidyr::separate_rows(V1, sep = "\\|") %>% # separate by row
    rename(Ball.hit.to.lag2 = "V1") %>%
    mutate(rally_num = as.numeric(rally_num)) %>% 
    mutate(id = row_number())   # add id column for joining
  
  # lag 3 within rally
  # this is where ball was hit 3 hits previous
  # this is also the previous Ball.lands location; Ball.lands could also be know as Ball.hit.to.lag1
  within_rally_lag3 = tapply(dat$Ball.hit.to, dat$rally_num, FUN = function(x) lag(x,3))
  # convert list with elements of differing length to data frame
  new.l3 = rapply(within_rally_lag3, function(x) paste(x, collapse = "|") %>% as.list(), how = "replace")
  
  dt3 = data.table::rbindlist(new.l3) # form data table
  dt3$rally_num = names(new.l3) # create rally_num column
  bht_lag3_tbl = dt3 %>% tidyr::separate_rows(V1, sep = "\\|") %>% # separate by row
    rename(Ball.hit.to.lag3 = "V1") %>%
    mutate(rally_num = as.numeric(rally_num)) %>% 
    mutate(id = row_number()) # add id column for joining
  
  # lag 4 within rally - therefore where a given player hit the ball 2 sequences ago
  within_rally_lag4 = tapply(dat$Ball.hit.to, dat$rally_num, FUN = function(x) lag(x,4))
  # convert list with elements of differing length to data frame
  new.l4 = rapply(within_rally_lag4, function(x) paste(x, collapse = "|") %>% as.list(), how = "replace")
  
  dt4 = data.table::rbindlist(new.l4) # form data table
  dt4$rally_num = names(new.l4) # create rally_num column
  bht_lag4_tbl = dt4 %>% tidyr::separate_rows(V1, sep = "\\|") %>% # separate by row
    rename(Ball.hit.to.lag4 = "V1") %>%
    mutate(rally_num = as.numeric(rally_num)) %>% 
    mutate(id = row_number()) # add id column for joining
  
  # join this to dat as dat2
  dat2 = dat %>% inner_join(bht_lag2_tbl, by = c("rally_num", "id")) %>% 
    inner_join(bht_lag3_tbl, by = c("rally_num", "id")) %>% 
    inner_join(bht_lag4_tbl, by = c("rally_num", "id")) %>%
    # join Opp.Stroke.Type
    inner_join(opst_tbl, by = c("rally_num","id")) %>% 
    as_tibble() %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate(id = row_number()) %>% 
    dplyr::select(Player,Stroke.Type,Opp.Stroke.Type,Ball.hit.to.lag4,Ball.hit.to.lag3,Ball.hit.to.lag2,Ball.lands,Ball.hit.from,Ball.hit.to, rally_num,id0, id, everything())
  
  # make variable that pastes name of player who had first obs in rally ("Player serving")
  serve = tapply(dat2$Player,dat2$rally_num, function(x) as.factor(x[1])) %>% as.data.frame()
  serve$rally_num = rownames(serve) %>% as.numeric()
  colnames(serve) = c("Serving","rally_num")
  # serve[serve$Serving==1,"Serving"] = "F"
  # serve[serve$Serving ==2,"Serving"] = 'N'
  # generalize to any two players
  serve[serve$Serving==1,"Serving"] = player_labels[1]
  serve[serve$Serving ==2,"Serving"] = player_labels[2]
  
  dat2$Stroke.Type %>% table()
  dat2$Ball.hit.to %>% table()
  
  # dataset with `Server` variable = indicator of if player is serving or not
  dat2b = dat2 %>% inner_join(serve, by = "rally_num") %>% 
    mutate(Server = ifelse(Serving == Player, 1, 0) %>% as.factor(),
           Ball.hit.to = as.character(Ball.hit.to))
  dat2b$Ball.hit.to[dat2b$In.out == "Net"] = "Net"# correctly specify Ball.hit.to obs in the Net
  dat2b$Ball.hit.to = as.factor(dat2b$Ball.hit.to) # change Ball.hit.to back to factor
  # dim(dat2b)
  
  # new data set
  # for response, change
  # Z15, Z18 to Z1
  # Z17, Z20 to Z6
  # remove response obs to Z16 and zone 19
  # this arrangement does not apply to all variables, so factor collapse
  # will throw warnings, but collapsing in this way will make this reproducible
  # (for other datasets of similar form)
  zones_simple = c("Z1","Z2","Z3","Z4","Z5","Z6")
  dat_new_simple = dat2b %>% 
    # relevel factors
    mutate(Ball.hit.to.lag4 = fct_collapse(Ball.hit.to.lag4,Z1 = c("Z1","Z15","Z18"), Z6 = c("Z6", "Z17", "Z20"),
                                           Z2 = c("Z2","Z7","Z11"), Z3 = c("Z3","Z8","Z12"),
                                           Z4 = c("Z4","Z9","Z13"), Z5 = c("Z5", "Z10", "Z14"),AT_NET = c("Z16","Z19")
                                           ),
           Ball.hit.to.lag3 = fct_collapse(Ball.hit.to.lag3, Z1 = c("Z1","Z15","Z18"), Z6 = c("Z6", "Z17", "Z20"),
                                           Z2 = c("Z2","Z7","Z11"), Z3 = c("Z3","Z8","Z12"),
                                           Z4 = c("Z4","Z9","Z13"), Z5 = c("Z5", "Z10", "Z14"),AT_NET = c("Z16","Z19")
                                           ),
           Ball.hit.to.lag2 = fct_collapse(Ball.hit.to.lag2, Z1 = c("Z1","Z15","Z18"), Z6 = c("Z6", "Z17", "Z20"),
                                           Z2 = c("Z2","Z7","Z11"), Z3 = c("Z3","Z8","Z12"),
                                           Z4 = c("Z4","Z9","Z13"), Z5 = c("Z5", "Z10", "Z14"),AT_NET = c("Z16","Z19")
                                           ),
           Ball.lands = fct_collapse(Ball.lands, Z1 = c("Z1","Z15","Z18"), Z6 = c("Z6", "Z17", "Z20"),
                                           Z2 = c("Z2","Z7","Z11"), Z3 = c("Z3","Z8","Z12"),
                                           Z4 = c("Z4","Z9","Z13"), Z5 = c("Z5", "Z10", "Z14"),AT_NET = c("Z16","Z19")
                                           ),
           Ball.hit.from = fct_collapse(Ball.hit.from, Z1 = c("Z1","Z15","Z18"), Z6 = c("Z6", "Z17", "Z20"),
                                           Z2 = c("Z2","Z7","Z11"), Z3 = c("Z3","Z8","Z12"),
                                           Z4 = c("Z4","Z9","Z13"), Z5 = c("Z5", "Z10", "Z14"),AT_NET = c("Z16","Z19")
                                           ),
           Ball.hit.to = fct_collapse(Ball.hit.to, Z1 = c("Z1","Z15","Z18"), Z6 = c("Z6", "Z17", "Z20"),
                                           Z2 = c("Z2","Z7","Z11"), Z3 = c("Z3","Z8","Z12"),
                                           Z4 = c("Z4","Z9","Z13"), Z5 = c("Z5", "Z10", "Z14")#,AT_NET = c("Z16","Z19")
                                           ),
           # (temp) Split Stroke.Type into Forehand and Backhand
           Stroke.Type.Hand = fct_collapse(Stroke.Type, Forehand = c("FL","FO","FS","FT","FV"),
                                      Backhand = c("BL","BO","BS","BT","BV"))
           # simplifying Stroke Type caused decrease in accuracy and auc, so will keep this commented out
           ) %>%
    #filter(Ball.hit.to.lag2 != "NA") %>%  # remove first two hits in the rally (dim = 1056 22)
    filter(In.out != "Net") %>% # remove obs hit into the net (dim =  984 22)
    filter(Ball.hit.to != "AT_NET") %>% # remove any lob shot or short ball obs (dim  = 977 22)
    #filter(Stroke.Type != 'S') %>% # remove serve obs
    dplyr::select(Player,Server,Stroke.Type,Opp.Stroke.Type,Ball.hit.to.lag4,Ball.hit.to.lag3,Ball.hit.to.lag2,Ball.lands,Ball.hit.from,Ball.hit.to,In.out,id,rally_num,hit.to.string)
  
  
  dat_new_simple$Stroke.Type %>% table()
  # make sure factors are (re)leveled correctly
  dat_new_simple$Ball.hit.to = factor(dat_new_simple$Ball.hit.to, levels = zones_simple)
  # only 6 zones for response variable, but (for now) keep AT_NET information in other explanatory vars # 4/4 temporary decision to take out AT_NET for Ball.hit.to.lag2 (and could do for all?)
  dat_new_simple$Ball.hit.to.lag4 = factor(dat_new_simple$Ball.hit.to.lag4, levels = c(zones_simple,"AT_NET"))
  dat_new_simple$Ball.hit.to.lag3 = factor(dat_new_simple$Ball.hit.to.lag3, levels = c(zones_simple,"AT_NET"))
  dat_new_simple$Ball.hit.to.lag2 = factor(dat_new_simple$Ball.hit.to.lag2, levels = c(zones_simple,"AT_NET"))
  dat_new_simple$Ball.lands = factor(dat_new_simple$Ball.lands, levels = c(zones_simple,"AT_NET"))
  dat_new_simple$Ball.hit.from = factor(dat_new_simple$Ball.hit.from, levels = c(zones_simple,"AT_NET"))
  dat_new_simple$Stroke.Type = factor(dat_new_simple$Stroke.Type,levels = c("BL","BO","BS","BT","BV","FL","FO","FS","FT","FV"))
  dat_new_simple$Opp.Stroke.Type = factor(dat_new_simple$Opp.Stroke.Type,levels = c("BL","BO","BS","BT","BV","FL","FO","FS","FT","FV"))
  dat_new_simple$rally_num = as.factor(dat_new_simple$rally_num)
  # add columns retrieving Stroke.Hand (first letter of Stroke.Type string)
  # and Hit.Type (second letter of Stroke.Type string)
  ## can do this quite easily using the substr function, 
  # which I was referred to after a search via microsoft copilot (CHAT GPT 3.5)
  dat_new_simple$Stroke.Hand = as.factor(substr(dat_new_simple$Stroke.Type,1,1))
  dat_new_simple$Hit.Type = as.factor(substr(dat_new_simple$Stroke.Type,2,2))
  dat_new_simple$Opp.Stroke.Hand = as.factor(substr(dat_new_simple$Opp.Stroke.Type,1,1))
  dat_new_simple$Opp.Hit.Type = as.factor(substr(dat_new_simple$Opp.Stroke.Type,2,2))
  
  dat_clean = dat_new_simple %>%
    filter(!is.na(Ball.hit.from)) %>% 
    filter(!is.na(Ball.lands)) %>% filter(!is.na(Ball.hit.to)) %>% 
    # filter out obs with ball received "AT_NET"
    # (denoting either a lob shot or short shot while player is at/ near net)
    filter(Ball.lands != "AT_NET") %>% 
    mutate(rally_length = str_split(hit.to.string, pattern = "_") %>% lapply(FUN = length) %>% unlist())
    # to possibly add here, or in later function:
    # remove hits that are not topspin or slice
    # i.e., Hit.Type == "T" or Hit.Type == "S"
    # %>% filter(Hit.Type %in% c("T", "S"))
  
  return(dat_clean)
}

# # test cleaning function on Wimbledon data
# wdat = read.csv("./Wimbledon/Wimbledon 2019.csv")
# wdat_clean = tennis_data_clean(wdat)
# fdat = read.csv("./2005 French Open.csv")
# fdat_clean = tennis_data_clean(fdat)
# adat = read.csv("./Australian Open/Federer Nadal Austrailian Open 2009 - Sheet1.csv")
# adat_clean = tennis_data_clean(adat)
# 
# # 568 rows removed in cleaning of 2019 Wimbledon data (1284 -> 716)
# nrow(wdat_clean)
# nrow(fdat_clean)
# nrow(adat_clean)
# 
# # apply simplification to 4 zones (2-5), and hit types
# # then look at distribution of counts across main variables 
# # (hit.to, lands, hit types)
# ######
# test = adat_clean %>% 
#   # simplify to 4 zones of interest (2:5)
#   # filter(Ball.hit.to %in% str_c("Z",2:5)) %>% 
#   # filter(Ball.lands %in% str_c("Z",2:5))
#   # or, another way
#   mutate(Ball.hit.to = factor(Ball.hit.to, levels = str_c("Z",2:5)),
#          Ball.lands = factor(Ball.lands, levels = str_c("Z",2:5)),
#          # simplify to only most prevalent hit types (Topspin and Slice)
#          Hit.Type = factor(Hit.Type, levels = c("S", "T"))
#   )
# 
# # get tables of f counts across main variables 
# # (hit.to, lands, hit types)
# test$Ball.hit.to %>% table()
# test$Ball.lands %>% table()
# test$Hit.Type %>% table()

# clean, simplify to 4 zones, and t and s hit types
clean_simplify_zht = function(dat,player_labels = c("F","N")) {
  dat_clean = tennis_data_clean(dat,player_labels)
  
  dat_simple_zht = dat_clean %>% 
    mutate(Ball.hit.to = factor(Ball.hit.to, levels = str_c("Z",2:5)),
         Ball.lands = factor(Ball.lands, levels = str_c("Z",2:5)),
         # simplify to only most prevalent hit types (Topspin and Slice)
         Hit.Type = factor(Hit.Type, levels = c("S", "T"))
    ) %>% 
    # remove NAs (from non 2:5 zones and non S & T Hit.Types)
    filter(!is.na(Ball.hit.to)) %>% 
    filter(!is.na(Ball.lands)) %>% 
    filter(!is.na(Hit.Type))
  
  return(dat_simple_zht)
}

# raw data
wdat = read.csv("./Wimbledon/Wimbledon 2019.csv")
fdat = read.csv("./FrenchOpen/2005 French Open.csv")
adat = read.csv("./AustralianOpen/Federer Nadal Austrailian Open 2009 - Sheet1.csv")

# clean data (yet to simplify zones)
adat_clean = tennis_data_clean(adat)
fdat_clean = tennis_data_clean(fdat)
wdat_clean = tennis_data_clean(wdat,player_labels = c("F","N"))

# cleaned, simplified data
adat_clean_simple = clean_simplify_zht(adat)
fdat_clean_simple = clean_simplify_zht(fdat)
wdat_clean_simple = clean_simplify_zht(wdat)

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

# add discussion datasets to cleaning
# Federer vs. Del potro
us_fdp = read.csv("./USOpen/2009 US Open.csv")
us_fdp_clean = clean_simplify_zht(us_fdp,player_labels = c("DP", "F"))

hr_counts_us_fdp = table(us_fdp_clean[,c("Ball.lands","Ball.hit.to")])
fed_counts_dp = table(us_fdp_clean[us_fdp_clean$Player=="F",c("Ball.lands","Ball.hit.to")])
dp_counts_fed = table(us_fdp_clean[us_fdp_clean$Player=="DP",c("Ball.lands","Ball.hit.to")])

# Nadal vs. Djokovic
wmb_ndj = read.csv("./Wimbledon/Nadal Vs Djokovic Wimbledon 2018 semi final - Sheet1.csv")
wmb_ndj_clean = clean_simplify_zht(wmb_ndj, player_labels = c("D","N"))

hr_counts_wim18_ndj = table(wmb_ndj_clean[,c("Ball.lands","Ball.hit.to")])
nad_counts_dj = table(wmb_ndj_clean[wmb_ndj_clean$Player=="N",c("Ball.lands","Ball.hit.to")])
dj_counts_nad = table(wmb_ndj_clean[wmb_ndj_clean$Player=="D",c("Ball.lands","Ball.hit.to")])
