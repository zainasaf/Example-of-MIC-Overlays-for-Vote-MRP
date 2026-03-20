
setwd('/Users/anouschka/Library/CloudStorage/OneDrive-SharedLibraries-MoreInCommon/More In Common Team Site - MRP/')

folder_for_preds <- paste0('Results/VI/Sept MRP/')
df <- read.csv(paste0(folder_for_preds,'preds_agg.csv')) %>% rename('Constituency'='const_name')
df%>%group_by(winner) %>% count()

########## Overlays ########

#### SMALLER PARTY LOCAL BOOST ####
overunder <- read.csv('/Users/anouschka/Library/CloudStorage/OneDrive-SharedLibraries-MoreInCommon/More In Common Team Site - MRP/Scripts/2025 MRP/UK predicted_vs_actual.csv')
overunder[is.na(overunder)]<-0
elex_results <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv") %>%
  mutate(const_name = gsub("Ynys M\xf4n", "Ynys Môn", const_name, useBytes = TRUE),
         const_name = gsub("Ynys Mon", "Ynys Môn", const_name, useBytes = TRUE)) %>%
  select(Constituency=const_name,turnout,ge_con,ge_lab,ge_libdem,ge_ref,ge_green,ge_snp,ge_pc,ge_oth)
map <- df %>% left_join(elex_results,by='Constituency')

map_withoverunder <- left_join(map,overunder,by='Constituency')
map_withoverunder <- map_withoverunder %>% rename('Liberal Democrat'='Liberal.Democrat',
                                                  'Plaid Cymru'='Plaid.Cymru',
                                                  'Reform UK'='Reform.UK',
                                                  'The Green Party'='The.Green.Party',
                                                  'Scottish National Party (SNP)'='Scottish.National.Party..SNP.')

map_withoverunder$LD_new <- ifelse((!is.na(map_withoverunder$lib_ovun)),pmax(map_withoverunder$'Liberal Democrat'+0.33*map_withoverunder$lib_ovun,0.5*map_withoverunder$ge_libdem,na.rm=FALSE),map_withoverunder$'Liberal Democrat') 
map_withoverunder$Grn_new <- ifelse((!is.na(map_withoverunder$grn_ovun)),pmax(map_withoverunder$'The Green Party'+map_withoverunder$grn_ovun,0.5*map_withoverunder$ge_green,na.rm=FALSE),map_withoverunder$'The Green Party') 

map_withoverunder <- map_withoverunder %>% select(Constituency,Conservative,Labour,
                                                  'Liberal Democrat'=LD_new,
                                                  'Other',
                                                  'Plaid Cymru',
                                                  'Reform UK',
                                                  'The Green Party'=Grn_new,
                                                  'Scottish National Party (SNP)')

map_withoverunder$sum <-rowSums(map_withoverunder[sapply(map_withoverunder, is.numeric)])
map_withoverunder[names(map_withoverunder) != "sum" & sapply(map_withoverunder, is.numeric)] <- 
  lapply(map_withoverunder[names(map_withoverunder) != "sum" & sapply(map_withoverunder, is.numeric)], function(x) x / map_withoverunder$sum)
map_withoverunder <- map_withoverunder %>% select(-sum)
# write.csv(map_withoverunder,'Results/VI/Sept MRP/v2_map_withoverunder.csv')

map_withoverunder$Winner <- max.col(map_withoverunder %>% select(c(Conservative, Labour, `Liberal Democrat`,  `Reform UK`, `The Green Party`,`Scottish National Party (SNP)`, `Plaid Cymru`, Other)))
map_withoverunder <- map_withoverunder %>% mutate(Winner = case_when(Winner  == 1 ~ "Conservative", Winner  == 2 ~ "Labour", Winner  == 3 ~ "Liberal Democrat", Winner  == 5 ~ "The Green Party", Winner  == 4 ~ "Reform UK", Winner  == 6 ~ "Scottish National Party (SNP)", Winner  == 7 ~ "Plaid Cymru", Winner  == 8 ~ "Other"))
map_withoverunder %>% group_by(Winner) %>% count()

#### PLAID BOOST ####
map <- read.csv('Results/VI/Sept MRP/v2_map_withoverunder.csv')
map <- map %>% left_join(elex_results,by='Constituency')
map$plaid_swing <- map$'Plaid.Cymru' - map$ge_pc
View(map)
map$PC_new <- ifelse(map$plaid_swing<(-0.1),map$ge_pc+0.5*map$plaid_swing, map$'Plaid.Cymru')
map_withPCboost <- map %>% select(Constituency,Conservative,Labour,
                                  'Liberal Democrat'='Liberal.Democrat',
                                                  'Other',
                                                  'Plaid Cymru'=PC_new,
                                  'Reform UK'='Reform.UK',
                                  'The Green Party'='The.Green.Party',
                                  'Scottish National Party (SNP)'='Scottish.National.Party..SNP.')

map_withPCboost$sum <-rowSums(map_withPCboost[sapply(map_withPCboost, is.numeric)])
map_withPCboost[names(map_withPCboost) != "sum" & sapply(map_withPCboost, is.numeric)] <- 
  lapply(map_withPCboost[names(map_withPCboost) != "sum" & sapply(map_withPCboost, is.numeric)], function(x) x / map_withPCboost$sum)
map_withPCboost <- map_withPCboost %>% select(-sum)
# write.csv(map_withPCboost,'Results/VI/Sept MRP/v2_map_withoverunder_PC.csv')

map_withPCboost$Winner <- max.col(map_withPCboost %>% select(c(Conservative, Labour, `Liberal Democrat`,  `Reform UK`, `The Green Party`,`Scottish National Party (SNP)`, `Plaid Cymru`, Other)))
map_withPCboost <- map_withPCboost %>% mutate(Winner = case_when(Winner  == 1 ~ "Conservative", Winner  == 2 ~ "Labour", Winner  == 3 ~ "Liberal Democrat", Winner  == 5 ~ "The Green Party", Winner  == 4 ~ "Reform UK", Winner  == 6 ~ "Scottish National Party (SNP)", Winner  == 7 ~ "Plaid Cymru", Winner  == 8 ~ "Other"))
map_withPCboost %>% group_by(Winner) %>% count()

#### INDEPENDENT BOOST ####
map <- read.csv('Results/VI/Sept MRP/v2_map_withoverunder_PC.csv')
map <- map %>% left_join(elex_results,by='Constituency')
map_withindboost <- map

map_withindboost$Other <- ifelse(map_withindboost$ge_oth==0,0,map_withindboost$Other)
map_withindboost$Other <- ifelse((map_withindboost$ge_oth>0.25)&(map_withindboost$Constituency!='Chorley'),map_withindboost$Other+0.25,map_withindboost$Other)

map_withindboost <- map_withindboost %>% select(Constituency,Conservative,Labour,
                                                Liberal.Democrat,Reform.UK,                    
                                                The.Green.Party, Scottish.National.Party..SNP., Plaid.Cymru,                  
                                                Other, Winner) %>% rename(orig_winner=Winner)
map_withindboost$sum <-rowSums(map_withindboost[sapply(map_withindboost, is.numeric)])
map_withindboost[names(map_withindboost) != "sum" & sapply(map_withindboost, is.numeric)] <- 
  lapply(map_withindboost[names(map_withindboost) != "sum" & sapply(map_withindboost, is.numeric)], function(x) x / map_withindboost$sum)
map_withindboost <- map_withindboost %>% select(-sum)
map_withindboost <- map_withindboost %>% select(Constituency,Conservative,Labour,
                                                'Liberal Democrat'=Liberal.Democrat,
                                                'Reform UK'=Reform.UK,                    
                                                'The Green Party'=The.Green.Party,
                                                'Scottish National Party (SNP)'=Scottish.National.Party..SNP.,
                                                'Plaid Cymru'=Plaid.Cymru,                  
                                                Other)
# write.csv(map_withindboost,'Results/VI/Sept MRP/v2_map_withoverunder_PCindboost.csv')

map_withindboost$Winner <- max.col(map_withindboost %>% select(c(Conservative, Labour, `Liberal Democrat`,  `Reform UK`, `The Green Party`,`Scottish National Party (SNP)`, `Plaid Cymru`, Other)))
map_withindboost <- map_withindboost %>% mutate(Winner = case_when(Winner  == 1 ~ "Conservative", Winner  == 2 ~ "Labour", Winner  == 3 ~ "Liberal Democrat", Winner  == 5 ~ "The Green Party", Winner  == 4 ~ "Reform UK", Winner  == 6 ~ "Scottish National Party (SNP)", Winner  == 7 ~ "Plaid Cymru", Winner  == 8 ~ "Other"))
map_withindboost %>% group_by(Winner) %>% count()

#### SCOTTISH LIB DEMS

map <- read.csv('Results/VI/Sept MRP/v2_map_withoverunder_PCindboost.csv')
map <- map %>% left_join(elex_results,by='Constituency')
map$ld_swing <- map$Liberal.Democrat-map$ge_libdem
map$Liberal.Democrat <- ifelse((map$ge_snp!=0)&(map$ld_swing<(-0.1)),map$ge_libdem+0.5*map$ld_swing,map$Liberal.Democrat)
# scotmap <- map %>%filter(ge_snp!=0)
# write.csv(map,'Results/VI/Sept MRP/v2_map_withoverunder_PCindscotLD.csv')

#### ADJUSTING SWINGS #####
map <- read.csv('Results/VI/Sept MRP/v2_map_withoverunder_PCindscotLD.csv')
map <- map %>% select(const_name=Constituency,Conservative,Labour,
                      'Liberal Democrat'=Liberal.Democrat,
                      'Reform UK'=Reform.UK,                    
                      'The Green Party'=The.Green.Party,
                      'Scottish National Party (SNP)'=Scottish.National.Party..SNP.,
                      'Plaid Cymru'=Plaid.Cymru,                  
                      Other)
# calculate swings from GE
const_ge_results <- read.csv('https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv')
const_ge_results <- const_ge_results %>% mutate(const_name = ifelse(grepl("Ynys", const_ge_results$const_name),"Ynys Môn", const_ge_results$const_name))
const_ge_results <- const_ge_results%>% rename('ge_winner'='winner')

map_withge <- left_join(map,const_ge_results,by='const_name') %>% select(-region)

map_withge$con_swing <- map_withge$Conservative-map_withge$ge_con
map_withge$lab_swing <- map_withge$Labour-map_withge$ge_lab
map_withge$ref_swing <- map_withge$`Reform UK`-map_withge$ge_ref
map_withge$ld_swing <- map_withge$`Liberal Democrat`-map_withge$ge_libdem

GB_2024_vote <- data.frame(Con=c(.244), Lab=c(.347), LibDem=c(.126), Reform=c(.147), Green=c(.069), SNP=c(.026), PC=c(.007), Other=c(.036))

### calc UNS
raw_implied_vi <- read.csv('Results/VI/Sept MRP/vi_implied_raw.csv') %>% as.data.frame() %>% select(-X,-votes)
raw_implied_vi <- as.data.frame(pivot_wider(raw_implied_vi,names_from='Party',values_from='vi'))

con_nat_swing <- raw_implied_vi$Conservative - GB_2024_vote$Con
lab_nat_swing <- raw_implied_vi$Labour - GB_2024_vote$Lab
ref_nat_swing <-raw_implied_vi$`Reform UK` - GB_2024_vote$Ref
uns_labtoref <- (ref_swing-lab_swing)/2

### calc PNS
pns_lab <- raw_implied_vi$Labour/GB_2024_vote$Lab
map_withge$lab_pns <- map_withge$ge_lab*pns_lab - map_withge$ge_lab
pns_con <- raw_implied_vi$Conservative/GB_2024_vote$Con
map_withge$con_pns <-  map_withge$ge_con*pns_con - map_withge$ge_con
pns_ref <- raw_implied_vi$Reform/GB_2024_vote$Ref
map_withge$ref_pns <-  map_withge$ge_ref*pns_ref - map_withge$ge_ref

# graph Lab swings
# y_range <- range(c(map_withge$lab_swing, map_withge$lab_pns), na.rm = TRUE)
# # map_withge <- map_withge %>% arrange(ge_lab)
# # plot(map_withge$lab_swing, type = "p", 
# #      xlab = "Index", ylab = "Values", 
# #      main = "Lab Swing")
# plot(map_withge$ge_lab, map_withge$lab_swing,ylim = y_range, type = "p", col = "black")
# lines(map_withge$ge_lab, map_withge$lab_pns, col = "red")
# abline(h = lab_nat_swing, col = "blue", lty = 2)
# 
# # graph Con swings
# y_range <- range(c(map_withge$con_swing, map_withge$con_pns), na.rm = TRUE)
# plot(map_withge$ge_con, map_withge$con_swing,ylim = y_range, type = "p", col = "black")
# lines(map_withge$ge_con, map_withge$con_pns, col = "red")
# abline(h = con_nat_swing, col = "blue", lty = 2)
# 
# # graph Ref swings
# y_range <- range(c(map_withge$ref_swing, map_withge$ref_pns), na.rm = TRUE)
# plot(map_withge$ge_ref, map_withge$ref_swing,ylim = y_range, type = "p", col = "black")
# lines(map_withge$ge_ref, map_withge$ref_pns, col = "red")
# abline(h = ref_nat_swing, col = "blue", lty = 2)

# map_withge <- map_withge %>% arrange(ge_ref)
# plot(map_withge$ref_swing, type = "p", 
#      xlab = "Ordered by Reform Vote Share", ylab = "Ref Swing", 
#      main = "Ref Swing")
# lines(map_withge$ref_pns, col = "red")
# abline(h = ref_nat_swing, col = "blue", lty = 2)
# 
# map_withge <- map_withge %>% arrange(ge_lab)
# plot(map_withge$ref_swing, type = "p", 
#      xlab = "Ordered by Labour 2024 Vote Share", ylab = "Swing to Reform", 
#      main = "Ref Swing")
# # lines(map_withge$ref_pns, col = "red")
# abline(h = ref_nat_swing, col = "blue", lty = 2)

#### Making Labour's swing less over-proportional

map_withge <- map_withge %>% arrange(ge_lab)
map_withge$lab_residuals <- map_withge$lab_swing - map_withge$lab_pns
shrink_factor <- 0.7
map_withge$lab_swing_adj <- ifelse(map_withge$lab_residuals<0, map_withge$lab_pns + (map_withge$lab_residuals * shrink_factor),map_withge$lab_swing)
# map_withge$lab_swing_adj <-  map_withge$lab_pns + (map_withge$lab_residuals * shrink_factor)

# y_range <- range(c(map_withge$lab_swing_adj, map_withge$lab_pns), na.rm = TRUE)
# plot(map_withge$ge_lab, map_withge$lab_swing_adj,ylim = y_range, type = "p", col = "black")
# lines(map_withge$ge_lab, map_withge$lab_pns, col = "red")
# abline(h = lab_nat_swing, col = "blue", lty = 2)

### cap lab swings
map_withge$lab_swing_adj <- ifelse(map_withge$lab_swing_adj<(-0.15),(-0.15)+0.6*(map_withge$lab_swing_adj+0.15),map_withge$lab_swing_adj)

map_withge$Lab_adj <- map_withge$ge_lab+ map_withge$lab_swing_adj


map_withunwind <- map_withge %>% select(Constituency=const_name,Conservative,
                                        Labour=Lab_adj,
                                        'Liberal Democrat',
                                        'Reform UK',                    
                                        'The Green Party',
                                        'Scottish National Party (SNP)',
                                        'Plaid Cymru',                  
                                        Other)

map_withunwind$sum <-rowSums(map_withunwind[sapply(map_withunwind, is.numeric)])
map_withunwind[names(map_withunwind) != "sum" & sapply(map_withunwind, is.numeric)] <- 
  lapply(map_withunwind[names(map_withunwind) != "sum" & sapply(map_withunwind, is.numeric)], function(x) x / map_withunwind$sum)
map_withunwind <- map_withunwind %>% select(-sum)

map_withunwind$Winner <- max.col(map_withunwind %>% select(c(Conservative, Labour, `Liberal Democrat`,  `Reform UK`, `The Green Party`,`Scottish National Party (SNP)`, `Plaid Cymru`, Other)))
map_withunwind <- map_withunwind %>% mutate(Winner = case_when(Winner  == 1 ~ "Conservative", Winner  == 2 ~ "Labour", Winner  == 3 ~ "Liberal Democrat", Winner  == 5 ~ "The Green Party", Winner  == 4 ~ "Reform UK", Winner  == 6 ~ "Scottish National Party (SNP)", Winner  == 7 ~ "Plaid Cymru", Winner  == 8 ~ "Other"))
map_withunwind %>% group_by(Winner) %>% count()

# write.csv(map_withunwind,'Results/VI/Sept MRP/v2_postlabunwindcap.csv')

#### Con Unwinding

map <- read.csv('Results/VI/Sept MRP/v2_postlabunwindcap.csv')
map <- map %>% select(const_name=Constituency,Conservative,Labour,
                      'Liberal Democrat'=Liberal.Democrat,
                      'Reform UK'=Reform.UK,                    
                      'The Green Party'=The.Green.Party,
                      'Scottish National Party (SNP)'=Scottish.National.Party..SNP.,
                      'Plaid Cymru'=Plaid.Cymru,                  
                      Other)
# calculate swings from GE
const_ge_results <- read.csv('https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv')
const_ge_results <- const_ge_results %>% mutate(const_name = ifelse(grepl("Ynys", const_ge_results$const_name),"Ynys Môn", const_ge_results$const_name))
const_ge_results <- const_ge_results%>% rename('ge_winner'='winner')

map_withge <- left_join(map,const_ge_results,by='const_name') %>% select(-region)
# colnames(map)
map_withge$con_swing <- map_withge$Conservative-map_withge$ge_con

GB_2024_vote <- data.frame(Con=c(.244), Lab=c(.347), LibDem=c(.126), Reform=c(.147), Green=c(.069), SNP=c(.026), PC=c(.007), Other=c(.036))

### calc UNS
raw_implied_vi <- read.csv('Results/VI/Sept MRP/vi_implied_raw.csv') %>% as.data.frame() %>% select(-X,-votes)
raw_implied_vi <- as.data.frame(pivot_wider(raw_implied_vi,names_from='Party',values_from='vi'))

con_nat_swing <- raw_implied_vi$Conservative - GB_2024_vote$Con

### calc PNS
pns_con <- raw_implied_vi$Conservative/GB_2024_vote$Con
map_withge$con_pns <-  map_withge$ge_con*pns_con - map_withge$ge_con

map_withge <- map_withge %>% arrange(ge_con)
map_withge$con_residuals <- map_withge$con_swing - map_withge$con_pns
shrink_factor <- 0.5
map_withge$con_swing_adj <- map_withge$con_pns + (map_withge$con_residuals * shrink_factor)

# y_range <- range(c(map_withge$con_swing_adj, map_withge$con_pns), na.rm = TRUE)
# plot(map_withge$ge_con, map_withge$con_swing_adj,ylim = y_range, type = "p", col = "black")
# lines(map_withge$ge_con, map_withge$con_pns, col = "red")
# abline(h = con_nat_swing, col = "blue", lty = 2)

map_withge$Con_adj <- map_withge$ge_con+ map_withge$con_swing_adj

map_withunwind <- map_withge %>% select(Constituency=const_name,
                                        Conservative=Con_adj,
                                        Labour=Labour,
                                        'Liberal Democrat',
                                        'Reform UK',                    
                                        'The Green Party',
                                        'Scottish National Party (SNP)',
                                        'Plaid Cymru',                  
                                        Other)

map_withunwind$sum <-rowSums(map_withunwind[sapply(map_withunwind, is.numeric)])
map_withunwind[names(map_withunwind) != "sum" & sapply(map_withunwind, is.numeric)] <- 
  lapply(map_withunwind[names(map_withunwind) != "sum" & sapply(map_withunwind, is.numeric)], function(x) x / map_withunwind$sum)
map_withunwind <- map_withunwind %>% select(-sum)

map_withunwind$Winner <- max.col(map_withunwind %>% select(c(Conservative, Labour, `Liberal Democrat`,  `Reform UK`, `The Green Party`,`Scottish National Party (SNP)`, `Plaid Cymru`, Other)))
map_withunwind <- map_withunwind %>% mutate(Winner = case_when(Winner  == 1 ~ "Conservative", Winner  == 2 ~ "Labour", Winner  == 3 ~ "Liberal Democrat", Winner  == 5 ~ "The Green Party", Winner  == 4 ~ "Reform UK", Winner  == 6 ~ "Scottish National Party (SNP)", Winner  == 7 ~ "Plaid Cymru", Winner  == 8 ~ "Other"))
map_withunwind %>% group_by(Winner) %>% count()

### cap con swings
map_withge$con_swing_adj <- ifelse(map_withge$con_swing_adj<(-0.06),(-0.06)+0.5*(map_withge$con_swing_adj+0.06),map_withge$con_swing_adj)

map_withge$Con_adj <- map_withge$ge_con+ map_withge$con_swing_adj

# y_range <- range(c(map_withge$con_swing_adj, map_withge$con_pns), na.rm = TRUE)
plot(map_withge$ge_con, map_withge$con_swing_adj,ylim = y_range, type = "p", col = "black")
lines(map_withge$ge_con, map_withge$con_pns, col = "red")
abline(h = con_nat_swing, col = "blue", lty = 2)

map_withunwind <- map_withge %>% select(Constituency=const_name,
                                        Conservative=Con_adj,
                                        Labour,
                                        'Liberal Democrat',
                                        'Reform UK',                    
                                        'The Green Party',
                                        'Scottish National Party (SNP)',
                                        'Plaid Cymru',                  
                                        Other)

map_withunwind$sum <-rowSums(map_withunwind[sapply(map_withunwind, is.numeric)])
map_withunwind[names(map_withunwind) != "sum" & sapply(map_withunwind, is.numeric)] <- 
  lapply(map_withunwind[names(map_withunwind) != "sum" & sapply(map_withunwind, is.numeric)], function(x) x / map_withunwind$sum)
map_withunwind <- map_withunwind %>% select(-sum)

map_withunwind$Winner <- max.col(map_withunwind %>% select(c(Conservative, Labour, `Liberal Democrat`,  `Reform UK`, `The Green Party`,`Scottish National Party (SNP)`, `Plaid Cymru`, Other)))
map_withunwind <- map_withunwind %>% mutate(Winner = case_when(Winner  == 1 ~ "Conservative", Winner  == 2 ~ "Labour", Winner  == 3 ~ "Liberal Democrat", Winner  == 5 ~ "The Green Party", Winner  == 4 ~ "Reform UK", Winner  == 6 ~ "Scottish National Party (SNP)", Winner  == 7 ~ "Plaid Cymru", Winner  == 8 ~ "Other"))
map_withunwind %>% group_by(Winner) %>% count()

# write.csv(map_withunwind,'Results/VI/Sept MRP/v2_postlabconunwind.csv')

#### Con Boost
map <- read.csv('Results/VI/Sept MRP/v2_postlabconunwind.csv')
numeric_cols <- names(map)[sapply(map, is.numeric) & !names(map) %in% c("X", "Conservative")]
map$Conservative_vs_Max_Other <- do.call(pmax, map[numeric_cols]) - map$Conservative
map$Conservative <- ifelse((map$Conservative_vs_Max_Other<0.01)&(map$'Plaid.Cymru'==0),map$Conservative+0.01,map$Conservative)

map <- map %>% select(const_name=Constituency,Conservative,Labour,
                      'Liberal Democrat'=Liberal.Democrat,
                      'Reform UK'=Reform.UK,                    
                      'The Green Party'=The.Green.Party,
                      'Scottish National Party (SNP)'=Scottish.National.Party..SNP.,
                      'Plaid Cymru'=Plaid.Cymru,                  
                      Other)

map$sum <-rowSums(map[sapply(map, is.numeric)])
map[names(map) != "sum" & sapply(map, is.numeric)] <- 
  lapply(map[names(map) != "sum" & sapply(map, is.numeric)], function(x) x / map$sum)
map <- map %>% select(-sum)

map$Winner <- max.col(map %>% select(c(Conservative, Labour, `Liberal Democrat`,  `Reform UK`, `The Green Party`,`Scottish National Party (SNP)`, `Plaid Cymru`, Other)))
map <- map %>% mutate(Winner = case_when(Winner  == 1 ~ "Conservative", Winner  == 2 ~ "Labour", Winner  == 3 ~ "Liberal Democrat", Winner  == 5 ~ "The Green Party", Winner  == 4 ~ "Reform UK", Winner  == 6 ~ "Scottish National Party (SNP)", Winner  == 7 ~ "Plaid Cymru", Winner  == 8 ~ "Other"))
map %>% group_by(Winner) %>% count()

# write.csv(map,'Results/VI/Sept MRP/v2_postoverlays.csv')

#### FoMic overlay
map <- read.csv('Results/VI/Sept MRP/v2_postoverlays.csv')
# colnames(map)
# map$'Reform.UK' <- ifelse(map$const_name=="Sevenoaks",map$'Reform.UK'-0.015,map$'Reform.UK')
# map$Labour <- ifelse(map$Constituency=="Hitchin",map$Labour+0.015,map$Labour)
# map$"Liberal.Democrat"  <- ifelse(map$Constituency=="Hitchin",map$"Liberal.Democrat"-0.01,map$"Liberal.Democrat")
# map$'Plaid.Cymru' <- ifelse(map$Constituency=="Ynys Môn",map$'Plaid.Cymru'+0.05,map$'Plaid.Cymru')

map <- map %>% select(Constituency,Conservative,Labour,
                      'Liberal Democrat'=Liberal.Democrat,
                      'Reform UK'=Reform.UK,                    
                      'The Green Party'=The.Green.Party,
                      'Scottish National Party (SNP)'=Scottish.National.Party..SNP.,
                      'Plaid Cymru'=Plaid.Cymru,                  
                      Other)

map$sum <-rowSums(map[sapply(map, is.numeric)])
map[names(map) != "sum" & sapply(map, is.numeric)] <- 
  lapply(map[names(map) != "sum" & sapply(map, is.numeric)], function(x) x / map$sum)
map <- map %>% select(-sum)

map$Winner <- max.col(map %>% select(c(Conservative, Labour, `Liberal Democrat`,  `Reform UK`, `The Green Party`,`Scottish National Party (SNP)`, `Plaid Cymru`, Other)))
map <- map %>% mutate(Winner = case_when(Winner  == 1 ~ "Conservative", Winner  == 2 ~ "Labour", Winner  == 3 ~ "Liberal Democrat", Winner  == 5 ~ "The Green Party", Winner  == 4 ~ "Reform UK", Winner  == 6 ~ "Scottish National Party (SNP)", Winner  == 7 ~ "Plaid Cymru", Winner  == 8 ~ "Other"))
map %>% group_by(Winner) %>% count()

# write.csv(map,'Results/VI/Sept MRP/v2.2_postoverlays.csv')

####### Totals and VI ########

df<- read.csv('Results/VI/Sept MRP/v2.2_postoverlays.csv')

###counts
df %>% group_by(Winner) %>% count()

### implied vi
electorates <- read.csv('/Users/anouschka/Library/CloudStorage/OneDrive-SharedLibraries-MoreInCommon/More In Common Team Site - MRP/Data/Constituency Data/electorate_size_const.csv') %>% select(Constituency=Constituency.name,Electorate)
electorates$Constituency <- ifelse(grepl("Montgomeryshire",electorates$Constituency),"Montgomeryshire and Glyndwr", electorates$Constituency)
electorates$Constituency <- ifelse(grepl("Ynys",electorates$Constituency),"Ynys Môn", electorates$Constituency)
grouped_withwnv <- read.csv('/Users/anouschka/Library/CloudStorage/OneDrive-SharedLibraries-MoreInCommon/More In Common Team Site - MRP/Results/VI/April MRP/preds_withwnv.csv')
grouped_withwnv <- grouped_withwnv %>% select(-X) %>% ungroup() %>% pivot_wider(names_from=level,values_from=pred)
grouped_withwnv <- grouped_withwnv%>% rename(Constituency=const_name)
grouped_withwnv$turnout <- 1-grouped_withwnv$'WNV'
electorates <- left_join(electorates,grouped_withwnv%>%select(Constituency,turnout),by='Constituency')
electorates$voters <- as.numeric(electorates$Electorate)*electorates$turnout
electorates <- electorates %>% select(Constituency,voters)

# map <-read.csv('/Users/anouschka/Library/CloudStorage/OneDrive-SharedLibraries-MoreInCommon/More In Common Team Site - MRP/Results/VI/April MRP/map_withids.csv') %>% select(-X)
map <- df
colnames(map)
# map$Constituency<-map$const_name
map$Constituency <- ifelse(grepl("Montgomeryshire",map$Constituency),"Montgomeryshire and Glyndwr", map$Constituency)
map$Constituency <- ifelse(grepl("Ynys",map$Constituency),"Ynys Môn", map$Constituency)
# map <- map %>% select(-X.1,-orig_winner,-Winner,-GE_winner,-Change,-ID)
vi_implied <- map  %>% pivot_longer(cols=c("Conservative","Labour", "Liberal.Democrat" ,"Reform.UK"    ,
                                                                                                       "The.Green.Party" , "Scottish.National.Party..SNP." ,"Plaid.Cymru"   ,
                                                                                                       "Other"),
                                                                                                names_to='Party',
                                                                                                values_to='voteshare')

# vi_implied <- map %>% pivot_longer(cols=c("Conservative","Labour", "Liberal Democrat" ,"Reform UK"    ,
#                                           "The Green Party" , "Scottish National Party (SNP)" ,"Plaid Cymru"   ,
#                                           "Other"),
#                                    names_to='Party',
#                                    values_to='voteshare')
vi_implied <- vi_implied %>% left_join(electorates,by="Constituency")
vi_implied$party_voters <- vi_implied$voteshare*vi_implied$voters
vi_implied <- vi_implied %>% group_by(Party) %>% summarize(votes = sum(party_voters))
totalvotes <- sum(vi_implied$votes)
vi_implied <- vi_implied %>% mutate(vi = votes/totalvotes)
write.csv(vi_implied,'Results/VI/Sept MRP/vi_implied_postoverlays.csv')


################ Productising
#### Adding change and IDs for flourish ####

elex_winners <- read.csv('/Users/anouschka/Library/CloudStorage/OneDrive-SharedLibraries-MoreInCommon/More In Common Team Site - MRP/Scripts/2025 MRP/ge_winners.csv')
elex_winners<-elex_winners%>% rename(Constituency=Constituency.name)
elex_winners <- elex_winners %>% mutate(GE_winner = case_when(Winner  == 'Con' ~ "Conservative",
                                                              Winner  == 'Lab' ~ "Labour",
                                                              Winner  == 'LD' ~ "Liberal Democrat",
                                                              Winner  == 'Green' ~ "The Green Party",
                                                              Winner  == 'RUK' ~ "Reform UK",
                                                              Winner  == 'SNP' ~ "Scottish National Party (SNP)",
                                                              Winner== "PC" ~ "Plaid Cymru",
                                                              Winner  == "Ind" ~ "Other")) %>% select(Constituency,GE_winner)
map<-map %>% left_join(elex_winners,by='Constituency')
map$GE_winner <- ifelse(map$GE_winner=='Scottish National Party (SNP)','SNP',
                        ifelse(map$GE_winner=='Other','Independent',
                               ifelse(map$GE_winner=='The Green Party','Green',
                                      map$GE_winner)))
map$Winner <- ifelse(map$Winner=='Scottish National Party (SNP)','SNP',
                     ifelse(map$Winner=='Other','Independent',
                            ifelse(map$Winner=='The Green Party','Green',
                                   map$Winner)))
map$Change <- ifelse(map$GE_winner==map$Winner, paste0(map$Winner,' HOLD'),paste0(map$Winner, ' GAIN from ',map$GE_winner))
ids <- read.csv('/Users/anouschka/Library/CloudStorage/OneDrive-SharedLibraries-MoreInCommon/More In Common Team Site - MRP/Scripts/2025 MRP/bar_id_lookup.csv')
map <- map %>% left_join(ids,by='Change')
map %>% write.csv('/Users/anouschka/Library/CloudStorage/OneDrive-SharedLibraries-MoreInCommon/More In Common Team Site - MRP/Results/VI/Sept MRP/map_withids.csv')
