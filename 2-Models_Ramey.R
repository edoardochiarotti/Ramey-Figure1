
rm(list=ls(all=TRUE))

# SET WD (CHANGE IT AS NEEDED)
mywd <- "~/Documents/GitHub/Ramey-Figure1"
setwd(mywd)

# GET FUNCTIONS
source("1-Functions_Ramey.R")

# GET PACKAGES
detachAllPackages()

if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if (!require(magrittr)) {
  install.packages("magrittr")
  require(magrittr)
}
if (!require(vars)) {
  install.packages("vars")
  require(vars)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}

# THEME
myThemeStuff <- theme(panel.background = element_rect(fill = NA),
                      panel.border = element_rect(fill = NA, color = "black"),
                      panel.grid.major.y = element_line(color = "gray", size = 0.1),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      axis.ticks = element_line(color = "gray5"),
                      axis.text = element_text(color = "black", size = 10),
                      axis.title = element_text(color = "black", size = 10),
                      plot.title = element_text(hjust = 0.5))


##################################################################################
################################# RAMEY FIGURE 1 #################################
##################################################################################

# LOAD
load("df_mp")
df1 <- df_mp
colnames(df1)

# GET THE RIGHT VARIABLES
df1 <- df1[,c("month",
              "nbr","tr","vault",
              "ip_ramey","unemp_ramey","cpi_ramey","pcom_ramey",
              "ffr_ramey","nbr_ramey","tr_ramey","m1_ramey")]
colnames(df1) <- c("month","nbr_new","tr_new","vault",
                   "ip","unemp","cpi","pcom",
                   "ffr","nbr","tr","m1")

# MAKE ALTERNATIVE VARIABLES FOR RESERVES
df1 <- cbind(df1, tr_alt = df1$tr_new - df1$vault)
df1 <- cbind(df1, nbr_alt = df1$nbr_new - df1$vault)

# tr_asramey_usa: RESBALNS
# tr_ramey_usa: from Ramey Moneyary Data Excel (RESBALNS)
# trarr_usa: TRARR
# tr_usa: TOTRESNS
# tr_alt_usa: TOTRESNS - VAULT
# nbr_asramey_usa: BOGNONBR
# nbr_ramey_usa: from Ramey Moneyary Data Excel (BOGNONBR)
# nbr_usa: NONBORRES
# nbr_alt_usa: NONBORRES - VAULT

# ADD NATION NAME
df1 <- cbind(state = "United States", df1)

# LOGS
myvariabs <- colnames(df1)[!colnames(df1) %in% c("state","month")]
for(i in 1:length(myvariabs)){
  df1 <- cbind(df1, log(df1[,myvariabs[i]]))
  colnames(df1)[ncol(df1)] <- paste("l",myvariabs[i],sep = "")
}
colnames(df1)[!sapply(1:length(colnames(df1)), function(x){
  all(!is.nan(df1[,x]))
})]

# DEFINE STUFF
myindexes <- c("state","month")
mynahead <- 48
mynlags <- 12
mytype <- "both"
mystate <- unique(df1$state)

##############################
##### FIGURE 1: BASELINE #####
##############################

myvariabs <- c("lip","unemp","lcpi","lpcom",
               "ffr","lnbr","ltr","lm1")
myimpulse <- myvariabs[grep("ffr",myvariabs,ignore.case = T)]
myspecname <- c("1965-95","1983-07","1983-07-NoMoney")
mycomblist <- list(
  list(start_date = "1965-01", end_date = "1995-06", variabs = myvariabs),
  list(start_date = "1983-01", end_date = "2007-12", variabs = myvariabs),
  list(start_date = "1983-01", end_date = "2007-12", variabs = myvariabs[1:5])
)

gg1 <- graphIRF_RameyCEE(data = df1, comblist = mycomblist, variabs = myvariabs,
                         n.ahead = mynahead, indexes = myindexes, impulse = myimpulse, 
                         spec.name = myspecname, p = mynlags, state = mystate, type = "const")
gg1$graph


############################
##### FIGURE 1: UPDATE #####
############################

myvariabs <- c("lip","unemp","lcpi","lpcom",
               "ffr","lnbr_alt","ltr","lm1")
myimpulse <- myvariabs[grep("ffr",myvariabs,ignore.case = T)]
myspecname <- c("1965-95","1983-07","1983-07-NoMoney")
mycomblist <- list(
  list(start_date = "1965-01", end_date = "1995-06", variabs = myvariabs),
  list(start_date = "1983-01", end_date = "2007-12", variabs = myvariabs),
  list(start_date = "1983-01", end_date = "2007-12", variabs = myvariabs[1:5])
)

gg1 <- graphIRF_RameyCEE(data = df1, comblist = mycomblist, variabs = myvariabs,
                         n.ahead = mynahead, indexes = myindexes, impulse = myimpulse, 
                         spec.name = myspecname, p = mynlags, state = mystate, type = "const")
gg1$graph

##############################
##### FIGURE 1: APPENDIX #####
##############################

myvariabs <- c("lip","unemp","lcpi","lpcom",
               "ffr","lnbr_alt","ltr_alt","lm1")
myimpulse <- myvariabs[grep("ffr",myvariabs,ignore.case = T)]
myspecname <- c("1965-95","1983-07","1983-07-NoMoney")
mycomblist <- list(
  list(start_date = "1965-01", end_date = "1995-06", variabs = myvariabs),
  list(start_date = "1983-01", end_date = "2007-12", variabs = myvariabs),
  list(start_date = "1983-01", end_date = "2007-12", variabs = myvariabs[1:5])
)

gg1 <- graphIRF_RameyCEE(data = df1, comblist = mycomblist, variabs = myvariabs,
                         n.ahead = mynahead, indexes = myindexes, impulse = myimpulse, 
                         spec.name = myspecname, p = mynlags, state = mystate, type = "const")
gg1$graph

