# --------------------------------------------------------------------------- #
# ------------- Commodity Price Boom and its Impact on Wages ---------------- #
# --------------------------------------------------------------------------- #

# --------------- Reading and cleaning data --------------------------------- #

options(scipen=999)

rm(list=ls())
wd <- "C:/Users/Hriday/Documents/R/fentareas/MIC_III/Research"
setwd(wd)

library(haven);library(dplyr);library(ggplot2);library(readxl);library(fixest);library(MatchIt)

# Read casen (datasets) to use
periodo <- rbind(2000,2003,2006,2009)
for (anio in 1:length(periodo)) {
    #anio=1
    base <- read_dta(paste0("data/casen",periodo[anio],".dta"))
    base$exp <- base$edad - base$esc - 6
    base <- base %>% filter(exp<=60 & exp>=0)
    
    # Input output linkages
    # Read Input Output Tables and calculate total Input and Output of each Sector 
    iot <- read.csv(paste0("data/CHL",periodo[anio],"dom.csv"))
    iot <- iot[1:44,1:45]
    rownames(iot) <- iot$X;iot$X<-NULL
    
    iot_casen_sectors <- readxl::read_xlsx("data/IOT_CASEN_Industry_Sector.xlsx",sheet="CASEN_IOT_Sectors")
    # Output (sales)
    iotaux<-iot;iotaux$rama <- iot_casen_sectors$Compatib_IOT_98
    iotoutput <- iotaux %>% filter(rama==2);iotoutput <- data.frame(output = colSums(iotoutput))  
    # Input (supplies)
    iotaux <- data.frame(t(iot));iotaux$rama<-iot_casen_sectors$Compatib_IOT_98;
    iotinput<-iotaux %>% filter(rama==2);iotinput<-data.frame(input=colSums(iotinput));
    
    # Aggregate by Casen sectors
    iotsum <- data.frame(input = iotinput[1:44,], output = iotoutput[1:44,])
    iotsum$rama <- iot_casen_sectors$Compatib_IOT_98
    iotsum <- iotsum %>% group_by(rama) %>% summarise(input=sum(input),output=sum(output))
    iotsum <- iotsum %>% mutate(inputshare = input/sum(input),outputshare=output/sum(output))
    
    base$input<-NA;base$output<-NA;base$inputshare<-NA;base$outputshare<-NA;
    ios <- seq(0,9)
    for (io in 1:length(ios)) {
        base$input <- ifelse(base$rama == ios[io],iotsum$input[io],base$input)
        base$output <- ifelse(base$rama == ios[io],iotsum$output[io],base$output)
        base$inputshare <- ifelse(base$rama == ios[io],iotsum$inputshare[io],base$inputshare)
        base$outputshare <- ifelse(base$rama == ios[io],iotsum$outputshare[io],base$outputshare)
    }
    assign(paste0("c",periodo[anio]),base)
    rm(list="base")
}

c2006<-c2006%>%rename("comu"="comuna");c2009<-c2009%>%rename("comu"="comuna","r"="region");
# c2011<-c2011%>%rename("comu"="comuna","r"="region","rama"="rama1") 

# Load CPI to use real wages
ipc <- readxl::read_excel("data/ipc.xlsx",sheet=3)
ipc$ajuste <- ipc$IPC[5]/ipc$IPC
ipc$year <- c(1996,1998,2000,2003,2006,2009,2011,2013,2015,2017)
ipc <- select(ipc,c(year,ajuste))

# Save variables that will be used, create real wage variable
cas03 <- c2003 %>% select(comu,esc,edad,exp,rama,sexo,yopraj,r,input,output,inputshare,outputshare) %>% filter(yopraj>30000) %>%
    mutate(trat = ifelse(rama==2,1,0),ingcor = yopraj*as.numeric(ipc[4,2]),anio=2003) %>% filter(!is.na(trat))

cas06 <- c2006 %>% select(comu,esc,edad,exp,rama,sexo,yopraj,r,input,output,inputshare,outputshare) %>% filter(yopraj>30000) %>% 
    mutate(trat = ifelse(rama==2,1,0),ingcor = yopraj*as.numeric(ipc[5,2]),anio=2006) %>% filter(!is.na(trat))

cas09 <- c2009 %>% select(comu,esc,edad,exp,rama,sexo,yopraj,r,input,output,inputshare,outputshare) %>% filter(yopraj>30000) %>% 
    mutate(trat = ifelse(rama==2,1,0),ingcor = yopraj*as.numeric(ipc[6,2]),anio=2009) %>% filter(!is.na(trat))

cas00 <- c2000 %>% select(comu,esc,edad,exp,rama,sexo,yopraj,r,input,output,inputshare,outputshare) %>% filter(yopraj>30000) %>% 
    mutate(trat = ifelse(rama==2,1,0),ingcor = yopraj*as.numeric(ipc[3,2]),anio=2000) %>% filter(!is.na(trat))

cas03$comu<-as.numeric(cas03$comu);cas06$comu<-as.numeric(cas06$comu);cas09$comu<-as.numeric(cas09$comu)
cas00$comu<-as.numeric(cas00$comu);cas00$rama<-as.numeric(cas00$rama)
cas03$r<-as.numeric(cas03$r);cas06$r<-as.numeric(cas06$r);cas09$r<-as.numeric(cas09$r);cas00$r<-as.numeric(cas00$r)


# ---------------------------------- MATCHING -------------------------------- #

# Save in one variable only treated individuals for matching
min0300 <- merge(cas03[cas03$trat==1,],cas00[cas00$trat==1,],all=T) %>% mutate(match=ifelse(anio==2003,1,0))
min0306 <- merge(cas03[cas03$trat==1,],cas06[cas06$trat==1,],all=T) %>% mutate(match=ifelse(anio==2003,1,0))
# min0309 <- merge(cas03[cas03$trat==1,],cas09[cas09$trat==1,],all=T) %>% mutate(match=ifelse(anio==2003,1,0))
# min0311 <- merge(cas03[cas03$trat==1,],cas11[cas11$trat==1,],all=T) %>% mutate(match=ifelse(anio==2003,1,0))

# First matching: find twins of treated group, define treated group
trat_cont03 <- matchit(trat ~ r + esc + edad + exp + sexo,data = cas03,
                 method = "nearest",distance="glm",ratio=8,replace=F)
base03 <- match.data(trat_cont03)
# Second matching: Considering the treated group of 2003, find twins in 2006
trat03_trat06 <- matchit(match ~ r + esc + edad + exp + sexo,data = min0306,
                       method = "nearest",distance="glm",ratio=1,replace=F)
# Save matched data
minero0306 <- match.data(trat03_trat06)
# Save matched miners from 2006 and merge base with treated from 2009, in order to later match 2006 with 2009
min0609 <- merge(minero0306[minero0306$anio==2006,],cas09[cas09$trat==1,],all=T) %>% mutate(match=ifelse(anio==2006,1,0)) %>% 
    select(-c("distance","weights","subclass"))
trat06_trat09 <- matchit(match ~ r + esc + edad + exp + sexo,data = min0609,
                         method = "nearest",distance="glm",ratio=1,replace=F)
minero0609 <- match.data(trat06_trat09)

trat03_trat00 <- matchit(match ~ r + esc + edad + exp + sexo,data = min0300,
                         method = "nearest",distance="glm",ratio=1,replace=F)
minero0300 <- match.data(trat03_trat00)

# Base to match treated and controles in 2006, 2009 and 2011
basenomatch06 <- merge(minero0306[minero0306$anio==2006,],cas06,by=colnames(cas06),all.y = T)
basenomatch06 <- basenomatch06 %>% filter(trat==1&!is.na(distance)|trat==0) %>% 
    select(!c("match","distance","weights","subclass"))
basenomatch09 <- merge(minero0609[minero0609$anio==2009,],cas09,by=colnames(cas09),all.y = T)
basenomatch09 <- basenomatch09 %>% filter(trat==1&!is.na(distance)|trat==0) %>% 
    select(!c("match","distance","weights","subclass"))
basenomatch00 <- merge(minero0300[minero0300$anio==2000,],cas00,by=colnames(cas00),all.y = T)
basenomatch00 <- basenomatch00 %>% filter(trat==1&!is.na(distance)|trat==0) %>% 
    select(!c("match","distance","weights","subclass"))
# Third matching: Find twins from the treated group, in order to define control group from 2006, 2009 and 2011
trat_cont06 <- matchit(trat ~ r + esc + edad + exp + sexo,data = basenomatch06,
                       method = "nearest",distance="glm",ratio=8,replace=F)
trat_cont09 <- matchit(trat ~ r + esc + edad + exp + sexo,data = basenomatch09,
                       method = "nearest",distance="glm",ratio=8,replace=F)
trat_cont00 <- matchit(trat ~ r + esc + edad + exp + sexo,data = basenomatch00,
                       method = "nearest",distance="glm",ratio=8,replace=F)
# Save matched data
base06 <- match.data(trat_cont06)
base09 <- match.data(trat_cont09)
base00 <- match.data(trat_cont00)

# # Check summary of each matching
# summary(trat_cont03)
# 
# plot(trat_cont03, type = "jitter", interactive = FALSE)
# plot(trat_cont03, type = "hist", interactive = FALSE) # ESTE MONO PUEDE IR PAL PAPER, NO NECESARIAMENTE ESTE ANIO
# png("psm_tratctrl06.png")
# plot(trat_cont06, type = "hist", interactive = FALSE) # ESTE DEBERIA IR AL PAPER
# dev.off()
# plot(summary(trat_cont03), abs = FALSE)
# 
# # ----
# summary(trat03_trat06)
# 
# plot(trat03_trat06, type = "jitter", interactive = FALSE)
# plot(trat03_trat06, type = "hist", interactive = FALSE)
# plot(summary(trat03_trat06), abs = FALSE)
# 
# # -------
# summary(trat_cont06)
# 
# plot(trat03_trat06, type = "jitter", interactive = FALSE)
# plot(summary(trat03_trat06), abs = FALSE)

# ------
# Compare means without doing regressions, match and no match
# No match
cas03 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
cas06 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
cas09 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
cas00 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
# Match
base03 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
base06 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
base09 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
base00 %>% group_by(trat) %>% summarise(ing = mean(ingcor))

# ---------

# Join merged bases from each year
basematch <- merge(base03,base06,all = T)
basematch <- merge(basematch,base09,all = T)
basematch <- merge(basematch,base00,all = T)
basematch$shock <- ifelse(basematch$anio>=2006,1,0)

# basematch <- merge(cas03,cas06,all = T)
# basematch <- merge(basematch,cas09,all = T)
# basematch <- merge(basematch,cas11,all = T)
# basematch$shock <- ifelse(basematch$anio>=2006,1,0)

# 2011 is not so good
basematch <- basematch %>% filter(anio>=2003)
# Dummy of high school
basematch$edmedia <- ifelse(basematch$esc>12,1,0)

# Regressions
library(estimatr);library(lfe)
regdid <- lm_robust(log(ingcor)~trat*shock,data=basematch,se_type = "HC1")
regmincer <- lm_robust(log(ingcor)~trat*shock + esc + exp + I(exp^2),data=basematch,se_type = "HC1")
regctrl <- lm_robust(log(ingcor)~trat*shock + esc + exp + I(exp^2)+sexo+edmedia,data=basematch,se_type = "HC1")
summary(regdid)
summary(regmincer)
summary(regctrl)

library(texreg)

regrows <- c("Intercept","Treatment","Shock","Treatment*Shock","Education","Experience","Experience$^2$","Gender","High School")
texreg(list(regdid,regmincer,regctrl),stars = c(0.01,0.05,0.1),file = "tablas_figuras/mainDiD.tex", caption.above = T,
       custom.coef.names = c(regrows),include.ci=F,include.rmse=F,digits=3,caption="Difference-in-difference estimation",
       custom.model.names = c("ln(Wage)","ln(Wage)","ln(Wage)"),float.pos = "H",
       custom.gof.rows = list("Controls" = c("No","Yes","Yes")))

reglink <- lm_robust(log(ingcor)~inputshare*shock,data=basematch,se_type = "HC1")
reglinkctrl <- lm_robust(log(ingcor)~inputshare*shock+esc+exp+I(exp^2),data=basematch,se_type = "HC1")
reglinkdid <- lm_robust(log(ingcor)~trat*shock + esc + exp + I(exp^2)+inputshare,data=basematch,se_type = "HC1")
summary(reglink)
summary(reglinkctrl)
summary(reglinkdid)

regrows <- c("Intercept","Input Shares","Shock","Input Shares*Shock","Education","Experience","Experience$^2$","Treatment","Treatment*Shock")
texreg(list(reglink,reglinkctrl,reglinkdid),stars = c(0.01,0.05,0.1),file = "tablas_figuras/linkDiD.tex", caption.above = T,
       custom.coef.names = c(regrows),include.ci=F,include.rmse=F,digits=3,caption="Difference-in-difference estimation",
       custom.model.names = c("ln(Wage)","ln(Wage)","ln(Wage)"),float.pos = "H",
       custom.gof.rows = list("Controls" = c("No","Yes","Yes")))

# reg <- lm_robust(log(ingcor)~trat*shock + esc + exp + I(exp^2),data=basematch,se_type = "HC1")
reg <- felm(log(ingcor)~trat*shock + esc + exp + I(exp^2)|rama,data=basematch)
summary(reg)

# ----------------------------- SUMMARY STATISTICS --------------------------- #

summ_stat<-basematch%>%group_by(anio,trat)%>%summarise(xingcor=mean(ingcor),xesc=mean(esc),xexp=mean(exp),obs=n()) %>% 
    mutate(xingcorctrl = lag(xingcor),xescctrl=lag(xesc),xexpctrl=lag(xexp),obsctrl=lag(obs)) %>% filter(trat==1)
summ_stat <- summ_stat[,c(1,3,7,4,8,5,9,6,10)]

print(xtable::xtable(summ_stat,caption = "Summary Statistics",label="tab:summ_stat"),
      include.rownames=F,table.placement="H",caption.placement="top",
      file="tablas_figuras/summary_stat.tex")

# --------------------------- INPUT OUTPUT LINKAGES ------------------------- #



# --------------------- PLACEBO ---------------------------------------------- #

basematch <- merge(base03,base06,all = T)
basematch <- merge(basematch,base09,all = T)
basematch <- merge(basematch,base00,all = T)
basematch$shock <- ifelse(basematch$anio>=2003,1,0)

basematch <- basematch %>% filter(anio<2009)
# Dummy of high school
basematch$edmedia <- ifelse(basematch$esc>12,1,0)

# Regressions
regdid <- lm_robust(log(ingcor)~trat*shock,data=basematch,se_type = "HC1")
regmincer <- lm_robust(log(ingcor)~trat*shock + esc + exp + I(exp^2),data=basematch,se_type = "HC1")
regctrl <- lm_robust(log(ingcor)~trat*shock + esc + exp + I(exp^2)+sexo+edmedia,data=basematch,se_type = "HC1")
summary(regdid)
summary(regmincer)
summary(regctrl)

regrows <- c("Intercept","Treatment","Shock","Treatment*Shock","Education","Experience","Experience$^2$","Gender","High School")
texreg(list(regdid,regmincer,regctrl),stars = c(0.01,0.05,0.1),file = "tablas_figuras/placeboDiD.tex", caption.above = T,
       custom.coef.names = c(regrows),include.ci=F,include.rmse=F,digits=3,caption="Placebo Difference-in-difference estimation",
       custom.model.names = c("ln(Wage)","ln(Wage)","ln(Wage)"),float.pos = "H",
       custom.gof.rows = list("Controls" = c("No","Yes","Yes")))


# -------------- Agricultural Sector as control group  ---------------------- #

cas03ag <- cas03 %>% filter(rama==1|rama==2)
cas06ag <- cas06 %>% filter(rama==1|rama==2)
cas09ag <- cas09 %>% filter(rama==1|rama==2)


# Save in one variable only treated individuals forr matching
min0306 <- merge(cas03ag[cas03ag$trat==1,],cas06ag[cas06ag$trat==1,],all=T) %>% mutate(match=ifelse(anio==2003,1,0))
# min0309 <- merge(cas03[cas03$trat==1,],cas09[cas09$trat==1,],all=T) %>% mutate(match=ifelse(anio==2003,1,0))
# min0311 <- merge(cas03[cas03$trat==1,],cas11[cas11$trat==1,],all=T) %>% mutate(match=ifelse(anio==2003,1,0))

# First matching: save twins from treated group to define control group
trat_cont03 <- matchit(trat ~ r + esc + edad + exp + sexo,data = cas03ag,
                       method = "nearest",distance="glm",ratio=8,replace=F)
base03 <- match.data(trat_cont03)
# Second matching: considering treated group from 2003, find twins in 2006
trat03_trat06 <- matchit(match ~ r + esc + edad + exp + sexo,data = min0306,
                         method = "nearest",distance="glm",ratio=1,replace=F)
# Save matched data
minero0306 <- match.data(trat03_trat06)
# Save only miner matched guys from 2006 and merge base with treated 2009, in order to later match 2006 with 2006 and 2009
min0609 <- merge(minero0306[minero0306$anio==2006,],cas09ag[cas09ag$trat==1,],all=T) %>% mutate(match=ifelse(anio==2006,1,0)) %>% 
    select(-c("distance","weights","subclass"))
trat06_trat09 <- matchit(match ~ r + esc + edad + exp + sexo,data = min0609,
                         method = "nearest",distance="glm",ratio=1,replace=F)
minero0609 <- match.data(trat06_trat09)


# Base to match treated and controls in 2006, 2009 and 2011
basenomatch06 <- merge(minero0306[minero0306$anio==2006,],cas06ag,by=colnames(cas06ag),all.y = T)
basenomatch06 <- basenomatch06 %>% filter(trat==1&!is.na(distance)|trat==0) %>% 
    select(!c("match","distance","weights","subclass"))
basenomatch09 <- merge(minero0609[minero0609$anio==2009,],cas09ag,by=colnames(cas09ag),all.y = T)
basenomatch09 <- basenomatch09 %>% filter(trat==1&!is.na(distance)|trat==0) %>% 
    select(!c("match","distance","weights","subclass"))
# Third matching: find twins from treated group to define control group in 2006, 2009 and 2011
trat_cont06 <- matchit(trat ~ r + esc + edad + exp + sexo,data = basenomatch06,
                       method = "nearest",distance="glm",ratio=8,replace=F)
trat_cont09 <- matchit(trat ~ r + esc + edad + exp + sexo,data = basenomatch09,
                       method = "nearest",distance="glm",ratio=8,replace=F)
# save matched data
base06 <- match.data(trat_cont06)
base09 <- match.data(trat_cont09)

# ------
# Compare means without regressions, match and no match
# No match
cas03ag %>% group_by(trat) %>% summarise(ing = mean(ingcor))
cas06ag %>% group_by(trat) %>% summarise(ing = mean(ingcor))
cas09ag %>% group_by(trat) %>% summarise(ing = mean(ingcor))
# Match
base03 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
base06 %>% group_by(trat) %>% summarise(ing = mean(ingcor))
base09 %>% group_by(trat) %>% summarise(ing = mean(ingcor))

# ---------

# join bases matched by year
basematch <- merge(base03,base06,all = T)
basematch <- merge(basematch,base09,all = T)
basematch$shock <- ifelse(basematch$anio>2003,1,0)

# basematch <- merge(cas03,cas06,all = T)
# basematch <- merge(basematch,cas09,all = T)
# basematch <- merge(basematch,cas11,all = T)
# basematch$shock <- ifelse(basematch$anio>=2006,1,0)

basematch <- basematch %>% filter(anio<=2009)
basematch$edmedia <- ifelse(basematch$esc>12,1,0)

# Regressions 
regdid <- lm_robust(log(ingcor)~trat*shock,data=basematch,se_type = "HC1")
regmincer <- lm_robust(log(ingcor)~trat*shock + esc + exp + I(exp^2),data=basematch,se_type = "HC1")
regctrl <- lm_robust(log(ingcor)~trat*shock + esc + exp + I(exp^2)+sexo+edmedia,data=basematch,se_type = "HC1")
summary(regdid)
summary(regmincer)
summary(regctrl)

regrows <- c("Intercept","Treatment","Shock","Education","Experience","Experience$^2$","Treatment*Shock","Gender","High School")
texreg(list(regmincer,regctrl),stars = c(0.01,0.05,0.1),file = "tablas_figuras/agric_ctrl_DiD.tex", caption.above = T,
       custom.coef.names = c(regrows),include.ci=F,include.rmse=F,digits=3,caption="Agricultural Sector as Control - DiD estimation",
       custom.model.names = c("ln(Wage)","ln(Wage)"),float.pos = "H")
