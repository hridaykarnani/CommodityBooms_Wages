
options(scipen=999)

rm(list=ls())
wd <- "C:/Users/Hriday/Documents/R/fentareas/MIC_III/Research"
setwd(wd)

library(haven);library(dplyr);library(ggplot2);library(readxl);library(fixest);

periodo <- rbind(2006,2011)
for (anio in 1:length(periodo)) {
    # anio=5
    base <- read_dta(paste0("data/casen",periodo[anio],".dta"))
    comunas <- read_xlsx(paste0("data/comunas",periodo[anio],".xlsx"))
        base <- mutate(base,comuna=comunas[,1])
    assign(paste0("c",periodo[anio]),base)
    rm(list="base")
}

c2011<-rename(c2011,c("rama"="rama1"))

casen <- list(c2006,c2011)
ramas <- c("06"=2,"11"=3)


# Read Input Output Tables and calculate total Input and Output of each Sector 
iot <- read.csv("data/CHL2009dom.csv")
iot <- iot[1:44,1:45]
rownames(iot) <- iot$X;iot$X<-NULL

# Compatibilize IOT sectors with Casen Sectors
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

# Compatibilize Casen sectors from 2011 with those from year<2011
# rama_comp = rama compatible
c2011$rama <- as.numeric(c2011$rama)
c2011$rama_comp <- NA
for (sector in 1:17) {
    c2011$rama_comp <- ifelse(c2011$rama == sector,
                              iot_casen_sectors$Compatib_98_11[sector],
                              c2011$rama_comp)
}

# Add io and io shares for each individual according to the sector in which they work
c2006$input<-NA;c2006$output<-NA;c2006$inputshare<-NA;c2006$outputshare<-NA;
ios <- seq(0,9)
for (io in 1:length(ios)) {
    c2006$input <- ifelse(c2006$rama == ios[io],iotsum$input[io],c2006$input)
    c2006$output <- ifelse(c2006$rama == ios[io],iotsum$output[io],c2006$output)
    c2006$inputshare <- ifelse(c2006$rama == ios[io],iotsum$inputshare[io],c2006$inputshare)
    c2006$outputshare <- ifelse(c2006$rama == ios[io],iotsum$outputshare[io],c2006$outputshare)
}