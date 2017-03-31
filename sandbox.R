library(data.table)
library(ggplot2)
############
# Roaming  #
############
options(scipen=999)
rm(list=ls())

# Operators source: http://www.123mobielinternet.be/roaming/roamingtarieven-proximus.php
# Big mac index: http://www.economist.com/content/big-mac-index
files <- c('bigmac','proximus','orange','base')
for (i in 1:length(files)) {
    assign(files[i],data.table(read.csv2(paste0(files[i],".csv"))))
}

bigmac <- bigmac[,.(Country,EU,dollar_price)]
dtmac <- merge(bigmac,proximus,by.x='Country',by.y='Land',all.x=T,all.y=F)
dtmac <- merge(dtmac,orange,by.x='Country',by.y='Land',all.x=T,all.y=F)
dtmac <- merge(dtmac,base,by.x='Country',by.y='Land',all.x=T,all.y=F)
rm(proximus,base,orange)

BE <- dtmac[Country=='België']
dtmac <- dtmac[Country != 'België']

colnames(dtmac) <- c('country','EU','dollarprice','proximus','orange','base')
dtmac[,averageprice := (proximus+base+orange)/3]

g <- ggplot(dtmac,aes(x=dollarprice,y=averageprice)) + 
    geom_jitter(aes(col=dtmac$EU)) + 
    geom_smooth(method='lm',se=F) +
    scale_color_discrete(name = "European Union") +
    ylab("Average Price / MB") +
    xlab("Price of a Big Mac in USD")

h <- ggplot(dtmac,aes(x=EU,y=averageprice)) + 
    geom_jitter(aes(col=dtmac$EU)) + 
    geom_smooth(method='lm',se=F) +
    scale_color_discrete(name = "European Union") +
    ylab("Average Price / MB") +
    xlab("Membership of the European Union")

i <- ggplot(dtmac,aes(x=EU,y=dollarprice)) + 
    geom_boxplot() +
    scale_color_discrete(name = "European Union") +
    ylab("Price of a Big Mac in USD") +
    xlab("Membership of the European Union")
