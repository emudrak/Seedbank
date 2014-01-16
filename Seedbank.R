##  Establish connection between seedbank and veg data in high rainfall conditions

## Idea:  summarize seedbank data (compare push 0 to push 1-4?)
## Then show that it is the same pattern (??) as annual density for watered shrubs. 

## Read in and munge data
setwd("~/SERDPproject/Seedbank/Seedbank")
AllData=read.csv("Seedbank.csv", header=T)
Seedbank=AllData[,1:16]

#What are the Summary Statistics? ------
# SpeRichDesert 
# Seedbank
# SpeRichAll    
# TotDensDesert 
# TotDensAll    
# NatDens       
# InvDens       
# InvDenPct  
#----------
CensusData=read.csv("Census_Winter.csv", header=T)
Census=CensusData[,1:32]

#What are the Summary Statistics? ------

# "TotCov" 
# "VegHeight" 
# "EstBiomass" 
# "NatCover"  
# "InvCover"  
# "EroCover" 
# "SchCover"  
# "BroCover"  
# "BraCover" 
# "Crust"   
# "CryCrust" 
# "SpecRich"  
# "TotDens" 
# "NatDens" 
# "InvDens"  
# "InvDensPct" 


# Seedbank Plots -----------

dircols=c("blue", "red")

DESERT="Mojave"   #Choice "Mojave" "Sonoran"
YEAR=2011           #Choice 2010, 2011, 2012
TARGET="NatDens"    #Choices TotDensDesert NatDens InvDens
                    #Other Choices SpeRichDesert SpeRichAll  TotDensAll InvDenPct

ylabel="Number of Seeds"

thisSeedbank=subset(Seedbank, (Desert==DESERT)& (Year==YEAR)& (GermPush==1234))
windows(7,3.5)
with (thisSeedbank, plot(MHcode, thisSeedbank[,TARGET] , pch=NA, main=paste(DESERT, YEAR, TARGET), ylab=ylabel, xlab="Habitat Code" ))
yplotmax=max(thisSeedbank[,TARGET])
for (i in unique(thisSeedbank$"ShrubID")) {  # i=ShrubNumber  
        for (j in unique(thisSeedbank$TranDir))   {#j=Direction
            mydir=thisSeedbank$TranDir[(thisSeedbank$ShrubID==i)&(thisSeedbank$TranDir==j)][1]
    with (thisSeedbank,
          lines(thisSeedbank$MHcode[(thisSeedbank$ShrubID==i)&(thisSeedbank$TranDir==j)],
          thisSeedbank[(thisSeedbank$ShrubID==i)&(thisSeedbank$TranDir==j),TARGET], 
          pch=20,lwd=2, type="o", col=dircols[mydir]
          )
    )
        }#end of j loop
} # end of i loop
legend(1,0.95*yplotmax, levels(thisSeedbank$TranDir), col=dircols, lwd=2, lty=1, bty="n")      #IF a 2012 Census.Train



#Plot things -----------

dircols=c("blue", "red")

DESERT="Mojave"   #Choice "Mojave" "Sonoran"
YEAR=2012           #Choice 2010, 2011, 2012
TARGET="InvDens"    #Choices TotDens NatDens InvDens


ylabel="Number of Plants"


thisCensus=subset(Census, (Desert==DESERT)& (Year==YEAR)& (RainTrt=="E")& (TurbTrt=="N")&(FireTrt=="UB")&(SeedTrt=="A"))
windows(7,3.5)
with (thisCensus, plot(MHcode, thisCensus[,TARGET] , pch=NA, main=paste(DESERT, YEAR, TARGET), ylab=ylabel, xlab="Habitat Code" ))
yplotmax=max(thisCensus[,TARGET])
for (i in unique(thisCensus$"ShrubID")) {  # i=ShrubNumber  
    for (j in unique(thisCensus$TranDir))   {#j=Direction
        mydir=thisCensus$TranDir[(thisCensus$ShrubID==i)&(thisCensus$TranDir==j)][1]
        with (thisCensus,
              lines(thisCensus$MHcode[(thisCensus$ShrubID==i)&(thisCensus$TranDir==j)],
                    thisCensus[(thisCensus$ShrubID==i)&(thisCensus$TranDir==j),TARGET], 
                    pch=20,lwd=2, type="o", col=dircols[mydir]
              )
        )
        print(i)
    }#end of j loop
} # end of i loop
legend(1,0.95*yplotmax, levels(thisCensus$TranDir), col=dircols, lwd=2, lty=1, bty="n")      #IF a 2012 Census.Train


# Seedbank Summaries --------------

library(doBy)
stderr <- function(x) sd(x)/sqrt(length(x))

Seedbank.summary=summaryBy(InvDens+NatDens+TotDensDesert~Desert + Year + MHcode + TranDir, data=Seedbank[Seedbank$GermPush==1234,], FUN=c(mean,stderr, length))

# Census Summaries--------------

#Summarize Mean and StdErr for each Habitat Group in these subsets of Census Data: 
# Unburned, Ambient seeds, Elevated Water, Not Turbated
# Unburned, Ambient seeds, Elevated Water, Turbated         #If turbating releases seedbank
# Burned, Ambient seeds, Elevated Water, Not Turbated       #If burning releases seedbank
# Burned, Ambient seeds, Elevated Water, Turbated           #Both releasing seedbank
# Unburned, Ambient seeds, Ambient Water, Not Turbated
# Unburned, Ambient seeds, Ambient Water, Turbated         #If turbating releases seedbank
# Burned, Ambient seeds, Ambient Water, Not Turbated       #If burning releases seedbank
# Burned, Ambient seeds, Ambient Water, Turbated           #Both releasing seedbank

# Compare 2011 Census to 2010 seeds, 2011 seeds, 
# Compare 2012 Census to 2010, 2011, 2012 seeds

#Show plots for all Desert/Yr combos

#compare to push 0/

Census.summ=summaryBy(InvDens+NatDens+TotDens~Desert + Year + MHcode + TranDir+FireTrt+RainTrt+TurbTrt+SeedTrt, data=Census, FUN=c(mean,stderr, length))
Census.summary=subset(Census.summ, (RainTrt!="D")&(SeedTrt!="S") )


#Compare means --------------
#Plot means and StdErrs from plots of same types and years

DESERT=="Sonoran"
YEAR="2012"
TRANDIR="S"
dircol="red"
FIRETRT="UB"
RAINTRT="E"

TURBTRT="N"

sub.Census=subset(Census.summary,(Desert==DESERT)&(Year==YEAR)&(FireTrt==FIRETRT)&(TranDir==TRANDIR)&
                      (RainTrt==RAINTRT)&(TurbTrt==TURBTRT))
sub.Seedbank=subset(Seedbank.summary,(Desert==DESERT)&(Year==YEAR)&(TranDir==TRANDIR))
xrange=range(c(sub.Seedbank$InvDens.mean+sub.Seedbank$InvDens.stderr, sub.Seedbank$InvDens.mean-sub.Seedbank$InvDens.stderr))
yrange=range(c(sub.Census$InvDens.mean+sub.Census$InvDens.stderr,sub.Census$InvDens.mean+sub.Census$InvDens.stderr))

plot(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean, xlab="Seedbank Density", ylab="Census Density", xlim=xrange, ylim=yrange, col=dircol)
arrows(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean,  sub.Seedbank$InvDens.mean+sub.Seedbank$InvDens.stderr, sub.Census$InvDens.mean, angle=90, length=0.05, col=dircol)
arrows(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean,  sub.Seedbank$InvDens.mean-sub.Seedbank$InvDens.stderr, sub.Census$InvDens.mean, angle=90, length=0.05, col=dircol)
arrows(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean,  sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean+sub.Census$InvDens.stderr, angle=90, length=0.05, col=dircol)
arrows(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean,  sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean-sub.Census$InvDens.stderr, angle=90, length=0.05, col=dircol)


#Try plotting all on same graph---------
#Invasive Density-------
#Do north and South separately
DESERT="Sonoran"
SeedYEAR="2010"
CensusYEAR="2012"
TRANDIR="S"

sub.Census=subset(Census.summary,(Desert==DESERT)&(Year==CensusYEAR)&(TranDir==TRANDIR))
sub.Seedbank=subset(Seedbank.summary,(Desert==DESERT)&(Year==SeedYEAR)&(TranDir==TRANDIR))
xrange=range(c(sub.Seedbank$InvDens.mean+sub.Seedbank$InvDens.stderr, sub.Seedbank$InvDens.mean-sub.Seedbank$InvDens.stderr))
yrange=range(c(sub.Census$InvDens.mean+sub.Census$InvDens.stderr,sub.Census$InvDens.mean+sub.Census$InvDens.stderr))
windows(6,6)
plot(0,0, xlab="Seed Bank Density", ylab="Census Density", xlim=xrange, ylim=yrange, pch=NA, 
     main=c(paste(DESERT, " ", SeedYEAR, " Seedbank     ", CensusYEAR , " Census "), paste( TRANDIR, " InvDens") ))

#expand.grid(Fire=c("B","UB"), Rain=c("A", "E"), Turb=c("Y","N"))

FireTrtmnts=c("B", "UB"); RainTrtmnts=c("A", "E"); TurbTrtmnts=c("Y","N")
for(FIRETRT in FireTrtmnts){
    for(RAINTRT in RainTrtmnts){
        for(TURBTRT in TurbTrtmnts){
            sub.Cen=sub.Seed=NULL
            if (FIRETRT=="B") thispch=16 else thispch=17
            if (RAINTRT=="A") thiscol="brown" else thiscol="blue"
            if (TURBTRT=="Y") thislty=1 else thislty=3
            sub.Cen=subset(sub.Census,(FireTrt==FIRETRT)&(RainTrt==RAINTRT)&(TurbTrt==TURBTRT))
            print(sub.Cen)
            sub.Seed=sub.Seedbank
            try(points(sub.Seed$InvDens.mean, sub.Cen$InvDens.mean, col=thiscol, pch=thispch))
            try(arrows(sub.Seed$InvDens.mean, sub.Cen$InvDens.mean,  sub.Seed$InvDens.mean+sub.Seed$InvDens.stderr, sub.Cen$InvDens.mean, angle=90, length=0.05, col=thiscol, pch=thispch, lty=thislty))
            try(arrows(sub.Seed$InvDens.mean, sub.Cen$InvDens.mean,  sub.Seed$InvDens.mean-sub.Seed$InvDens.stderr, sub.Cen$InvDens.mean, angle=90, length=0.05, col=thiscol, pch=thispch, lty=thislty))
            try(arrows(sub.Seed$InvDens.mean, sub.Cen$InvDens.mean,  sub.Seed$InvDens.mean, sub.Cen$InvDens.mean+sub.Cen$InvDens.stderr, angle=90, length=0.05, col=thiscol, pch=thispch, lty=thislty))
            try(arrows(sub.Seed$InvDens.mean, sub.Cen$InvDens.mean,  sub.Seed$InvDens.mean, sub.Cen$InvDens.mean-sub.Cen$InvDens.stderr, angle=90, length=0.05, col=thiscol, pch=thispch, lty=thislty))
            #text(sub.Seed$InvDens.mean, sub.Cen$InvDens.mean, c(1,2,3,4), pos=2)
        }
    }
}

#Native Density-------
#Do North and South separately
DESERT="Sonoran"
SeedYEAR="2012"
CensusYEAR="2012"
TRANDIR="S"

sub.Census=subset(Census.summary,(Desert==DESERT)&(Year==CensusYEAR)&(TranDir==TRANDIR))
sub.Seedbank=subset(Seedbank.summary,(Desert==DESERT)&(Year==SeedYEAR)&(TranDir==TRANDIR))
xrange=range(c(sub.Seedbank$NatDens.mean+sub.Seedbank$NatDens.stderr, sub.Seedbank$NatDens.mean-sub.Seedbank$NatDens.stderr))
yrange=range(c(sub.Census$NatDens.mean+sub.Census$NatDens.stderr,sub.Census$NatDens.mean+sub.Census$NatDens.stderr))
windows(6,6)
plot(0,0, xlab="Seed Bank Density", ylab="Census Density", xlim=xrange, ylim=yrange, pch=NA, 
     main=c(paste(DESERT, " ", SeedYEAR, " Seedbank     ", CensusYEAR , " Census "), paste( TRANDIR, " NatDens") ))

#expand.grid(Fire=c("B","UB"), Rain=c("A", "E"), Turb=c("Y","N"))

FireTrtmnts=c("B", "UB"); RainTrtmnts=c("A", "E"); TurbTrtmnts=c("Y","N")
for(FIRETRT in FireTrtmnts){
    for(RAINTRT in RainTrtmnts){
        for(TURBTRT in TurbTrtmnts){
            sub.Cen=sub.Seed=NULL
            if (FIRETRT=="B") thispch=16 else thispch=17
            if (RAINTRT=="A") thiscol="brown" else thiscol="blue"
            if (TURBTRT=="Y") thislty=1 else thislty=3
            sub.Cen=subset(sub.Census,(FireTrt==FIRETRT)&(RainTrt==RAINTRT)&(TurbTrt==TURBTRT))
            print(sub.Cen)
            sub.Seed=sub.Seedbank
            try(points(sub.Seed$NatDens.mean, sub.Cen$NatDens.mean, col=thiscol, pch=thispch))
            try(arrows(sub.Seed$NatDens.mean, sub.Cen$NatDens.mean,  sub.Seed$NatDens.mean+sub.Seed$NatDens.stderr, sub.Cen$NatDens.mean, angle=90, length=0.05, col=thiscol, pch=thispch, lty=thislty))
            try(arrows(sub.Seed$NatDens.mean, sub.Cen$NatDens.mean,  sub.Seed$NatDens.mean-sub.Seed$NatDens.stderr, sub.Cen$NatDens.mean, angle=90, length=0.05, col=thiscol, pch=thispch, lty=thislty))
            try(arrows(sub.Seed$NatDens.mean, sub.Cen$NatDens.mean,  sub.Seed$NatDens.mean, sub.Cen$NatDens.mean+sub.Cen$NatDens.stderr, angle=90, length=0.05, col=thiscol, pch=thispch, lty=thislty))
            try(arrows(sub.Seed$NatDens.mean, sub.Cen$NatDens.mean,  sub.Seed$NatDens.mean, sub.Cen$NatDens.mean-sub.Cen$NatDens.stderr, angle=90, length=0.05, col=thiscol, pch=thispch, lty=thislty))
            #text(sub.Seed$NatDens.mean, sub.Cen$NatDens.mean, c(1,2,3,4), pos=2)
        }
    }
}



library(lme4)
#----------------
#Try fitting mixed effects model to Census data (and Seedbank Data?), and for a given trt combo,
# Come up with LSMEANS, for each ones- This will allow for using more data.
# Then compare the seedbank value for these means. 
# Treat seedbank as a covariate. Is covariate (seedbank) significant?  Can't do this b/c don't have corresponding data to put into lines
# 

plot( NatDens ~ MH*Desert*Year, data= Seedbank[Seedbank$GermPush==1234,])
plot( InvDens ~ MH*Desert*Year, data= Seedbank[Seedbank$GermPush==1234,])
plot( TotDensAll ~ MH*Desert*Year, data= Seedbank[Seedbank$GermPush==1234,])

Seedbank$Year=as.factor(Seedbank$Year)

Seedbank.glme=glmer( NatDens~Year+MH+Desert+TranDir+ 
                    Year:MH + Year:Desert + Year:TranDir + # MH:Desert + Desert:TranDir+  MH:TranDir +
                        #Year:Desert:TranDir + Year:MH:TranDir + Year:MH:Desert+ MH:Desert:TranDir + 
                    (1|ShrubID) , family=poisson, data=Seedbank[Seedbank$GermPush==1234,])

summary(Seedbank.glme)

library(lsmeans)

Seedbank.means=lsmeans(Seedbank.glme,  ~  Year:MH:Desert )
lsmeans(Seedbank.glme, pairwise~  Year:Desert )
lsmeans(Seedbank.glme, pairwise~   Year:TranDir)

logit=function(x){log(x/(1-x))}
invlogit=function(a){exp(a)/(1+exp(a))}

invlogit(means[[1]][,c(4,7,8)])

# #Francoise's idea:  stack dataframes of Seedbank and Census ----------------
# Shrub|MH|Dir|PlantCt|Type(Census or Seedbank)
# model count= MH*Dir*Type
# random shrub(type)
# get lsmeas for (MH*Dir*Type)

# Prep Data --------------------
#Use Seedbank and Census (not summarized), subset by Mojave, North, 2010 Seedbank, 2011 Census or 2011 Seedbank and 2011 Census
# Response: InvDensity or Total Density
SEEDYEAR=2010    #2010 or 2011
CENYEAR=2011    #Stick with 2011 
Sub.Seed=subset(Seedbank, (Desert=="Mojave")&(Year==SEEDYEAR)&(TranDir=="N")&((GermPush=="1234")|(GermPush=="0")),select=c("ShrubID", "MH", "MHcode", "TotDensAll","NatDens", "InvDens", "GermPush" ))
library(doBy)  #Must combine data from Push 0 (germinated in the field) and from Push1234 (total germinated in greenhouse)
Sub.Seedbank=summaryBy( TotDensAll + NatDens + InvDens ~ ShrubID + MH + MHcode, data=Sub.Seed, FUN=sum)
Sub.Seedbank$Type="Seedbank" 
Sub.Census=subset(Census, (Desert=="Mojave")&(Year==2011)&(TranDir=="N"),select=c("ShrubID", "MH", "MHcode", "TotDens","NatDens", "InvDens"))
Sub.Census$Type="Census"
names(Sub.Seedbank)=names(Sub.Census)
#Stack Census and Seedbank Data
CombSub=rbind(Sub.Seedbank, Sub.Census)
CombSub$Type=factor(CombSub$Type)

# Make Models ----------------
library(lme4)
TARGET=CombSub$InvDens
TARGNAME="Invasive Density"
Seedbank.glme=glmer( TARGET~MH*Type+(1|ShrubID) , family=poisson, data=CombSub)
summary(Seedbank.glme)

library(lsmeans)
Seedbank.means=lsmeans(Seedbank.glme,  ~  MH:Type )$`MH:Type lsmeans`
#back transform becasue of poisson family log-link
Seedbank.means$exp.lsmean=exp(Seedbank.means$lsmean)
Seedbank.means$exp.LCL=exp(Seedbank.means$asymp.LCL)
Seedbank.means$exp.UCL=exp(Seedbank.means$asymp.UCL)

# Plot Figure --------------
xrange=range(c(Seedbank.means[Seedbank.means$Type=="Seedbank",c("exp.UCL","exp.LCL")]))
yrange=range(c(Seedbank.means[Seedbank.means$Type=="Census",c("exp.UCL","exp.LCL")]))
windows(5,5)
plot(0,0, xlab=paste("Seed Bank Density ", SEEDYEAR), ylab=paste("Census Density ", CENYEAR), xlim=xrange, ylim=yrange, pch=NA)
points(Seedbank.means[Seedbank.means$Type=="Seedbank","exp.lsmean"],  Seedbank.means[Seedbank.means$Type=="Census","exp.lsmean"], pch=19)
arrows(Seedbank.means[Seedbank.means$Type=="Seedbank","exp.lsmean"], Seedbank.means[Seedbank.means$Type=="Census","exp.lsmean"],  
       Seedbank.means[Seedbank.means$Type=="Seedbank","exp.UCL"], Seedbank.means[Seedbank.means$Type=="Census","exp.lsmean"], angle=90, length=0.05)
arrows(Seedbank.means[Seedbank.means$Type=="Seedbank","exp.lsmean"], Seedbank.means[Seedbank.means$Type=="Census","exp.lsmean"],  
       Seedbank.means[Seedbank.means$Type=="Seedbank","exp.LCL"], Seedbank.means[Seedbank.means$Type=="Census","exp.lsmean"], angle=90, length=0.05)

arrows(Seedbank.means[Seedbank.means$Type=="Seedbank","exp.lsmean"], Seedbank.means[Seedbank.means$Type=="Census","exp.lsmean"],  
       Seedbank.means[Seedbank.means$Type=="Seedbank","exp.lsmean"], Seedbank.means[Seedbank.means$Type=="Census","exp.UCL"], angle=90, length=0.05)
arrows(Seedbank.means[Seedbank.means$Type=="Seedbank","exp.lsmean"], Seedbank.means[Seedbank.means$Type=="Census","exp.lsmean"],  
       Seedbank.means[Seedbank.means$Type=="Seedbank","exp.lsmean"], Seedbank.means[Seedbank.means$Type=="Census","exp.LCL"], angle=90, length=0.05)
abline(0,1, lty=2)
adjvals=c(c(0,0), c(0,1),c(0,1),c(1,0))
text(locator(4),   #Click four times where you want the labels to go
     labels=Seedbank.means[Seedbank.means$Type=="Census","MH"],  cex=0.8)
title(paste("Mojave North "))



