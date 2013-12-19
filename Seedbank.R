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
TRANDIR="N"
FIRETRT="UB"
RAINTRT="E"
TURBTRT="N"

sub.Census=subset(Census.summary,(Desert==DESERT)&(Year==YEAR)&(FireTrt==FIRETRT)&(TranDir==TRANDIR)&
                      (RainTrt==RAINTRT)&(TurbTrt==TURBTRT))
sub.Seedbank=subset(Seedbank.summary,(Desert==DESERT)&(Year==YEAR)&(TranDir==TRANDIR))
xrange=range(c(sub.Seedbank$InvDens.mean+sub.Seedbank$InvDens.stderr, sub.Seedbank$InvDens.mean-sub.Seedbank$InvDens.stderr))
yrange=range(c(sub.Census$InvDens.mean+sub.Census$InvDens.stderr,sub.Census$InvDens.mean+sub.Census$InvDens.stderr))

plot(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean, xlab="Seedbank Density", ylab="Census Density", xlim=xrange, ylim=yrange)
arrows(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean,  sub.Seedbank$InvDens.mean+sub.Seedbank$InvDens.stderr, sub.Census$InvDens.mean, angle=90, length=0.05)
arrows(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean,  sub.Seedbank$InvDens.mean-sub.Seedbank$InvDens.stderr, sub.Census$InvDens.mean, angle=90, length=0.05)
arrows(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean,  sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean+sub.Census$InvDens.stderr, angle=90, length=0.05)
arrows(sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean,  sub.Seedbank$InvDens.mean, sub.Census$InvDens.mean-sub.Census$InvDens.stderr, angle=90, length=0.05)




#Try fitting mixed effects model to Census data, and for a given trt combo,
# Come up with LSMEANS, for each ones- This will allow for using more data.
# Then compare the seedbank value for these means. 
# Treat seedbank as a covariate. Is covariate (seedbank) significant. 
# 




