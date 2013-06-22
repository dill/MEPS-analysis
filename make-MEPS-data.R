# make a simplified version of the data for the MEPS appendix

# requires the osampuri package with the data in it!
suppressPackageStartupMessages(library(osampuri))
data("uri-lt-data")


## build the observation data

# only loons
obs.loons <- obs[obs$Group=="Loon",]
rm(obs)
# with recorded bins
obs.loons <- obs.loons[obs.loons$Bin!="Not recorded",]
# on the water
obs.loons <- obs.loons[obs.loons$Location=="On Water",]
# in the Winter
obs.loons <- obs.loons[obs.loons$Season=="Winter",]
effort <- effort[effort$Season=="Winter",]

## Build the effort
# left + right = 1
effort$eff.sides <- 0.5*((effort$Onsurvey_Left=="Yes") +
                         (effort$Onsurvey_Right=="Yes"))

seg <- merge(effort,seg,by="Sample.Label")
seg$Effort <- seg$Effort*seg$eff.sides
seg$Transect.Label <- seg$Transect.Label.x
seg$Transect.Label.x <- NULL
seg$Transect.Label.y <- NULL

# Date as sample
seg$Sample.Label <- paste(seg$Sample.Label,seg$Date,sep="-")
obs.loons$Sample.Label <- paste(obs.loons$Sample.Label,
                                obs.loons$Date,sep="-")
seg <- seg[seg$Effort > 0,]

# remove the crap
rm(mpred,samp,tpred)

# add in the extra height and width for plotting
pred$width <- rep(2,nrow(pred))
pred$height <- rep(2, nrow(pred))

# and some ggplot options
p.opts.geo <- theme(panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.background=element_blank(),
                    strip.background=element_blank(),
                    legend.key=element_blank(),
                    aspect.ratio=1
                   )
xlims <- c(-45,40)
ylims <- c(-27,34)


# save the image
save.image("loon-data.RData")

