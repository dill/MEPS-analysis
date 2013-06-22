
# data setup
load("MEPS-loons-models.RData")
suppressPackageStartupMessages(library(dsm))

# useful stuff
t.size<-18
gg.theme <- theme(axis.text.x=element_text(size=t.size))+
            theme(axis.text.y=element_text(size=t.size))+
            theme(axis.title.x=element_text(size=t.size))+
            theme(axis.title.y=element_text(size=t.size))+
            theme(legend.text=element_text(size=t.size))+
            theme(legend.title=element_text(size=t.size))+
            theme(title=element_text(size=14))+
            theme(aspect.ratio=1)
plot.dir <- "paperplots/"


### plot the detection function
png(paste0(plot.dir,"detfct.png"), 800, 800)
par(ps=18)
plot(hr.df$ddf,pl.den=0,main="")
mtext(side=1,text="                      (m)",line=3)
dev.off()

# plotting functions
plot.covar <- function(covar,title,legend.title){
  p <- ggplot(pred)
  p <- p + p.opts.geo
  p <- p + geom_polygon(aes(x=x,y=y,group=group),
                        colour="black",fill=NA,data=coast)
  p <- p + coord_equal(xlim = xlims, ylim = ylims)
  p <- p + geom_tile(aes_string(x="x",y="y",fill=covar,
                         height="height", width="width"))
  p <- p + scale_fill_gradient(low="white",high="black")
  p <- p + labs(title = title,fill=legend.title,
                y="Kilometres North of centre",x="Kilometres East of centre")
  p <- p + gg.theme
  print(p)
}
plot.preds <- function(pred.data){
  p <- ggplot(pred.data)
  p <- p + p.opts.geo
  p <- p + geom_polygon(aes(x=x,y=y,group=group),
                        colour="black",fill=NA,data=coast)
  p <- p + coord_equal(xlim = xlims, ylim = ylims)
  p <- p + geom_tile(aes_string(x="x",y="y",fill="N",
                         height="height", width="width"))
  p <- p + scale_fill_gradient(low="white",high="black")
  p <- p + labs(y="Kilometres North of centre",x="Kilometres East of centre",
                fill=expression(hat(N)))
  p <- p + gg.theme
  print(p)
}

### depth
png(paste0(plot.dir,"plot-depth.png"), 800, 800)
plot.covar("depth","","Depth (m)")
dev.off()
### distland
png(paste0(plot.dir,"plot-distland.png"), 800, 800)
#plot.covar("distancelandkm","Distance to land (km)","Distance\nto land (km)")
plot.covar("distancelandkm","","Distance\nto land (km)")
dev.off()
### gchlwinter
png(paste0(plot.dir,"plot-gchlwinter.png"), 800, 800)
#plot.covar("gchl_winter","Geometric mean of chlorophyll a winters of 2010-2011 and 2011-2012","Geometric\nmean of\nchlorophyll")
plot.covar("gchl_winter","","Geometric\nmean of\nwinter\nchlorophyll\n2010-2011")
dev.off()
### lgeom
png(paste0(plot.dir,"plot-lgeom.png"), 800, 800)
#plot.covar("gchl_long","Geometric mean of chlorophyll a 2002-2012","Geometric\nmean of\nchlorophyll")
plot.covar("gchl_long","","Geometric\nmean of\nwinter\nchlorophyll\n2002-2012")
dev.off()
### fcpi
png(paste0(plot.dir,"plot-fcpi.png"), 800, 800)
#plot.covar("fcpi","FCPI","FCPI")
plot.covar("fcpi","","FCPI")
dev.off()
### phimedian
png(paste0(plot.dir,"plot-phimedian.png"), 800, 800)
plot.covar("phimedian","","Median\nsediment size")
dev.off()
### roughness
png(paste0(plot.dir,"plot-roughness.png"), 800, 800)
plot.covar("roughness","","Bottom\nroughness")
dev.off()


### raw observations
png(paste0(plot.dir,"plot-rawobs.png"), 800, 800)
p <- ggplot(obs.loons)
p <- p + geom_point(aes(x=x,y=y,size=size),alpha=0.4)
p <- p + p.opts.geo + theme(legend.key=element_blank())
p <- p + geom_polygon(aes(x=x,y=y,group=group),
                      colour="black",fill=NA,data=coast)
p <- p + coord_equal(xlim = xlims, ylim = ylims)
p <- p + labs(title = "",size="Observed\ngroup size")
p <- p + labs(y="Kilometres North of centre",x="Kilometres East of centre")
p <- p + gg.theme
print(p)
dev.off()


### plot of smooths of gchl and y
png(paste0(plot.dir,"smooths.png"), 1200, 400)
par(mfrow=c(1,3),ps=18)
plot(loon.model.hr,residuals=TRUE,select=1,scale=-1,
     shade=TRUE,pch=19,cex=0.05,
     xlab="Geometric mean of chlorophyll a 2002-2012",ylab="Linear predictor")
plot(loon.model.hr,residuals=TRUE,select=2,scale=-1,
     shade=TRUE,pch=19,cex=0.05,
     xlab="Depth (m)",ylab="Linear predictor")
plot(loon.model.hr,residuals=TRUE,select=3,scale=-1,
     shade=TRUE,pch=19,cex=0.05,xlab="Kilometers north of center",
     ylab="Linear predictor")
dev.off()


print(summary(dsm.var.prop(loon.model.hr, pred, pred$cellarea)))

### CV plotting
lgeom.var <- dsm.var.prop(loon.model.hr, split(pred,1:nrow(pred)), pred$cellarea)
gg.obj <- plot(lgeom.var,poly=coast,plot=FALSE,observations=FALSE,
               gg.grad=scale_fill_gradient(low="white",high="black",
                               trans="log",breaks=c(0.3,1,2.7)))
gg.obj <- gg.obj + coord_equal(xlim = xlims,ylim = ylims)
gg.obj <- gg.obj + gg.theme
gg.obj <- gg.obj + labs(y="Kilometres North of centre",
                        x="Kilometres East of centre")
#gg.obj <- gg.obj + geom_point(aes(x=x,y=y),data=obs.loons)
png(paste0(plot.dir,"cvplot.png"), 800, 800)
print(gg.obj)
dev.off()

## @knitr lgeom-preds
loon.model.predict <- predict(loon.model.hr, pred, pred$cellaream)
loon.model.predict <- cbind(pred, N=loon.model.predict)
png(paste0(plot.dir,"preds.png"),800,800)
par(ps=18)
plot.preds(loon.model.predict)
dev.off()

## correlogram
png(paste0(plot.dir,"corel.png"),800,800)
par(ps=18)
dsm.cor(loon.model.hr,max.lag=30)
dev.off()
