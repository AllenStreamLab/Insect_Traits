##################
#Create plots 
#################

library(ggplot2)
library(rethinking)
library(randomForest)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


project.directory <- "C:/Users/AllenLabWorkstation/Dropbox/Dissertation/Chapter_3_Distance_Deposition/Manuscript/Ecography_Submission/Project_data"
setwd(project.directory)

#directory to write files 
fig.dir <- "C:/Users/AllenLabWorkstation/Dropbox/Dissertation/Chapter_3_Distance_Deposition/Manuscript/Figures"

#shapefile for marginal effects - avalabe form NHDPlusV2
#vpushp <- read_sf("C:/Users/AllenLabWorkstation/Dropbox/Dissertation/StreamResiliencyRCN/VPU_NAD83.shp")


###########################################################
# Random Forest Plots
###########################################################

optifiles <- c("rf_eptdo_optimize", " rf_SS_25_optimize", "empty",              "empty",
               "rf_ESP_p_optimize",  "rf_VOM_p_optimize", "rf_DSF_p_optimize",  "empty",
               "rf_BSLM_p_optimize", "rf_FDH_p_optimize", "rf_LSLS_p_optimize", "rf_FSS_p_optimize")  

plotnames <- c("FLYING", "SS25", "EMPTY", "EMPTY",
               "ESP",    "VOM",  "DSF",   "EMPTY",
               "BSL",    "FDH",  "LSL",   "FSS")

#mtry
#####
# read in optimized models 
windowsFonts(A = windowsFont("Times New Roman"))
windows(7, 7)
par(mfcol = c(4, 3))

namcnt <- 1
for (i in optifiles){
  if(i == "empty"){
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n")
  } else {
    #i <- "rf_eptdo_optimize"
    load(i)
    par(mar = c(2, 2, 1.5, 0.5))
    par(bty = "n")
    count <- 1
    
    for (j in unique(rf_model_list$optimize_mtry$mtry)){
      if(count == 1){
        plot(0, 0, type="n", col="black", xlab="ntree", ylab="PVE", 
             xlim = c(min(rf_model_list$optimize_mtry$ntree), 
                      max(rf_model_list$optimize_mtry$ntree)),
             ylim = c(min(rf_model_list$optimize_mtry$PVE), 
                      max(rf_model_list$optimize_mtry$PVE)), 
             main = plotnames[namcnt], 
             family = "A")
        
        lines(rf_model_list$optimize_mtry$ntree[rf_model_list$optimize_mtry$mtry == j],
              rf_model_list$optimize_mtry$PVE[rf_model_list$optimize_mtry$mtry == j], 
              col=count, lwd = 2)
      } else {
        
        lines(rf_model_list$optimize_mtry$ntree[rf_model_list$optimize_mtry$mtry == j],
              rf_model_list$optimize_mtry$PVE[rf_model_list$optimize_mtry$mtry == j], 
              col=count, lwd = 2)
      }
      count <- count + 1
    }
  }
  namcnt <- namcnt + 1
}

# create legend legend
windows(4, 4)
par(mar = c(2, 4, 1.5, 0.5))
par(bty = "n")
par(family = "A")
plot(0, 0)
legend("bottom", horiz = F, 
       legend = paste("p=",as.character(unique(rf_model_list$optimize_mtry$mtry))), 
       lty = 1, col = 1:length(unique(rf_model_list$optimize_mtry$mtry)), lwd = 3)
#savePlot(filename = paste0(fig.dir, "/mtry_legend.jpg"), type = "jpg")
#dev.off(which = dev.cur())  
################
#savePlot(filename = paste0(fig.dir, "/all_mtry.jpg"), type = "jpg")
#dev.off(which = dev.cur()) 

#ntry
#####
windows(7, 7)
par(mfcol = c(4, 3))
windowsFonts(A = windowsFont("Times New Roman"))

namcnt <- 1
for (i in optifiles){
  if(i == "empty"){
    plot(1, 1, type = "n",xaxt = "n", yaxt = "n")
  } else {
    par(mar = c(2, 4, 3, 0.5))
    par(bty = "n")
    load(i)
    plot(rf_model_list[["optimize_ntree"]]$ntree,
         rf_model_list[["optimize_ntree"]]$corr, 
         xlab = "", 
         ylab = "", 
         main = plotnames[namcnt], 
         ylim = c(min(rf_model_list[["optimize_ntree"]]$corr), 1), 
         family = "A")
    points(max(rf_model_list[["optimize_ntree"]]$ntree),
           max(rf_model_list[["optimize_ntree"]]$corr), 
           pch = 19, cex = 2)
  }
  namcnt <- namcnt + 1
}
################
#savePlot(filename = paste0(fig.dir, "/all_ntree.jpg"), type = "jpg")
#dev.off(which = dev.cur()) 

#Importance plots 
#####
windows(7, 10)
par(mfcol = c(4, 3))
par(oma = c(1.2, 1, 1.5, 1)) 
windowsFonts(A = windowsFont("Times New Roman"))

namcnt <- 1
for(q in optifiles){
  if(q == "empty"){
    plot(1, 1, type = "n",xaxt = "n", yaxt = "n")
  } else {
    par(mar = c(2, 4, 3, 0.5))
    par(bty = "n")
    
    #q <- "rf_eptdo_optimize"
    load(q)
    colvec <- viridis(6, begin = 0, end = 0.9) 
    
    #extract importance
    imp <- importance(rf_model_list[["randomforest"]], scale = T, type = 1)
    impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]
    rownames(imp) <- c("EMBED", "AREA", "SLOPE", 
                       "URBAN", "AGRI", "FINES", 
                       "ELEV", "REGION", "PRECIP", "PRECIP_CoV", 
                       "TEMP_MA", "TEMP_CoV", "HYDROCLASS")
    
    vpuloc <- which(names(sort(imp[, 1], decreasing = T)) == "REGION")

    par(mai = c(0.5, 0.5, 0, 0))#, font.axis = 1, xaxt = "s")
    par(bty="n")
    dotchart(sort(imp[, 1]), 
             color = "black", 
             bg = "black", 
             pt.cex = 0.7,  
             xlim = c(min(imp[, 1]) - 5, max(imp[, 1]) + 5), 
             cex = 0.6, 
             xaxt = "n",
             lcolor = "white",
             xlab = "", 
             bty="n",
             family = "A")
    mtext(plotnames[namcnt], family = "A")
  }
  namcnt<-namcnt+1
}
################
#savePlot(filename = paste0(fig.dir,"/all_varimp.jpg"), type = c("jpg"), device=dev.cur())
#dev.off(which = dev.cur())

#partial dependence plots
#####

# identify most important variables for each model. 
# output table used of ordering Figure 1
out <- data.frame()
for(q in optifiles){
  #q <- "rf_eptdo_optimize"
  load(q)
  
  #extract importance
  imp <- randomForest::importance(rf_model_list[["randomforest"]], scale = T, type = 1)
  impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]
  rownames(imp) <- c("EMBED", "AREA", "SLOPE", 
                     "URBAN", "AGRI", "FINES", 
                     "ELEV", "REGION", "PRECIP", 
                     "PRECIP_CoV", "TEMP_MA", 
                     "TEMP_CoV", "HYRDOCLASS")
  
  for (i in grep("vpu", impvar, invert = T)[5:1]) {
    a <- partialPlot(rf_model_list[["randomforest"]], 
                     pred.data = rf_model_list[["traindat"]], 
                     x.var = impvar[i], 
                     plot = F)  
    
    resp <- unlist(lapply(strsplit(q, "_"),"[[", 2))
    resp <- ifelse(resp != "SS" & resp != "eptdo", 
                   paste0(resp,"_p"), 
                   ifelse(resp=="SS", paste0(resp,"_25"), resp))
    plot_threshold <- (max(a$y)-min(a$y))/(max(rf_model_list$traindat[,resp]) - min(rf_model_list$traindat[,resp]))
    
    temp <- data.frame(q, resp, var = impvar[i], plot_threshold, rank = i,
                       labels = names(sort(imp[,1]))[length(sort(imp[,1])):(length(sort(imp[,1]))-5)][i])
    out <- rbind(out, temp)
  }
}

out$plot_threshold<-round(out$plot_threshold,2)
out <- split(out, out$q)

# plot each q 
# can save each plot inside for loop 
for(q in out){
  #q <- out$rf_BSLM_p_optimize
  
  load(as.character(q[1, 1]))
  #initialize plot window
  windows(2.5, 3)
  windowsFonts(A = windowsFont("Times New Roman"))
  par(oma = c(2, 0.5, 4, 0.2))
  par(mfrow = c(ifelse(nrow(q) < 3, 3, nrow(q)), 1))
  par(font.axis = 2, xaxt = "s" )
  
  q <- q[order(q$rank),]
  vars <- as.character(q$var)
  draw.axis <- max(q[q$var != "exp", "rank"])
  
  #offset to fit labels in plot area
  offest <- ifelse(q[1,1] == "rf_SS_25_optimize", 0.4, 0.025)
  if(length(vars[vars != "exp"]) > 1){
    min.val <- min(apply(rf_model_list[["traindat"]][, vars[vars != "exp"]], 2, min))
    max.val <- max(apply(rf_model_list[["traindat"]][, vars[vars != "exp"]], 2, max))
  } else {
    min.val <- min(rf_model_list[["traindat"]][, vars[vars != "exp"]])
    max.val <- max(rf_model_list[["traindat"]][, vars[vars != "exp"]])
  }
  
  for (i in 1:nrow(q)){
    
    par(mar = c (1, 2.8, 0.5, 0.5))
    
    #add more room at the bottom
    if(q[i, "rank"] == draw.axis & q[i, "rank"] != max(q[i, "rank"])){
      par(mar = c (2, 2.8, 0.5, 0.5))
    }
    
    if (vars[i] != "exp"){
      a <- partialPlot(rf_model_list[["randomforest"]], 
                       pred.data = rf_model_list[["traindat"]], 
                       x.var = vars[i], 
                       plot = F)  
      
      plot(a$x, a$y, type = "l", 
           xlim = c(min.val, max.val), ylim = c(min(a$y) - offest, max(a$y) + offest),
           cex.lab = 1.3, cex.axis = 1.2, lwd = (7 - q[i, "rank"]), 
           main = as.character(q[i, "labels"]),cex.main = 0.9, 
           col = "black",
           las = 1, xlab = "Predictor Value (Mean-Centered & Scaled)", axes = F, family="A")
      
      #axis twice to labe min and max only 
      axis(2, at = c(min(a$y) - offest, (max(a$y) + min(a$y))/2, max(a$y) + offest), labels = FALSE, las = 1)
      axis(2, at = c(min(a$y) - offest, max(a$y) + offest), 
           labels = round(c(min(a$y) - offest, max(a$y) + offest), 2), las = 1)
    }
    
    else if(vars[i] == "exp"){
      
      a <- partialPlot(rf_model_list[["randomforest"]], 
                       pred.data = rf_model_list[["traindat"]], 
                       x.var = vars[i], plot = F)
      a <- data.frame(a)
      a <- a[order(a$y), ]
      
      #equal intervals along continuous variable range 
      a <- data.frame(a, seq = seq(min.val, max.val, by = abs(min.val-max.val)/length(a$x))[-1])
      
      plot(a$y ~ a$seq, ylab = "", ylim = c(min(a$y) - offest, max(a$y) + offest),
           type = "n", col = "white", axes = F, xlab = "", 
           main = as.character(q[i, "labels"]), 
           cex.main = 0.9, family = "A")
      
      text(a$seq, a$y, labels = a$x, las = 1, col = "black", family = "A")
      
      #axis twice to label min and max only 
      axis(2, at = c(min(a$y) - offest, (max(a$y) + min(a$y))/2, max(a$y) + offest), 
           labels = FALSE, las = 1, family = "A")
      axis(2, at = c(min(a$y) - offest, max(a$y) + offest), 
           labels = round(c(min(a$y), max(a$y) + offest), 2), 
           las = 1, family = "A")
    }
    
    #draw axis
    if(q[i, "rank"] == draw.axis){
      axis(1, family = "A")
    }
  }
  
  mtext(unique(q$q), 3, outer =  T, cex= 1.3, line = 1, family = "A")
  
  #savePlot(filename = paste0(fig.dir, "/MarEff_",q$q[1],"_all.jpg"),
  #         type = c("jpg"),
  #         device=dev.cur())
  #dev.off(which = dev.cur())
}    

#rank legend
windows()   
par(family = "A")
plot(0,0, type = "n")
legend("center", legend = 1:5, lwd = 5:1)
#savePlot(filename = paste0(fig.dir, "/MarEff_legend.jpg"),
#         type = c("jpg"),
#         device=dev.cur()) 
#################

#Marginal effets of hydrologic region
#write shapefiles, Maps produced in ArcGIS
#####

for (q in optifiles){
  #q <- "rf_eptdo_optimize"
  load(q)
  #extract importance
  imp <- importance(rf_model_list[["randomforest"]], scale = T, type = 1)
  impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]
  rownames(imp) <- c("EMBED", "AREA", "SLOPE", 
                     "URBAN", "AGRI", "FINES", 
                     "ELEV", "REGION", "PRECIP", "PRECIP_CoV", 
                     "TEMP_MA", "TEMP_CoV", "HYROCLASS")
  
  a <- partialPlot(rf_model_list[["randomforest"]], 
                   pred.data = rf_model_list[["traindat"]], 
                   x.var = "vpu", rug = F, plot = F)  
  a <- data.frame(VPUID = a$x, mar_eff = a$y, rank = which(impvar == "vpu"))
  a <- merge(vpushp, a, by = "VPUID")

  write_sf(a, paste0(fig.dir,"/mar_eff_", q,".shp"))
}
################


###########################################################
# Intercept Only Models
# Posterior Distributions
# trait density models
###########################################################

# eptdo
#####

load(paste0("DisturbanceIntercepts_", "eptdo"))
s <- extract.samples(VE.EPTDO, n = 8000, clean.names = F)
CrI <- precis(VE.EPTDO, depth = 2, prob = 0.95)@output[c(3:5), c("Mean",  "lower 0.95", "upper 0.95")]

#windows(1.5,1.5)
windowsFonts(A = windowsFont("Times New Roman"))

minlab <- round(min(data.frame(x = logistic(s$a[,c(3,2,4)]))), 2)
maxlab <- round(max(data.frame(x = logistic(s$a[,c(3,2,4)]))), 2)
midlab <- round((maxlab+minlab)/2, 2)

flying <- ggplot() + 
        geom_density(aes(x=x), fill="white", 
                     data = data.frame(x = logistic(s$a[,3])), alpha=.5) + 
        geom_density(aes(x=x), fill="grey", 
                     data=data.frame(x = logistic(s$a[,2])), alpha=.5)+  
        geom_density(aes(x=x), fill="black", 
                     data=data.frame(x = logistic(s$a[,4])), alpha=.5)+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"))+
        xlab("Flying") +
        ylab("Density") +
        theme(axis.text = element_text(family = "A", size = 10),
              axis.title = element_text(family = "A", face = "bold")) + 
        scale_x_continuous(breaks = c(minlab, midlab, maxlab),
                           labels = c(minlab, midlab, maxlab))

###############
#savePlot(filename = paste0("EPTDO","Rplot"), type = c("jpg"), device=dev.cur(), res = 300)
flying

#ss_25
#####
load(paste0("DisturbanceIntercepts_", "SS_25"))
s <- extract.samples(VE.SS_25, n = 3000, clean.names = F)
CrI <- precis(VE.SS_25,depth = 2, prob = 0.95)@output[c(2:4), c("Mean",  "lower 0.95", "upper 0.95")]

#windows(2,2)
windowsFonts(A = windowsFont("Times New Roman"))

minlab <- round(min(data.frame(x = s$a[,c(3,2,4)])), 2)
maxlab <- round(max(data.frame(x = s$a[,c(3,2,4)])), 2)
midlab <- round((maxlab+minlab)/2, 2)

SS25 <- ggplot() + 
        geom_density(aes(x=x), fill="white", data = data.frame(x =s$a[,3]), alpha=.5) + 
        geom_density(aes(x=x), fill="grey", data=data.frame(x = s$a[,2]), alpha=.5)+  
        geom_density(aes(x=x), fill="black", data=data.frame(x = s$a[,4]), alpha=.5)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limits=c(0,NA), expand=c(0,0)) + 
        xlab("SS25 (m)")+
        ylab("Density") +
        theme(axis.text = element_text(family="A", size = 10),
              axis.title = element_text(family = "A", face = "bold")) + 
        scale_x_continuous(breaks = c(minlab, midlab, maxlab),
                           labels = c(minlab, midlab, maxlab))
###############
#savePlot(filename = paste0("ss25", "Rplot"), type = c("jpg"), device = dev.cur())
SS25

#create legend
#####
x <- data.frame(x= logistic(s$a[, 3]), Condition = "Least Disturbed")
x <- rbind(x, data.frame(x= logistic(s$a[, 2]), Condition = "Intermediate"))
x <- rbind(x, data.frame(x= logistic(s$a[, 4]), Condition = "Most Disturbed"))

legplot <- ggplot() + 
  geom_density(aes(x = x, fill = Condition), data = x, alpha=.5)+
  scale_fill_manual(values=c("white", "grey", "black")) + 
  theme(legend.text = element_text(family="A", size=10),
        legend.title = element_text(family = "A", size = 12), 
        legend.title.align = 0.5, 
        legend.key.size = unit(0.75, "lines"),
        legend.justification = "center") + 
  labs(fill = "Site Condition")

legend <- g_legend(legplot)
##########

#Traits 
#####
#ordering
tras <- c("DisturbanceIntercepts_ESP", "DisturbanceIntercepts_VOM",
          "DisturbanceIntercepts_DSF", 
          "DisturbanceIntercepts_BSLM", "DisturbanceIntercepts_FDH",  
          "DisturbanceIntercepts_LSLS", "DisturbanceIntercepts_FSS")
windowsFonts(A = windowsFont("Times New Roman"))
out <- data.frame()

plotlist <- list(flying, SS25, legend)
count<-length(plotlist)+1
for (i in tras){
  #i<-"DisturbanceIntercepts_DSF"
  load(i)
  #windows(2,2)
  
  xaxisName <- unlist(lapply(strsplit(i, "_"), "[[", 2))
  xaxisName <- ifelse(length(xaxisName)!=3,substring(xaxisName,1,3), xaxisName)
  
  s <- extract.samples(VE.traits, n = 3000, clean.names = F)
  samp.out <- data.frame(i, s$a[,c(3, 2, 4)])
  names(samp.out) <- c("model", "least", "fair", "most")
  
  CrI <- round(precis(VE.traits,depth = 2, prob = 0.95)@
                 output[c(3:5), c("Mean",  "lower 0.95", "upper 0.95")],2)
  z <- apply(CrI,2,function(x) round(logistic(x),3))
  rownames(z) <- c("fair", "least", "most")
  out <- rbind(out, data.frame(z,i))
  
  minlab <- round(min(data.frame(x = logistic(s$a[,c(3,2,4)]))), 2)
  maxlab <- round(max(data.frame(x = logistic(s$a[,c(3,2,4)]))), 2)
  midlab <- round((maxlab+minlab)/2, 2)
  
  plotlist[[count]] <- ggplot() + 
    geom_density(aes(x=x), 
                 fill="white", 
                 data = data.frame(x = logistic(s$a[,3])), 
                 alpha=.5) + 
    geom_density(aes(x=x), 
                       fill="grey", 
                       data=data.frame(x = logistic(s$a[,2])), 
                       alpha=.5) +  
    geom_density(aes(x=x), 
                       fill="black", 
                       data=data.frame(x = logistic(s$a[,4])), 
                       alpha=.5) +
    theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black")) +
    scale_y_continuous(limits=c(0, NA), expand=c(0, 0)) + 
    xlab(xaxisName)+
    ylab("Density") +
    theme(axis.text = element_text(family="A", size = 10),
                axis.title = element_text(family = "A", face = "bold")) + 
    scale_x_continuous(breaks = c(minlab, midlab, maxlab),
                       labels = c(minlab, midlab, maxlab))
  
  count<-count+1
  #savePlot(filename = paste0(i,"Rplot"), type = c("jpg"), device=dev.cur())
}

###############
#windows(4.5, 4.5)

grid.arrange(grobs = plotlist, 
             layout_matrix = rbind(c(1, 4, 7),
                                   c(2, 5, 8),
                                   c(3, 6, 9),
                                   c(NA,NA,10)))
#################
#savePlot(filename = "disturbance", type = c("jpg"), device = dev.cur())
