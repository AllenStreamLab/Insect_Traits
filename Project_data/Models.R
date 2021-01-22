##############
# new models are written to working directory 
# careful that they do not overwrite anything 
# important!
##############

#Traits analysis
#library
library(randomForest)
library(rethinking)
library(sf)

#set directory
project.directory <- "C:/Users/AllenLabWorkstation/Dropbox/Dissertation/Chapter_3_Distance_Deposition/Manuscript/Ecography_Submission/Project_data"
nrsas <- read.csv(paste0(project.directory, "/Traitsdata.csv"))

#read in suporting functions
source(paste0(project.dir,"/Supporting_functions.R"))

#Variable Vector Assigments
#####
trait_abvname <- c("DSF", "DSN", "DSS", "ESP", "ESW", 
                   "FDH", "FDL", "FSS", "FSW", "LSL", 
                   "LSS", "LSV", "BSL", "BSM", "BSS", 
                   "VOS", "VOM", "VOU")

con_traits <- c("DSF", "ESP","VOM")
ext_traits <- c("FDH", "FSS", "LSL", "BSL")

dat.vars <- c("XCEMBED", "TOTDASQKM", "SLOPE", "Urb2011ws", "ag2011ws",
              "Q0001E", "JulAug_tempC", "Pfn", "MAXELEVSMO","MINELEVSMO",
              "XWIDTH", "XDEPTH_CM", "PRECIPVC", "PRECIP_CoV",
              "TEMPVC", "TEMPVC_CoV", "XFC_ALG", "exp")
res.vars <- c("eptdo", "SS_25", 
              "DSF_p", "ESP_p", "VOM_p",
              "FDH_p", "FSS_p", "LSLS_p", "BSLM_p")
################

##################
# random forest modeling
##################

#Random forests
#####
# eptdo
formulas <- list(Formula_eptdo <- eptdo ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp,
                 Formula_ss_25 <- SS_25 ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp,
                 Formula_DSF <- DSF_p ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp,
                 Formula_ESP <- ESP_p ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp,
                 Formula_VOM <- VOM_p ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp,
                 Formula_FDH <- FDH_p ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp,
                 Formula_FSS <- FSS_p ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp,
                 Formula_LSLS <- LSLS_p ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp,
                 Formula_BSLM <- BSLM_p ~ XCEMBED +  TOTDASQKM + SLOPE + Urb2011ws + ag2011ws +
                   Pfn + elev + vpu + PRECIPVC + PRECIP_CoV + TEMPVC + TEMPVC_CoV + exp)

#iterate through formulas
for (p in 1:length(formulas)){
  #p <- 3
  response <- as.character(formulas[[p]])[2]
  print(response)
  
  #prepare data
  #####
  nrsas.cc <- nrsas
  nrsas.cc$BSLM_p <- nrsas.cc$BSM_p + nrsas.cc$BSL_p
  nrsas.cc$LSLS_p <- nrsas.cc$LSS_p + nrsas.cc$LSL_p

  dat.vars <- c("XCEMBED", "TOTDASQKM", "SLOPE", "Urb2011ws", "ag2011ws",
                "Q0001E", "JulAug_tempC", "Pfn", "MAXELEVSMO","MINELEVSMO",
                "XWIDTH", "XDEPTH_CM", "PRECIPVC", "PRECIP_CoV",
                "TEMPVC", "TEMPVC_CoV", "XFC_ALG", "exp")

  #check for complete cases here 
  nrsas.cc <- nrsas.cc[complete.cases(nrsas.cc[, c(response, dat.vars)]), ]

  #transformed and scaled to make plots
  #also puts variables on same scale for imporatance
  nrsas.cc$TOTDASQKM <- log(nrsas.cc$TOTDASQKM)
  nrsas.cc$Q0001E <- log(nrsas.cc$Q0001E + 1) #some sites have zero flow
  nrsas.cc$elev <- (nrsas.cc$MAXELEVSMO + nrsas.cc$MINELEVSMO)/2
  nrsas.cc$elev <- log(nrsas.cc$elev + 1) #some sites are at sea level
  nrsas.cc$XDEPTH_CM <- log(nrsas.cc$XDEPTH_CM + 1)
  nrsas.cc$XWIDTH <- log(nrsas.cc$XWIDTH + 1)
  nrsas.cc$Urb2011ws <- car::logit(nrsas.cc$Urb2011ws)
  nrsas.cc$PRECIPVC <- log(nrsas.cc$PRECIPVC)
  nrsas.cc$exp <- as.factor(nrsas.cc$exp)

  dat.vars <- c("XCEMBED", "TOTDASQKM", "SLOPE", "Urb2011ws", "ag2011ws",
                "Q0001E", "JulAug_tempC", "Pfn", "elev", "XWIDTH", 
                "XDEPTH_CM", "PRECIPVC", "PRECIP_CoV",
                "TEMPVC", "TEMPVC_CoV", "XFC_ALG")

  #scaled for ploting- scaling did not effect results
  nrsas.cc[,dat.vars] <- scale(nrsas.cc[, dat.vars])[,]

  #remove correlated variables
  #TOTDASQKM = c(Q0001E, XWIDTH, XDEPTH_CM)
  #TEMPVC = c("JulAug_tempC")
  #cor.vars <- cor(nrsas.cc[,dat.vars])
  #apply(cor.vars, 1, function (x) x[which(x > 0.7)])

  ################
  
  #split test&trainaing  80/20
  ######
  ind <- sample(2, nrow(nrsas.cc), replace = T, prob = c(0.80, 0.20))
  traindat <- nrsas.cc[ind == 1, ]
  testdat <- nrsas.cc[ind == 2, ]
  ################
  
  #optimize mtry parameter - sensitivity analysis of mtry parameter
  ######
  results_mtry_optimization <- matrix(data = NA , nrow = 0, ncol = 3)
  for (i in c(seq(from = 10, to = 1000 , by = 10))){  # values of ntree
    #print(i)
    #c(1, "2*sqrt(p)","0.2*p", "p/3", "p") where p is number of variables. P/3 is rf default
    for (j in c(1, 3, 4, 7, 13)){   
      rf_ij <- randomForest(formulas[[p]], data = traindat,
                          importance = TRUE, proximity = TRUE, 
                          ntree = i, mtry = j)
      results_mtry_optimization <- rbind(results_mtry_optimization, 
                                  c(i, j, tail(rf_ij$rsq, 1)))
    }
  }

  # Clean up the file format
  results_mtry_optimization <- as.data.frame(results_mtry_optimization)
  colnames(results_mtry_optimization) <- c("ntree", "mtry", "PVE")

  mtry <- results_mtry_optimization[which.max(results_mtry_optimization$PVE), ]
  mtry_defaut.max <- max(results_mtry_optimization$PVE[results_mtry_optimization$mtry == 4])

  #what is the imporvement in PVE from optimal mtry? 
  #if it is not <5% improvement use the default.
  if(abs(mtry$PVE - mtry_defaut.max) <= 0.05){
    mtry.default.max <- results_mtry_optimization[results_mtry_optimization$mtry == 4, ]  
    mtry <- mtry.default.max[which.max(mtry.default.max$PVE),]
  }
  
  ################
  
  #check repeatability of random forest to set ntree parameater
  ######
  results_ntree_optimization <- matrix(data = NA , nrow = 0, ncol = 2)
  for (tree in seq(500, 3000, by = 500)){
    #p <- ntree
    rf_1 <- randomForest(formulas[[p]], data = traindat, 
                         importance = TRUE, proximity = TRUE, 
                         ntree = tree, mtry = mtry$mtry)

    rf_2 <- randomForest(formulas[[p]], data = traindat, 
                         importance = TRUE, proximity = TRUE, 
                         ntree = tree, mtry = mtry$mtry)

    r <- cor(importance(rf_1, scale = T, type = 1), 
             importance(rf_2, scale = T, type = 1))
  
    results_ntree_optimization <- rbind(results_ntree_optimization, c(tree, r))
    if(r >= 0.99){break()}
  }
  
  results_ntree_optimization <- as.data.frame(results_ntree_optimization)
  colnames(results_ntree_optimization) <- c("ntree", "corr")

  ################
  
  #reun model with optimized parameters
  ######
  rf <- randomForest(formulas[[p]], data = traindat, 
                     ntree = tree, mtry = mtry$mtry,
                     importance = T, proximity = T)

  rf_model_list <- list(model_form = formulas[[p]],
                        traindat = traindat, 
                        testdat = testdat, 
                        optimize_mtry = results_mtry_optimization,
                        optimize_ntree = results_ntree_optimization, 
                        randomforest = rf)

  ################
  
  #writes models to WD - will overwrite optimized models
  #save(rf_model_list, file = paste0("rf_", response, "_optimize"))
}
################

#sensitivity of importance variable to traits data
#set to ecography project directory to read optimized RF files
setwd(project.directory)
######
out2 <- data.frame()
for (q in seq(0.5, 1, by = 0.1)){
  for(p in c("rf_BSLM_p_optimize", "rf_FDH_p_optimize",  
             "rf_LSLS_p_optimize", "rf_FSS_p_optimize", 
             "rf_ESP_p_optimize",  "rf_VOM_p_optimize", 
             "rf_DSF_p_optimize")){

    #p <- "rf_BSLM_p_optimize"
    load(p)
    newdata <- rbind(rf_model_list[["traindat"]], rf_model_list[["testdat"]])
  
    #select traits threshold
    uid <- nrsas$UID[nrsas$Trait_Assign >= q]
    newdata <- newdata[newdata$UID%in%uid, ]

    rf <- randomForest(rf_model_list[["model_form"]], 
                                data = newdata, 
                                mtry = 4, ntree = 3000, importance = T)

  imp2 <- importance(rf)
  temp <- data.frame(p, rsq = round(tail(rf$rsq,1),2), 
                     var = rownames(imp2[order(imp2[,1], decreasing = T),])[1:6], 
                     MDA = imp2[order(imp2, decreasing = T)][1:6], 
                     rank = c(1:6), threshold = q, n = nrow(newdata))

  out2 <- rbind(temp,out2)
  }
}


#Format table
#table shows the change in variable importance
#and rsq for the different datasets
w <- reshape2::dcast(p + rsq + threshold +n ~ var, data = out2, value.var = "rank")
w <- w[w$threshold < 0.9, ]
w <- w[order(w$p, w$threshold),]
names(w) <- c("model_name", "rsq", "threshold","n", "AGRI", "FINES",
              "SLOPE","TEMP_MA","TEMP_CoV", "EMBED", "ELEV",
              "HYROCLASS","PRECIP_CoV","PRECIP","REGION","URBAN","AREA")

w$model_name <- unlist(lapply(strsplit(as.character(w$model_name), "_"),"[[",2))
w[is.na(w)] <- 0
w<-t(w)
################
#write.csv(w, "trait_threshold_sensitivity.csv")

#preformance metrics
#####
optifiles <- c("rf_eptdo_optimize", "rf_SS_25_optimize", 
               "rf_BSLM_p_optimize", "rf_FDH_p_optimize",  
               "rf_LSLS_p_optimize", "rf_FSS_p_optimize", 
               "rf_ESP_p_optimize",  "rf_VOM_p_optimize", 
               "rf_DSF_p_optimize")  

out <- data.frame()
for (i in optifiles){
  #i <- "rf_eptdo_optimize"
  load(i)
 
  resp <- as.character(rf_model_list[["model_form"]])[2]
  mean_resp = round(mean(c(rf_model_list[["testdat"]][,resp],rf_model_list[["traindat"]][,resp])), 2)
  sd_resp = round(sd(c(rf_model_list[["testdat"]][,resp],rf_model_list[["traindat"]][,resp])), 2)
  
  nse <- round(NSE(OBS = rf_model_list[["testdat"]][,resp], 
             PRED = predict(rf_model_list[["randomforest"]], newdata = rf_model_list[["testdat"]])), 2)
  rsr <- round(RSR(OBS = rf_model_list[["testdat"]][,resp], 
             PRED = predict(rf_model_list[["randomforest"]], newdata = rf_model_list[["testdat"]])), 2)
  mae <- round(MAE(OBS = rf_model_list[["testdat"]][,resp], 
             PRED = predict(rf_model_list[["randomforest"]], newdata = rf_model_list[["testdat"]])), 2)
  
  temp <- data.frame(resp, 
                     ntrain = nrow(rf_model_list[["traindat"]]),
                     ntest = nrow(rf_model_list[["testdat"]]),
                     mean_resp, sd_resp,
                     rsq = round(tail(rf_model_list[["randomforest"]]$rsq, 1), 2),
                     nse, rsr, mae)
  out <- rbind(out, temp)
}
################
#write.csv(out, "modelvalidation.csv", row.names = F)



#########################################
# NRSA Disturbance intercept models 
# (3 levels: Least disturbed, most disturbed)
#########################################

nrsas.cc <- nrsas  
nrsas.cc$index <- coerce_index(as.factor(as.character(nrsas.cc$RT_NRSA_CAT)))
unique(nrsas.cc[,c("index", "RT_NRSA_CAT")])

# EPTDO
#####
i <- "eptdo"
d <- data.frame(n = as.integer(nrsas.cc$TOTAL_taxa), 
                y = as.integer(nrsas.cc[, i]*nrsas.cc$TOTAL_taxa), 
                index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model with disturbance class
#prior came from logit transformation of Greenwood and Booker 2016
logit(0.83)
logit(0.14)

VE.EPTDO <- map2stan(
  alist(
    y ~ dbetabinom(n, p, theta),
    
    logit(p) <- a[index],
    a[index] ~ dnorm(1.58, 1.82),
    theta ~ dexp(1)
  ), 
  
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  constraints = list(theta = "lower=0"), start = list(theta = 3),
  control = list(max_treedepth = 15))

#MCMC
tracerplot(VE.EPTDO)

#huge difference between least and most distrubed! 
Mu <- precis(VE.EPTDO, depth = 2, prob = 0.95)@output[,c("Mean", "lower 0.95", "upper 0.95")]

index <- unique(nrsas.cc[, c("RT_NRSA_CAT", "index")])
index <- as.character(index[order(index$index),"RT_NRSA_CAT"])
sam_n <- aggregate(d$index, by = list(d$index), function(x) length(x))

Mu.eptdo <- data.frame(mean = logistic(Mu[,c(1)]), 
                       L95 = logistic(Mu[,c(2)]), 
                       U95 = logistic(Mu[,c(3)]),
                       trait = i, 
                       intercept = rownames(Mu),
                       Name = c("NA", index), 
                       sam_n = c(NA, sam_n$x))
mean(nrsas$eptdo)
############
#save(VE.EPTDO, file = paste0("DisturbanceIntercepts_", "eptdo"))
load("DisturbanceIntercepts_eptdo")
diff.p(x = VE.EPTDO) #P[most disturbed < least disturbed]  

# SS_25
#####
i <- "SS_25"  
d <- data.frame(y = nrsas.cc[, i], index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model
VE.SS_25 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a[index],
    a[index] ~ dnorm(21.5, 18),
    sigma ~ dcauchy(0, 1)
  ),
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  control = list(max_treedepth = 15))

tracerplot(VE.SS_25)
Mu <- precis(VE.SS_25, depth = 2, prob = 0.95)@output[,c("Mean", "lower 0.95", "upper 0.95")]

index <- unique(nrsas.cc[, c("RT_NRSA_CAT", "index")])
index <- as.character(index[order(index$index), "RT_NRSA_CAT"])
sam_n <- aggregate(d$index, by = list(d$index), function(x) length(x))

Mu_ss <- data.frame(mean = Mu[,c(1)], 
                    L95 = Mu[,c(2)], 
                    U95 = Mu[,c(3)],
                    trait = i, 
                    parameter = rownames(Mu),
                    Name = c(index,"NA"), 
                    sam_n = c(sam_n$x,"NA"))


############
#save(VE.SS_25, file = paste0("DisturbanceIntercepts_", "SS_25"))
load("DisturbanceIntercepts_SS_25")
diff.SS(x = VE.SS_25, logit_transform = F) #P[most disturbed < least disturbed]

#data prep for traits - complete cases
#####
nrsas.cc <- nrsas[complete.cases(nrsas[, c("TOTAL300", trait_abvname)]), ]
#combine BSM+BSL & LSS+LSL
#bodysize medium is better than bs small
nrsas.cc$BSLM <- nrsas.cc$BSM + nrsas.cc$BSL
nrsas.cc$LSLS <- nrsas.cc$LSS + nrsas.cc$LSL
nrsas.cc$index <- coerce_index(as.factor(as.character(nrsas.cc$RT_NRSA_CAT)))
###############

# DSF
#######
i <- "DSF"
d <- data.frame(n = as.integer(nrsas.cc$TOTAL300), 
                y = as.integer(nrsas.cc[, i]), 
                index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model with disturbance class
VE.traits <- map2stan(
  alist(
    y ~ dbetabinom(n, p, theta),
    logit(p) <- a[index],
    a[index] ~ dnorm(0.5, 2),
    theta ~ dexp(1)
  ), 
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  constraints = list(theta = "lower=0"), start = list(theta = 3),
  control = list(max_treedepth = 15))


#save the models
###############
#save(VE.traits, file = paste0("DisturbanceIntercepts_",i))
load(paste0("DisturbanceIntercepts_", "DSF"))
diff.p(x = VE.traits, logit_transform = T)#P[most disturbed < least disturbed]

# ESP
#######
i <- "ESP"
d <- data.frame(n = as.integer(nrsas.cc$TOTAL300), 
                y = as.integer(nrsas.cc[, i]), 
                index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model with disturbance class
VE.traits <- map2stan(
  alist(
    y ~ dbetabinom(n, p, theta),
    logit(p) <- a[index],
    a[index] ~ dnorm(0.5, 2),
    theta ~ dexp(1)
  ), 
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  constraints = list(theta = "lower=0"), start = list(theta = 3),
  control = list(max_treedepth = 15))
############### 
#save(VE.traits, file = paste0("DisturbanceIntercepts_",i))
load(paste0("DisturbanceIntercepts_","ESP"))
diff.p(x = VE.traits, logit_transform = T) #P[most disturbed < least disturbed]

# VOM
#######
i <- "VOM"
d <- data.frame(n = as.integer(nrsas.cc$TOTAL300), 
                y = as.integer(nrsas.cc[, i]), 
                index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model with disturbance class
VE.traits <- map2stan(
  alist(
    y ~ dbetabinom(n, p, theta),
    logit(p) <- a[index],
    a[index] ~ dnorm(0.5, 2),
    theta ~ dexp(1)
  ), 
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  constraints = list(theta = "lower=0"), start = list(theta = 3),
  control = list(max_treedepth = 15))
############### 
#save(VE.traits, file = paste0("DisturbanceIntercepts_",i))  
load(paste0("DisturbanceIntercepts_","VOM"))
diff.p(x = VE.traits, logit_transform = T)#P[most disturbed < least disturbed]

# FDH
#######
i <- "FDH"
d <- data.frame(n = as.integer(nrsas.cc$TOTAL300), 
                y = as.integer(nrsas.cc[, i]), 
                index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model with disturbance class
VE.traits <- map2stan(
  alist(
    y ~ dbetabinom(n, p, theta),
    logit(p) <- a[index],
    a[index] ~ dnorm(0.5, 2),
    theta ~ dexp(1)
  ), 
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  constraints = list(theta = "lower=0"), start = list(theta = 3),
  control = list(max_treedepth = 15))
############### 
#save(VE.traits, file = paste0("DisturbanceIntercepts_",i))    
load(paste0("DisturbanceIntercepts_","FDH"))
diff.p(x = VE.traits, logit_transform = T)#P[most disturbed < least disturbed]

# FSS
#######
i <- "FSS"
d <- data.frame(n = as.integer(nrsas.cc$TOTAL300), 
                y = as.integer(nrsas.cc[, i]), 
                index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model with disturbance class
VE.traits <- map2stan(
  alist(
    y ~ dbetabinom(n, p, theta),
    logit(p) <- a[index],
    a[index] ~ dnorm(0.5, 2),
    theta ~ dexp(1)
  ), 
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  constraints = list(theta = "lower=0"), start = list(theta = 3),
  control = list(max_treedepth = 15))
############### 
#save(VE.traits, file = paste0("DisturbanceIntercepts_",i))      
load(paste0("DisturbanceIntercepts_","FSS"))
diff.p(x = VE.traits, logit_transform = T)#P[most disturbed < least disturbed]

# LSLS
#######
i <- "LSLS"
d <- data.frame(n = as.integer(nrsas.cc$TOTAL300), 
                y = as.integer(nrsas.cc[, i]), 
                index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model with disturbance class
VE.traits <- map2stan(
  alist(
    y ~ dbetabinom(n, p, theta),
    logit(p) <- a[index],
    a[index] ~ dnorm(0.5, 2),
    theta ~ dexp(1)
  ), 
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  constraints = list(theta = "lower=0"), start = list(theta = 3),
  control = list(max_treedepth = 15))
############### 
#save(VE.traits, file = paste0("DisturbanceIntercepts_",i))        
load(paste0("DisturbanceIntercepts_","LSLS"))
diff.p(x = VE.traits, logit_transform = T)#P[most disturbed < least disturbed]

# BSLM
#######
i <- "BSLM"
d <- data.frame(n = as.integer(nrsas.cc$TOTAL300), 
                y = as.integer(nrsas.cc[, i]), 
                index = nrsas.cc$index)
d <- d[complete.cases(d), ]

#intercept only model with disturbance class
VE.traits <- map2stan(
  alist(
    y ~ dbetabinom(n, p, theta),
    logit(p) <- a[index],
    a[index] ~ dnorm(0.5, 2),
    theta ~ dexp(1)
  ), 
  data = d, iter = 10000, warmup = 2000, chains = 4, cores = 4, 
  constraints = list(theta = "lower=0"), start = list(theta = 3),
  control = list(max_treedepth = 15))
############### 
#save(VE.traits, file = paste0("DisturbanceIntercepts_",i))   
load(paste0("DisturbanceIntercepts_","BSLM"))
diff.p(x = VE.traits, logit_transform = T)#P[most disturbed < least disturbed]

