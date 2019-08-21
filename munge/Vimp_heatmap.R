library(readstata13)
library(vimp)
library(SuperLearner)
library(ctmle)
library(ggplot2)
library(Amelia)
library(superheat)
library(dplyr)
library(heatmap3)
library(xtable)
library(RColorBrewer)
library(heatmap.plus)
setwd("/Users/waverlywei/Desktop/Violence Project ")
dat.1 = readxl::read_excel("Master by Neighborhood for Alan.xlsx",sheet = 1)
dat.2 = read.dta13("chronicdz3.dta")

# variable selection with vim package \

# compute percentage variable for ages 
percentage <- c()
for (i in 51: 63){
  p <- dat.2[,i] / dat.2$totalpopest * 100
  percentage <- cbind(percentage, p)
}

# race percentages
p_native <- dat.2[,66] / dat.2$totalpopest * 100
p_api <- dat.2[,68] / dat.2$totalpopest * 100
p_tworaces <- dat.2[,69] / dat.2$totalpopest * 100
p_latinoany <- dat.2[,71] / dat.2$totalpopest * 100
percentage <- cbind(percentage, p_native,p_api,p_tworaces,p_latinoany)


per_name <- names(dat.2[c(51:63,66,68,69,71)])
per_name <- gsub("total", "percent", per_name)
colnames(percentage) <- per_name
# combine with dataframe
dat.2 <- cbind(dat.2,percentage)
dat.2 <- dat.2 %>% mutate(percentpop_male = totalpop_male / totalpopest * 100)
# select covariates set
# cov <- dat.2 %>% select(mhi,pbpl,totalhouse_est,percentpop_white,
#                         percentpop_black,percentpop_asian,percentpop_latino,
#                         percentpop_female,percentpop_male)

# using set suggested by Marissa 
cov <- dat.2 %>% select(mhi,pbpl,totalhouse_est,
                        percentpop_white,percentpop_native,
                        percentpop_black,percentpop_asian,
                        percentpop_latino,percentpop_latinoanyrace,
                        percentpop_api,percentpop_tworaces,
                        percentpop_female,percentpop_male)

# combine percentage age groups
# 5 - 19
dat.2 %>% select(percentpop_5to9yrs,
                 percentpop_10to14yrs,
                 percentpop_15to19yrs) %>% 
      rowSums(na.rm=TRUE) -> dat.2$percentpop_5to19yrs
# 20 - 44
dat.2 %>% select(percentpop_20to24yrs,
                 percentpop_25to34yrs,
                 percentpop_35to44yrs) %>% 
  rowSums(na.rm=TRUE) -> dat.2$percentpop_20to44yrs

# 45 - 59
dat.2 %>% select(percentpop_45to54yrs,
                 percentpop_55to59yrs) %>% 
  rowSums(na.rm=TRUE) -> dat.2$percentpop_45to59yrs

# 60 - 84
dat.2 %>% select(percentpop_60to64yrs,
                 percentpop_65to74yrs,
                 percentpop_75to84yrs) %>% 
  rowSums(na.rm=TRUE) -> dat.2$percentpop_60to84yrs
                                                                   
cov <- cbind(cov, dat.2[,c(86,103:106,98)])

save(cov, file = "cov.Rda")




# ===== only for homicide ======== #
df_imp = cbind(out=dat.2$homicide_ayll,cov)

n = ncol(df_imp) - 1
imp = low = high = NULL
for (i in 1:n){
single_vim =vim(f1 = y~x, f2 = fit~x, data = df_imp, y = df_imp[,1], indx = i, SL.library = sl_lib)
imp = c(imp,single_vim$est)
low = c(low, single_vim$ci[1])
high = c(high, single_vim$ci[2])
}

nam = names(df_imp)[2:ncol(df_imp)]
df = data.frame(imp, low, high, nam)
imp_descend = df[with(df,order(-imp)),]
save("imp_descend", file = "imp_descend.Rda")


## Changed vignette 
## Re-run vimp 

learner.lib <- c("SL.mean", "SL.xgboost", "SL.glmnet", "SL.randomForest")

## the full conditional mean
full_regression <- SuperLearner(Y = df_imp$out, X = cov, SL.library = learner.lib)
full_fit <- full_regression$SL.predict

## the reduced conditional mean
reduced_regression <- SuperLearner(Y = full_fit, X = cov[, 1, drop = FALSE], SL.library = learner.lib)
reduced_fit <- reduced_regression$SL.predict
## -------------------------------------------------------------
## get variable importance!
## -------------------------------------------------------------
## get the variable importance estimate, SE, and CI
vimp <- vimp_regression(Y = df_imp$out, f1 = full_fit, f2 = reduced_fit, indx = 1, run_regression = FALSE)




# ==========for all the outcomes vs covariates ===========#
df_all <-  cbind(dat.2[,5:24],cov)
imp <- low <- high <- out_name <- cov_name <- NULL

# remove NA outcomes 
NA_index <- apply(df_all[,1:20],1, function(x) any(is.na(x)))
df_all <- df_all[!NA_index,]

## IMPUTATION
#imp_W = amelia(df_all[,25:ncol(df_all)])
#write.amelia(imp_W, file.stem = "imputed_data_set_ma")
#complete_W = read.csv("imputed_data_set_ma1.csv")


# everytime create a new dataframe 
for (i in 1:20){
for (j in 1:ncol(cov)){
   print(i)
   df_now = df_all[,c(i,21:ncol(df_all))]
  
  #single_vim = vimp(f1 = y~x, f2 = fit~x, data = df_now, y = df_now[,1], indx = j, SL.library = sl_lib)
  #out_name = c(out_name, names(df_all)[i])
  #cov_name = c(cov_name,names(df_now)[j])
  #imp = c(imp,single_vim$est)
  #low = c(low, single_vim$ci[1])
  #high = c(high, single_vim$ci[2])
  co <- df_now[,2:ncol(df_now)]
  ## the full conditional mean
  full_regression <- SuperLearner::SuperLearner(Y = df_now[,1], X = co, SL.library = learner.lib)
  full_fit <- full_regression$SL.predict
  ## the reduced conditional mean
  reduced_regression <- SuperLearner::SuperLearner(Y = full_fit, X = co[,j, drop = FALSE], SL.library = learner.lib)
  reduced_fit <- reduced_regression$SL.predict
  
  ## -------------------------------------------------------------
  ## get variable importance!
  ## -------------------------------------------------------------
  ## get the variable importance estimate, SE, and CI
  vimp <- vimp_regression(Y = df_now[,1], f1 = full_fit, f2 = reduced_fit, indx = j, run_regression = FALSE)
  out_name <- c(out_name, names(df_all)[i])
  cov_name <- c(cov_name,names(co)[j])
  imp = c(imp,vimp$est)
  low = c(low, vimp$ci[1])
   high = c(high, vimp$ci[2])
}
}

#nam = names(df_imp)[2:ncol(df_imp)]

#whole <- data.frame(out_name, cov_name, imp, low, high)
#save("whole", file = "whole.Rda")
#idx <- seq(1,nrow(whole),39)
#whole_new = whole[-idx, ]

df_all <- data.frame(out_name, cov_name, imp, low, high)
save(df_all, file = "df_all.Rda")


load("df_all.Rda")
## TRY superheat
# re-organize data
try <- matrix(df_all$imp, nrow = 20, ncol = 22,byrow = TRUE)
rownames(try) <- names(dat.2)[5:24]
colnames(try) <- names(cov)


## Categorize neighborhood by percentpop_latino
q25 <-  quantile(cov$percentpop_0to5yrs)[2]
q50 <- quantile(cov$percentpop_0to5yrs)[3]
q75 <- quantile(cov$percentpop_0to5yrs)[4]

label <- cov$percentpop_0to5yrs
category <- ifelse(label <= q25, 1, 
       ifelse(label>=q25 & label<= q50, 2,
              ifelse(label>=q50 & label<= q75, 3,4)))

par(mfrow=c(1,2)) 
for (i in 7:8){
test_label <- cbind(category,dat.2[,i])
plot(test_label)

}

test_label <- data.frame(cbind(category,dat.2[,5]))
d <- dist(test_label, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram

test_label %>% group_by(category) %>% 
  summarise(mean = mean(V2))


## create annotation matrix 
try <- matrix(df_all$imp, nrow = 22, ncol = 20,byrow = FALSE)
colnames(try) <- names(dat.2)[5:24]
rownames(try) <- names(cov)


annotation <- as.matrix(try["percentpop_latino",])
colnames(annotation) <- "percentpop_latino"
q20 <- quantile(annotation,probs = seq(0, 1, 0.2))[2]
q40 <- quantile (annotation,probs = seq(0, 1, 0.2))[3]
q60 <- quantile (annotation,probs = seq(0, 1, 0.2)) [4]
q80 <- quantile (annotation,probs = seq(0, 1, 0.2)) [5]

# write into function
get_annotation <- function(VarName){
  annotation <- as.matrix(try[VarName,])
  colnames(annotation) <- VarName
  return(annotation)
}



mapVarToColor<-function(color, annotation){
  # set color
  col.lst <- brewer.pal(5,color)
  # get quantile
  q20 <- quantile(annotation,probs = seq(0, 1, 0.2))[2]
  q40 <- quantile (annotation,probs = seq(0, 1, 0.2))[3]
  q60 <- quantile (annotation,probs = seq(0, 1, 0.2)) [4]
  q80 <- quantile (annotation,probs = seq(0, 1, 0.2)) [5]
  # assign colors
  colorsVector = ifelse(annotation <= q20, col.lst[1], 
                        ifelse(annotation>=q20 & annotation<=q40, col.lst[2],
                        ifelse(annotation>=q40 & annotation<=q60, col.lst[3],
                        ifelse(annotation>=q60 & annotation<=q80, col.lst[4],col.lst[5]))))
  return(colorsVector)
}



testHeatmap<-function(dat, annotation) {    
  annotation_latino <- get_annotation("percentpop_latino")
  annotation_pbpl <- get_annotation("pbpl")
  annotation_under5 <- get_annotation("percentpop_0to5yrs")
  annotation_over85 <- get_annotation("percentpop_85plus")
  #annotation_mhi <- get_annotation("mhi")
  
  
  sampleColors1 <- mapVarToColor("BuPu",annotation_latino)
  sampleColors2 <- mapVarToColor("YlGn",annotation_pbpl)
  sampleColors3 <- mapVarToColor("Greys",annotation_under5)
  sampleColors4 <- mapVarToColor("YlGnBu",annotation_over85)
  #sampleColors5 <- mapVarToColor("Purples",annotation_mhi)
  
  cols.1 <- cbind(sampleColors1,sampleColors2)
  colnames(cols.1)=c("percentpop_latino","pbpl")
  
  cols.2 <- cbind(sampleColors3,sampleColors4)
  colnames(cols.2)=c("percentpop_under5","percentpop_over85")
  
  cols <- cbind(sampleColors1,sampleColors2,sampleColors3)
  colnames(cols)=c("percentpop_latino","pbpl","percentpop_under5")
  
  # Assign column annotations and make a custom legend for them
  heatmap.plus(try, margins=c(8,10),
                             ColSideColors=cols,main="Variable Importance Measures")
           #legendfun=function()showLegend(legend=c("MiracleDrugA", 
                                                  # "MiracleDrugB", "?", "aa"),
                                          #col=c("blue", "green", "red","yellow"), cex=0.8))
  #heatmap.plus(try, margins=c(8,10),ColSideWidth=1,
                     #ColSideColors=cols.2,main="Variable Importance Measures")
  
  
  
}
testHeatmap(try,annotation)

# ----------------------------------

# Neighborhood VS.Outcome Heatmap

#------------------------------------

df.N <- matrix(unlist(dat.2[,5:24]),nrow = 20,byrow = TRUE)
colnames(df.N) <- dat.2$neighborhood
rownames(df.N) <- names(dat.2)[5:24]
# replace missing values with 0 
df.N[is.na(df.N)] <- 0


for(i in 1:nrow(df.N)){
  
  maxCol <- max(df.N[i,])
  scaled <- sapply(df.N[i,], function(x) (maxCol-x)/maxCol)
  df.N[i,] <- scaled
}


Neighbor_annotation <- function(VarName){
  annotation <- as.matrix(dat.2[,VarName])
  colnames(annotation) <- VarName
  return(annotation)
}

NeighborhoodHeatmap<-function(dat, annotation) {    
  annotation_latino <- Neighbor_annotation("percentpop_latino")
  annotation_pbpl <- Neighbor_annotation("pbpl")
  annotation_under5 <- Neighbor_annotation("percentpop_0to5yrs")
  annotation_over85 <- Neighbor_annotation("percentpop_85plus")
  annotation_mhi <- Neighbor_annotation("mhi")
  
  
  sampleColors1 <- mapVarToColor("BuPu",annotation_latino)
  sampleColors2 <- mapVarToColor("YlGn",annotation_pbpl)
  sampleColors3 <- mapVarToColor("Greys",annotation_under5)
  sampleColors4 <- mapVarToColor("YlGnBu",annotation_over85)
  sampleColors5 <- mapVarToColor("Purples",annotation_mhi)
  
  
  cols.1 <- cbind(sampleColors1,sampleColors2,sampleColors5)
  colnames(cols.1)=c("percentpop_latino","pbpl","mhi")
  cols.2 <- cbind(sampleColors3,sampleColors4)
  colnames(cols.2)=c("percentpop_under5","percentpop_over85")
  
  heatmap.plus(df.N, margins=c(8,10),
               ColSideColors=cols.1,main="Neighborhood VS.Scaled Outcome")
  
  heatmap.plus(df.N, margins=c(8,10),
               ColSideColors=cols.2,main="Neighborhood VS.Scaled Outcome")
  
  
  
}
testHeatmap(try,annotation)


heatmap.plus(df.N, margins=c(8,10))


##  Superheat 
superheat(try,bottom.label.text.angle = 90,bottom.label.text.size = 2.5,row.dendrogram = TRUE,
          title = "Variable Importance Measures",left.label.text.size = 3)
#superheat(try,bottom.label.text.angle = 80,bottom.label.text.size = 3,n.clusters.rows = 3,
          #left.label = 'variable')
# ==========TODO: heat map===========#
# USE CTMLE results 
#out = rep("out",nrow(imp_descend))
#df_for_hm = cbind(out, imp_descend)

h = ggplot(whole_new, aes(out_name,cov_name)) +ggtitle('Variable Importance Measures')+
  geom_tile(aes(fill = imp), color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  ylab("Covariates") +
  xlab("Outcome") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "variable importance")
h




## bar plot 
imp_top = imp_descend[1:10,]
p <- ggplot(imp_top,aes(imp_top$nam,imp_top$imp))
p = p + geom_crossbar(aes(ymin = imp_top$low,ymax = imp_top$high,colour = "red"), width = 0.1) + 
  ylab("importance")+xlab("Covariates")+ggtitle("ayll_homicide Top10 Important Variables")+
  theme(axis.text.x = element_text(face="bold", size=8, angle=45))+ 
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))





## ==============variable selection with C-TMLE==============
## To DO: automate the var selection 
## set everything binary 
## Now only run on subset 

# set Y
Y <- df_imp$out
N <- nrow(df_imp)
str_vec <- c()
vec <- c()
est_vec <- c()
p_vec <- c()
low_vec <- c()
high_vec <- c()
# set pbpl as A for now 
# > mean ==1, <mean == 0 
for ( i in 2:ncol(df_imp)){
A = ifelse(df_imp[,i]<=mean(df_imp[,i]),0,1)
# set W
W = df_imp[,-c(1,i)]
# Q
Q = cbind(rep(mean(Y[A == 0]), N), rep(mean(Y[A == 1]), N))
# variable selection 
#ctmle_discrete_fit1 <- ctmleDiscrete(Y = Y, A = A, W = data.frame(W), Q = Q,
                                    # preOrder = FALSE, detailed = TRUE)
  



# try fit2, w/o preset Q
ctmle_discrete_fit2 <- ctmleDiscrete(Y = Y, A = A, W = data.frame(W),
                                     preOrder = FALSE, detailed = TRUE)

res = summary(ctmle_discrete_fit2)
# selected candidate
selected = res$selected
str = paste(res$terms[1:selected],collapse = ",")
# get ctmle est
est <- res$est
ci_low <- res$CI[1]
ci_high <- res$CI[2]
p <- res$pvalue
vec <- c(vec,str)
est_vec <- c(est_vec,est)
low_vec <- c(low_vec,ci_low)
high_vec <- c(high_vec,ci_high)
p_vec <- c(p_vec,p)
}

# create df 
df <- cbind(names(df_imp)[2:ncol(df_imp)],vec)
names(df) = c("Treatment","important vars")

# out
save(df,file = "ctmle_var.Rda")

adjusted <- cbind(est_vec,low_vec,high_vec,p_vec)

colnames(adjusted) <- c("Adjusted CTMLE", "CI_low", "CI_high", "p value")
rownames(adjusted) <- names(df_imp)[2:ncol(df_imp)]
save(adjusted,file = "adjusted.Rda")

xtable(adjusted)


