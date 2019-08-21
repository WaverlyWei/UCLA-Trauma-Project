library(readstata13)
library(vimp)
library(SuperLearner)
library(ctmle)
library(ggplot2)
library(Amelia)
library(superheat)
setwd("/Users/waverlywei/Desktop/New Trauma Project ")
dat.1 <- readxl::read_excel("Master by Neighborhood for Alan.xlsx",sheet = 1)
dat.2 <- read.dta13("chronicdz3.dta")

# variable selection with vim package 

sl_lib = c("SL.glm","SL.mean","SL.glmnet","SL.rpart")

# set covariates
cov = dat.2[,c(2:4,25,49:83)]

# =====  homicide as outcome ======== #
df_imp = cbind(out=dat.2$homicide_ayll,cov)

n = ncol(df_imp) - 1
imp = low = high = NULL
for (i in 1 :n){
single_vim =vim(f1 = y~x, f2 = fit~x, data = df_imp, y = df_imp[,1], indx = i, SL.library = sl_lib)
# create importance measure vector and CI's
imp = c(imp,single_vim$est)
low = c(low, single_vim$ci[1])
high = c(high, single_vim$ci[2])
}

nam = names(df_imp)[2:ncol(df_imp)]
df = data.frame(imp, low, high, nam)
imp_descend = df[with(df,order(-imp)),]
save("imp_descend", file = "imp_descend.Rda")


# ==========all "_ayll" variables as outcomes + covariates ===========#
df_all =  cbind(dat.2[,5:24],cov)
imp = low = high = out_name = cov_name = NULL

# remove NA outcomes 
NA_index = apply(df_all[,1:20],1, function(x) any(is.na(x)))
df_all = df_all[!NA_index,]



# create a new dataframe for each outcome variable to feed into importance measure function
for (i in 1:20){
for (j in 1:39){
  print(i)
  df_now = df_all[,c(i,21:ncol(df_all))]
  single_vim =vim(f1 = y~x, f2 = fit~x, data = df_now, y = df_now[,1], indx = j, SL.library = sl_lib)
  out_name = c(out_name, names(df_all)[i])
  cov_name = c(cov_name,names(df_now)[j])
  imp = c(imp,single_vim$est)
  low = c(low, single_vim$ci[1])
  high = c(high, single_vim$ci[2])
}
}


whole = data.frame(out_name, cov_name, imp, low, high)

save("whole", file = "whole.Rda")

idx = seq(1,nrow(whole),39)

whole_new = whole[-idx, ]

## Heatmap using superheat
# re-organize data
try = matrix(whole_new$imp, nrow = 20, ncol = 38,byrow = TRUE)
rownames(try) = names(dat.2)[5:24]
colnames(try) = whole_new$cov_name[1:38]
superheat(try,bottom.label.text.angle = 80,bottom.label.text.size = 3,row.dendrogram = TRUE,
          title = "Variable Importance Measures")


## ===========SKIP=======
# TRY heatmap using ggplot
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

# set Y
Y = df_imp$out
N = nrow(df_imp)
str_vec = c()
est_vec = c()
p_vec = c()
low_vec = c()
high_vec = c()
# set pbpl as A for now 
# > mean ==1, <mean == 0 
for ( i in 3:40){
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
vec = c(vec,str)
}

# create df 
df = cbind(names(df_imp)[3:40],vec)
names(df) = c("A","important vars")

# out
save(df,file = "ctmle_results.Rda")




#-----------------------
# PCA
#------------------------













