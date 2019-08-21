# ----------------------------------

# Neighborhood VS.Outcome Heatmap

#------------------------------------
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

NeighborhoodHeatmap<-function(dat, annotation) {   
  annotation_mhi <- Neighbor_annotation("mhi")
  annotation_black <- Neighbor_annotation("percentpop_black")
  annotation_under65 <- Neighbor_annotation("percentpop_under65")
  #annotation_19plus <- Neighbor_annotation("percentpop_19plus")
  #annotation_est <- Neighbor_annotation("totalhouse_est" )
  #annotation_20to24 <- Neighbor_annotation("percentpop_20to24yrs")
 # annotation_female <- Neighbor_annotation("percentpop_female")
  #annotation_black <- Neighbor_annotation("percentpop_black")
  
  sampleColors1 <- mapVarToColor("YlOrRd",annotation_mhi)
  sampleColors2 <- mapVarToColor("YlOrRd",annotation_black)
  sampleColors3 <- mapVarToColor("YlOrRd",annotation_under65)
  #sampleColors4 <- mapVarToColor("YlOrRd",annotation_20to24)
  #sampleColors5 <- mapVarToColor("YlOrRd",annotation_female)
  #sampleColors6 <- mapVarToColor("YlOrRd",annotation_black)
  
  
  cols.1 <- cbind(sampleColors1,sampleColors2,sampleColors3)
  colnames(cols.1)=c("mhi","percentpop_black","percentpop_under65")
                     
  # cols.2 <- cbind(#sampleColors4,
  #   sampleColors5,sampleColors6)
  # colnames(cols.2)=c(#"percentpop_under20to24",
  #   "percentpop_feamle","percentpop_black")
  
   heatmap.plus(df.N, margins=c(8,10),
               ColSideColors=cols.1,main="Neighborhood VS.Scaled Outcome")
  
  # heatmap.plus(df.N, margins=c(8,10),
  #              ColSideColors=cols.2,main="Neighborhood VS.Scaled Outcome")
  
  
  
}
testHeatmap(try,annotation)


heatmap.plus(df.N, margins=c(8,10))