rm(list=ls(all=TRUE))
library(sqldf)
library(raster)
library(plyr)
library(dplyr)


#INPUT FILE:
comparisonfile<-"Cartel1.csv"
comparisonType<-"custom"         # "quantiles" or "custom"


if (comparisonType!="quantiles"){        
  
  A_low<-0
  A_high<-0.9
  
  B_low<-0
  B_high<-0.9
  
}


calcPerformance<-function(A_1_B_1,A_0_B_1,A_1_B_0,A_0_B_0){
  accuracy<-(nrow(A_1_B_1)+nrow(A_0_B_0))*100/nrow(comparisondf)
  
  # Contingency table
  xtab <- as.table(rbind(c(nrow(A_1_B_1),nrow(A_1_B_0)), 
                         c(nrow(A_0_B_1), nrow(A_0_B_0))))
  # Descriptive statistics
  diagonal.counts <- diag(xtab)
  N <- sum(xtab)
  row.marginal.props <- rowSums(xtab)/N
  col.marginal.props <- colSums(xtab)/N
  # Compute kappa (k)
  Po <- sum(diagonal.counts)/N
  Pe <- sum(row.marginal.props*col.marginal.props)
  k <- (Po - Pe)/(1 - Pe)
  cat("Accuracy=",accuracy,"%\n")
  cat("Kappa=",k,"\n")
}



cat("Reading..\n")
comparisondf<-read.csv(comparisonfile)

comparisondf$x_01<-round(comparisondf$longitude,1)
comparisondf$y_01<-round(comparisondf$latitude,1)
#treat A data equal to * as 0
comparisondf$A_NA<-comparisondf$A
comparisondf$A_NA[which(comparisondf$A_NA=="*")]<-0

#treat B data equal to * as 0
comparisondf$B_NA<-comparisondf$B
comparisondf$B_NA[which(comparisondf$B_NA=="*")]<-0

cat("Reducing dataset size..\n")
comparisondf$xy_01<-paste0(comparisondf$x_01,";",comparisondf$y_01)
comparisondf_filt2<-comparisondf[,c("x_01","y_01","A_NA","B_NA","xy_01")]
sqldf()
comparisondf_reduced<-sqldf("select x_01,y_01, avg(A_NA) as A, avg(B_NA) as B from comparisondf_filt2 group by xy_01")
sqldf()

#filter marine data only: use depth file as a filter
cat("Excluding land points..\n")
depth_file = "gebco_30sec_8.asc"
asc_file<-raster(depth_file)
grid<-data.frame(x = comparisondf_reduced$x_01, y = comparisondf_reduced$y_01)
depth_values<-raster::extract(x=asc_file,y=grid,method='simple')
comparisondf_reduced<-comparisondf_reduced[which(depth_values<0),]
non_zeroA<-comparisondf_reduced$A[comparisondf_reduced$A>0]
non_zeroB<-comparisondf_reduced$B[comparisondf_reduced$B>0]

if (comparisonType!="quantiles"){
  cat("Using custom thresholds..\n")
  
  threshold1_A<-A_low
  threshold1_B<-B_low
  threshold2_A<-A_high
  threshold2_B<-B_high
  
  cat("A: low<=",threshold1_A,"; high>",threshold2_A,"\n")
  cat("B: low<=",threshold1_B,"; high>",threshold2_B,"\n")
  
  
}else{
  cat("Taking quantiles..\n")
  qt_A_1<-quantile(non_zeroA)[2]
  qt_A_2<-quantile(non_zeroA)[4]
  qt_B_1<-quantile(non_zeroB)[2]
  qt_B_2<-quantile(non_zeroB)[4]
  cat("A: Q2=",qt_A_1,"; Q4=",qt_A_2,"\n")
  cat("B: Q2=",qt_B_1,"; Q4=",qt_B_2,"\n")
  
  comparisondf<-comparisondf_reduced
  
  threshold1_A<-qt_A_1
  threshold1_B<-qt_B_1
  threshold2_A<-qt_A_2
  threshold2_B<-qt_B_2
}

cat("\n")
cat("Calculating the agreement on two categories: Low/non-Low..\n")

A_1_B_1 = comparisondf[which(comparisondf$A>threshold1_A & 
                               comparisondf$B>threshold1_B),]

A_0_B_1 = comparisondf[which(comparisondf$A<=threshold1_A & 
                               comparisondf$B>threshold1_B),]

A_1_B_0 = comparisondf[which(comparisondf$A>threshold1_A & 
                               comparisondf$B<=threshold1_B),]

A_0_B_0 = comparisondf[which(comparisondf$A<=threshold1_A & 
                               comparisondf$B<=threshold1_B),]
calcPerformance(A_1_B_1,A_0_B_1,A_1_B_0,A_0_B_0)

cat("\n")
cat("Calculating the agreement on two categories: High/non-High..\n")

A_1_B_1 = comparisondf[which(comparisondf$A>threshold2_A & 
                               comparisondf$B>threshold2_B),]

A_0_B_1 = comparisondf[which(comparisondf$A<=threshold2_A & 
                               comparisondf$B>threshold2_B),]

A_1_B_0 = comparisondf[which(comparisondf$A>threshold2_A & 
                               comparisondf$B<=threshold2_B),]

A_0_B_0 = comparisondf[which(comparisondf$A<=threshold2_A & 
                               comparisondf$B<=threshold2_B),]
calcPerformance(A_1_B_1,A_0_B_1,A_1_B_0,A_0_B_0)

cat("\n")
cat("Generating visual agreement H vs H=1; L vs L=0; L vs M/H=-1; M/H vs L=-2..\n")

comparisondf_visual<-comparisondf
comparisondf_visual$agreement<-0
comparisondf_visual$agreement[which(comparisondf$A>threshold2_A & 
                                      comparisondf$B>threshold2_B)]<-1

comparisondf_visual$agreement[which(comparisondf$A<=threshold1_A & 
                                      comparisondf$B>threshold1_B)]<--1

comparisondf_visual$agreement[which(comparisondf$A>threshold1_A & 
                                      comparisondf$B<=threshold1_B)]<--2

outfile<-gsub(x = comparisonfile, pattern = "\\.csv", replacement = "_agreement.csv")
write.csv(file=outfile,x = comparisondf_visual,row.names = F)

cat("\n")
cat("Calculating the agreement on three categories: Low/Medium/High..\n")
A_2_B_2 = comparisondf[which(comparisondf$A>threshold2_A & 
                               comparisondf$B>threshold2_B),]
A_0_B_0 = comparisondf[which(comparisondf$A<=threshold1_A & 
                               comparisondf$B<=threshold1_B),]
A_1_B_2 = comparisondf[which(comparisondf$A>threshold1_A & 
                               comparisondf$A<=threshold2_A& 
                               comparisondf$B>threshold2_B),]
A_0_B_2 = comparisondf[which(comparisondf$A<=threshold1_A & 
                               comparisondf$B>threshold2_B),]
A_2_B_1 = comparisondf[which(comparisondf$A>threshold2_A& 
                               comparisondf$B>threshold1_B &
                               comparisondf$B<=threshold2_B ),]
A_1_B_1 = comparisondf[which(comparisondf$A>threshold1_A & 
                               comparisondf$A<=threshold2_A&
                               comparisondf$B>threshold1_B &
                               comparisondf$B<=threshold2_B ),]
A_0_B_1 = comparisondf[which(comparisondf$A<=threshold1_A & 
                               comparisondf$B>threshold1_B &
                               comparisondf$B<=threshold2_B),]
A_2_B_0 = comparisondf[which(comparisondf$A>threshold2_A & 
                               comparisondf$B<=threshold1_B),]
A_1_B_0 = comparisondf[which(comparisondf$A>threshold1_A & 
                               comparisondf$A<=threshold2_A&
                               comparisondf$B<=threshold1_B),]

calcPerformance3<-function(A_2_B_2,A_0_B_0,A_1_B_2,A_0_B_2,A_2_B_1,A_1_B_1,A_0_B_1,A_2_B_0,A_1_B_0){
  accuracy<-(nrow(A_2_B_2)+nrow(A_1_B_1)+nrow(A_0_B_0))*100/nrow(comparisondf)
  
  # Contingency table
  xtab <- as.table(rbind(c(nrow(A_1_B_1),nrow(A_1_B_0)), 
                         c(nrow(A_0_B_1), nrow(A_0_B_0))))
  # Contingency table
  xtab <- as.table(rbind(c(nrow(A_0_B_0),nrow(A_0_B_1),nrow(A_0_B_2)), 
                         c(nrow(A_1_B_0),nrow(A_1_B_1),nrow(A_1_B_2)),
                         c(nrow(A_2_B_0),nrow(A_2_B_1),nrow(A_2_B_2))
  ))
  # Descriptive statistics
  diagonal.counts <- diag(xtab)
  N <- sum(xtab)
  row.marginal.props <- rowSums(xtab)/N
  col.marginal.props <- colSums(xtab)/N
  # Compute kappa (k)
  Po <- sum(diagonal.counts)/N
  Pe <- sum(row.marginal.props*col.marginal.props)
  k <- (Po - Pe)/(1 - Pe)
  
  cat("Accuracy=",accuracy,"%\n")
  cat("Kappa=",k,"\n")
}

calcPerformance3(A_2_B_2,A_0_B_0,A_1_B_2,A_0_B_2,A_2_B_1,A_1_B_1,A_0_B_1,A_2_B_0,A_1_B_0)




