
library(dplyr)
library(plyr)
library(readr)



multiplot<-function(d_set,scaling=1){
  'scaling=1 scales the data and scaling=0 uses the raw data'
  d_matt<-as.matrix(d_set)
  d_mat<-matrix(0,length(d_matt[,1]),length(d_matt[1,])-1)
  for (j in 2:length(d_matt[1,])){	
    d_mat[,j-1]<-d_matt[,j]
  }
  d_mat<-unname(d_mat)
  c_dim<-length(d_mat[1,])
  r_dim<-length(d_mat[,1])
  c_mean<-apply(d_mat,2,mean)
  c_sd<-apply(d_mat,2,sd)
  y<-matrix(0,r_dim,c_dim)
  y_neg<-matrix(0,r_dim,c_dim)
  y_pos<-matrix(0,r_dim,c_dim)
  
  
  
  for (i in 1:r_dim){	
    for (j in 1:c_dim){	
      y[i,j]<-(d_mat[i,j]*((d_mat[i,j]-c_mean[j] )))
      
    }	
  }	
  
  for (i in 1:r_dim){	
    for (j in 1:c_dim){	
      if (y[i,j]<0){ y_neg[i,j]<-y[i,j]}
      if (y[i,j]>0){ y_pos[i,j]<-y[i,j]}
    }
  }
  
  yneg<-apply(y_neg,2,sum)
  ypos<-apply(y_pos,2,sum)
  score<-ypos+yneg
  totscore<-ypos/score
  posTotal<-totscore
  negTotal<-1-totscore
  ortneg<- negTotal-posTotal*sum(posTotal*negTotal)/sum(posTotal*posTotal)
  
  st_dmat <-scaled(d_mat,scaling)
  
  Plus_Axis<-st_dmat%*%(posTotal)
  Orth_Minus_Axis<-st_dmat%*%(ortneg)
  cbind(d_matt[,1],Plus_Axis, Orth_Minus_Axis)
  plot(Orth_Minus_Axis,Plus_Axis,pch=d_matt[,1],col=d_matt[,1])
  text(Orth_Minus_Axis,Plus_Axis,labels=d_matt[,1])
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

onlydat<-function(d_set){
  d_matt<-as.matrix(d_set)
  d_mat<-matrix(0,length(d_matt[,1]),length(d_matt[1,])-1)
  for (j in 2:length(d_matt[1,])){	
    d_mat[,j-1]<-d_matt[,j]
  }
  d_mat
}

scaled<-function(d_set,scaling){
  if (scaling == 1) {	
    d_mat<-scale(d_set)
  }
  else
  { d_mat<-d_set}
 
}