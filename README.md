# Analyst
Statistics, Analysis, Functions

# Analisis Desktriptif (Descriptive Analyst)
```R
descriptive.test <- function(data, col.name = NULL, row.name = NULL){
  data <- data.frame(data)
  N <- MIN<-MAX<-MEAN<-SD<-VAR<-c()
  
  for (j in 1:ncol(data)){N[j]<-length(data[,j]);MIN[j]<-min(data[,j]);MAX[j]<-max(data[,j]);MEAN[j]<-mean(data[,j]);VAR[j]<-var(data[,j]);SD[j]<-sd(data[,j])}
  
  des.data <- data.frame(N, MIN, MAX, MEAN, VAR, SD)
  
  if(is.null(col.name)){colnames(des.data)<-c('N','Minimum', 'Maksimum', 'Rata-rata', 'Variansi', 'Std. Deviasi')}else{colnames(des.data)<-c(col.name)}
  if(is.null(row.name)){rownames(des.data)<-names(data)}else{rownames(des.data)<-c(row.name)}
  return(des.data)
}
```
