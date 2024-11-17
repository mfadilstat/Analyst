# Analyst
Statistics, Analysis, Functions

## Analisis Desktriptif (Descriptive Analyst)
```R
## ########################################################################## ##
## license : DF-Stat
## DF-Code : Descriptive Analysis
## Update  : 2024-11-17
## ########################################################################## ##

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
## Scatter Plot using ggplot2 (install package ggplot2)
```R
## ########################################################################## ##
## License : DF-Stat                                                          ##
## DF-Code : Scatter PLot using ggplot 2                                      ##
## Update  : 2024-11-17                                                       ##
## ########################################################################## ##

scatter.plot <- function(y, x, color = 'black', ylabs = NULL, 
                         xlabs = NULL, title = NULL, subtitle = NULL){
  data.set <- data.frame(x,y)
  ggplot(data.set) + 
    geom_point(aes(x = x1, y = y), color = color) +
    labs(
      title = (if(!is.null(title)){title}),
      subtitle = (if(!is.null(subtitle)){subtitle})
    ) +
    (if(!is.null(ylabs)){ylab(ylabs)})+
    (if(!is.null(xlabs)){xlab(xlabs)})->output
  return(output)
}
```
