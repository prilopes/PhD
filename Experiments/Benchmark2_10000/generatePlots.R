library(foreach)
library(plyr)

setwd("C:/Users/Priscilla/Dropbox/PhD/Tese/Experimentos/R/Experiments_BenchGen/Benchmark2_10000/measures")

allFiles <- list.files(pattern = "[.]csv", include.dirs = FALSE)
measures <- foreach(file = allFiles, .combine = 'rbind') %do% sub(".csv", "", file)

selected <- list(
  c(3,9,13,16,30,43,56,69,73,79,82,86), #cRand: higher is better
  c(3,9,13,16,30,43,56,69,73,79,82,86), #F1: higher is better
  NULL, #nMac
  NULL, #nMiC
  c(3,9,13,16,30,43,56,69,73,79,82,86), #precision: higher is better
  c(3,9,13,16,30,43,56,69,73,79,82,86), #purity: 0 = bad, 1 = good
  c(3,9,13,16,30,43,56,69,73,79,82,86), #recall: lower is better
  NULL, #silhueta: higher is better
  NULL #SSQ: lower is better
  )
names(selected) <- unlist(measures)

for(i in 1:length(measures)){
  
  measure <- measures[i]
  file <- allFiles[[i]]
  
  df <- read.csv(file)
  
  nn <- nrow(df)
  
  sel <- selected[[i]]
  
  ymin <- floor(min(as.matrix(df[sel,2:6]), na.rm = TRUE))
  ymax <- ceiling(max(as.matrix(df[sel,2:6]), na.rm = TRUE))
  
  #write.csv(x = df[sel,], file = paste(measure,"_selectVal.csv", sep = ""), row.names = FALSE)
  
  if(!is.null(sel)){
  
    pdf(paste(measure,"_selectPlots.pdf", sep = ""), 8 , 7)
    
    layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
    par(mar=c(5,4,4,0)) #No margin on the right side
    matplot(t(as.matrix(df[sel,2:6])), 
            type = "o", 
            xlab = "fase Offline", 
            ylab = measure,
            col = seq_len(nn),
            lty = seq_len(nn), 
            pch = seq_len(nn),
            ylim = c(ymin, ymax))
    par(mar=c(5,0,4,2)) #No margin on the left side
    plot(c(0,1),type="n", axes=F, xlab="", ylab="")
    legend("center", 
           as.character(df[sel,1]),
           col = seq_len(nn),
           lty = seq_len(nn), 
           pch = seq_len(nn))
    dev.off()
  }
}