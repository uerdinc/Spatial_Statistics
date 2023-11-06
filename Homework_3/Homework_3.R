library(geoR)               #
data("wolfcamp")            # Calling libraries and wolfcamp data

##### Choosing Variogram Model -----

choose_variog<-function(data) {
  cov.models<-c("matern", "exponential", "gaussian", "spherical", 
                "circular", "cubic", "wave", "powered.exponential", 
                "cauchy", "gneiting", "pure.nugget")
  means<-numeric()
  
  vv<-variog(data, trend = "1st", max.dist = 200)
  plot(vv,type="l")
  
  for (i in 1:length(cov.models)){
    vv.fit<-variofit(vv, ini.cov.pars = c(3000,60) ,cov.model = cov.models[i])
    lines(vv.fit, col = i)
    vv.fit.x<-xvalid(data, model = vv.fit)
    means[i]<-mean(vv.fit.x$std.error^2)
    
  }
  
  b_mean<-min(abs(1-means))
  b_mean_index<-which.min(abs(1-means))
  best_cov.model<-cov.models[b_mean_index]
  print(paste("Best cov model is : ", best_cov.model)) # 
  print(paste("Mean is : ", means[b_mean_index]))   # 
  print("All means are ; ")
  means
}

choose_variog(wolfcamp)


