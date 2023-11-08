library(geoR)               #
data("wolfcamp")            # Calling libraries and wolfcamp data

##### Choosing Variogram Model -----

choose_variog<-function(data) { # Creating a function for selecting variogram model
  cov.models<-c("matern", "exponential", "gaussian", "spherical", # Inserting cov models into function
                "circular", "cubic", "powered.exponential", 
                "cauchy", "gneiting", "pure.nugget")
  #wave
  means_variofit<-numeric()     # Creating an empty numeric datasets for variofit and likfit functions to keep coefficients
  means_likfit<-numeric()       #
  
  vv<-variog(data, trend = "1st", max.dist = 200) # Creating variogram
  plot(vv,type="l")                               # Plotting variogram 
  
  for (i in 1:length(cov.models)){        # For loop to implement variofit functions and likfit functions
    vv.variofit<-variofit(vv, ini.cov.pars = c(3000,60), cov.model = cov.models[i]) # Implementing variofit for each cov model
    lines(vv.variofit, col = i, lty = 1, lwd = 1.5)     # Add every lines for each variofit functions
    vv.variofit.x<-xvalid(data, model = vv.variofit)    # Cross validation for models
    means_variofit[i]<-mean(vv.variofit.x$std.error^2)  # Keeping all coefficients
    
    vv.likfit<-likfit(wolfcamp,trend="1st", ini.cov.pars = c(3000,60), cov.model = cov.models[i]) # Implementing likfit for each cov model
    lines(vv.likfit, col = i, lty = 2, lwd = 1.5)   # Add every lines for each likfit functions
    vv.likfit.x<-xvalid(wolfcamp,model=vv.likfit)   # Cross validation for models
    means_likfit[i]<-mean(vv.likfit.x$std.error^2)  # Keeping all coefficients   
    
  } # end of for loop
  legend("bottomright",
         legend = c("Variofit models","Likfit models"),
         col = 1,
         lty = c(1,2),
         lwd = 1.5)                                 # Adding legens into plot to identify lines
  
  b_mean_v<-min(abs(1-means_variofit))              # The coefficient that nearest to 1 means better cov model 
  b_mean_index_v<-which.min(abs(1-means_variofit))  # Selecting the best cov model according to variofit 
  best_cov.model_v<-cov.models[b_mean_index_v]      # 
  table.cov_v<-matrix(means_variofit,ncol = length(means_variofit)) # 
  colnames(table.cov_v)<-cov.models                 # Creating table that shows coefficients for each cov model
    
  b_mean_l<-min(abs(1-means_likfit))                # 
  b_mean_index_l<-which.min(abs(1-means_likfit))    # Selecting the best cov model according to variofit
  best_cov.model_l<-cov.models[b_mean_index_l]      # 
  table.cov_l<-matrix(means_likfit,ncol = length(means_likfit)) # 
  colnames(table.cov_l)<-cov.models                 # Creating table that shows coefficients for each cov model
  
  table_means<-matrix(c(cov.models,table.cov_v,table.cov_l), nrow=length(cov.models))
  colnames(table_means)<-c("Cov.Model","Variofit Mean","Likfit Mean") # Putting together both tables
  
  print(table_means)   # 
  print(paste("Best cov model according to variofit is : ", best_cov.model_v))  # Printing best model of variofit
  print(paste("Mean of the model is : ", means_variofit[b_mean_index_v]))       # 
  print(paste("Best cov model according to likfit is : ", best_cov.model_l))    # Printing best model of likfit
  print(paste("Mean of the model is : ", means_likfit[b_mean_index_l]))         # 
  
} # end of function

choose_variog(wolfcamp) # Calling the function
# The table shows all variances that calculating with variofit and likfit for each cov.model 
# Best cov.model is "gaussian" (1.0788) for variofit function
# Best cov.model is "powered.exponential" (1.0216) for likfit function
# Comparing for variofit and likfit --> Since the variance of best likfit model is lower than best variofit model,
# likfit model is better with cov.model = "powered.exponential" 

##### Spatial Interpolation -----

vv.likfit<-likfit(wolfcamp,trend="1st", ini.cov.pars = c(3000,60), cov.model = "powered.exponential") 
# Implementing likfit estimation using with cov.model = powered.exponential 

#vv.likfit.x<-xvalid(wolfcamp,model=vv.likfit) #
#mean(vv.likfit.x$std.error^2)                 # For checking coefficient of cross validation again

x<-seq(min(wolfcamp$coord[,1]), max(wolfcamp$coord[,1]))
y<-seq(min(wolfcamp$coord[,2]), max(wolfcamp$coord[,2]))
grid<-expand.grid(x,y)                                    # Creating grid for kriging

krige_info<-krige.control(trend.d="1st", trend.l="1st", obj.model=vv.likfit, 
                  cov.model="powered.exponential", cov.pars=c(3000,60), kappa = 2) # Creating krige.control 
krige.model<-krige.conv(wolfcamp, locations = grid, krige = krige_info) # Krige model using krige.control info

#names(krige.model)
mean(krige.model$krige.var^2) # Coefficient of krige model








