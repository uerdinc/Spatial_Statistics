library(sp)
library(gstat)
library(geoR)
library(Metrics)
data("wolfcamp")                  # Calling libraries and wolfcamp dataset

#class(wolfcamp)                  # Checking some information about the dataset 
#summary(wolfcamp)                #
#plot(wolfcamp)                   #
#points(wolfcamp)                 
#plot(variog(wolfcamp,option="bin"),type="l")
#plot(variog(wolfcamp,option="bin", trend = "1st"),type="l")

##### Function for best P -----

wolfcamp_df<-data.frame(wolfcamp) # Converting wolfcamp data to dataframe format
coordinates(wolfcamp_df)= ~X1+X2  # Creating coordinates column 

wolfcamp_df$ldata<-log(wolfcamp_df$data) # Calculating log dataset

wolfcam_grid <- expand.grid(x = seq(min(wolfcamp$coords[,1]), max(wolfcamp$coords[,1])), 
                    y = seq(min(wolfcamp$coords[,2]), max(wolfcamp$coords[,2])))          
gridded(wolfcam_grid) = ~x+y      # Creating grid data set for IDW estimation

best_p<- function(k){             # creating function for best p 
  p = 1                           # starting p = 1 
  rmse_idw_wolfcamp<-numeric()     # creating an empty numeric data set for MSE scores
  for (i in 1:k) {
    idw_wolfcamp<- idw(ldata~1, wolfcamp_df, wolfcam_grid, idp = p[i]) # creating IDW model for each p value
    rmse_idw_wolfcamp[i]<-rmse(wolfcamp_df$ldata,idw_wolfcamp$var1.pred) # calculating RMSE scores for each p value
    p[i+1] <- p[i]+0.5          # increasing p value for upcoming IDW model
  }
  
  best_p<-p[which.min(rmse_idw_wolfcamp)] # selecting best p value with minimum MSE 
  print(paste("Best p value:", best_p))  # printing best p value

} # This function can be only use for wolfcamp dataset. 
  # Because it is creating IDW model with using wolfcamp dataset inside the function.
  # The function does create IDW models with using different p values. 
  # The p values starts from 1 and continues k times increasing 0.5 each time. 
  # The k value is the function parameter that user input the function 

best_p(5)  # Trial for function with k = 5 --> This means function check for p values that p = (1, 1.5, 2, 2.5, 3)
best_p(10) # Trial for function with k = 10 --> This means function check for p values that p = (1, ...., 5.5)
## best_p(c) # Trial for function with k = c --> This means function check for p values that p = (1, ...., 0.5+(c/2))

# After run the function --> Best p value is equal to 1 for wolfcamp dataset

##### IDW with 20x20 grid -----

wolfcam_grid_2 <- expand.grid(x = seq(min(wolfcamp$coords[,1]), max(wolfcamp$coords[,1]), length = 20),
                              y = seq(min(wolfcamp$coords[,2]), max(wolfcamp$coords[,2]), length = 20))
gridded(wolfcam_grid_2) = ~x+y            # Creating 20x20 grid dataset for IDW estimation

idw_wolfcamp<- idw(ldata~1, wolfcamp_df, wolfcam_grid_2, idp = 2)   # Implementing IDW model for 20x20 gridded wolfcamp data
rmse_idw_wolfcamp<-rmse(wolfcamp_df$ldata,idw_wolfcamp$var1.pred)  # Calculating RMSE for IDW model

##### Divide data into train and test -----

wolfcamp_df_2<-data.frame(wolfcamp)       # Creating new wolfcamp dataframe dataset 

split1<- sample(c(rep(0, 0.7 * nrow(wolfcamp_df)), rep(1, 0.3 * nrow(wolfcamp_df)))) 
train_wolfcamp<- wolfcamp_df_2[split1 == 0, ]               # 
test_wolfcamp <- wolfcamp_df_2[split1== 1, ]                #  Splitting wolfcamp dataset as train and test datasets

round(table(split1)/length(wolfcamp_df)*100) # Checking the percentage of train and test dataset sizes divided by length of all data

##### MBA -----

library(MBA)                      # Calling MBA library

mba_wolfcamp<-mba.surf(train_wolfcamp, no.X = 500, no.Y = 500,extend = TRUE) # Implementing mba.surf function to our train dataset
image(mba_wolfcamp$xyz.est)

persp(mba_wolfcamp$xyz.est, box = FALSE,phi = -5, theta = 125, ltheta = -125 ,
      expand = 1 ,col = "red", shade = 1.5, border = NA)

rmse_mba_wolfcamp<-rmse(log(mba_wolfcamp$xyz.est$z),log(test_wolfcamp$data)) # Calculating RMSE for MBA model

##### Comparing RMSE values -----

rmse_table<-matrix(c(rmse_idw_wolfcamp,rmse_mba_wolfcamp),ncol=2)  # Creating a table for comparing RMSE values for each model
colnames(rmse_table)<-c("RMSE for IDW","RMSE for MBA")             # Giving column names to RMSE table
rmse_table

# According to our RMSE table, RMSE for IDW model is almost equal to 0.39 and RMSE for MBA model is equal to almost 0.45
# Since RMSE for IDW is lower than the RMSE for MBA, the IDW model is better than MBA model for wolfcamp dataset
















