library(gstat)
library(sp)
library(geoR)

data("wolfcamp")

wolfcamp_df<-data.frame(wolfcamp)
class(wolfcamp_df)

coordinates(wolfcamp_df)= ~X1+X2

aq.grid
gridded(aq.grid) = ~x+y

#gridded(wolfcamp.grid)= ~x+y
spplot(wolfcamp_df["data"])
#class(wolfcamp_df$data)

class(aq.grid)
aq.grid_df<-data.frame(aq.grid)
class(aq.grid_df)
#spplot(aq.grid_df)

wolfcamp_df$ldata<-log(wolfcamp_df$data)
spplot(wolfcamp_df["ldata"])



y<-idw(ldata~1,wolfcamp_df,aq.grid)
y1<-idw(ldata~1,wolfcamp_df,aq.grid,idp=1.5)
y2<-idw(ldata~1,wolfcamp_df,aq.grid,idp=3)


spplot(y["var1.pred"])
spplot(y1["var1.pred"])
spplot(y2["var1.pred"])



y3<-idw(ldata~1,wolfcamp_df,aq.grid,idp=50)
y4<-idw(ldata~1,wolfcamp_df,aq.grid,idp=5)
y5<-idw(ldata~1,wolfcamp_df,aq.grid,idp=10)
View(y3)

spplot(y3["var1.pred"])
spplot(y4["var1.pred"])
spplot(y5["var1.pred"])



library(MBA)
?mba.surf

?mba.points

data(LIDAR)

########################

# Divide data into train and test

wolfcamp_df

split1<- sample(c(rep(0, 0.7 * nrow(wolfcamp_df)), rep(1, 0.3 * nrow(wolfcamp_df))))
train <- wolfcamp_df[split1 == 0, ]    
test <- wolfcamp_df[split1== 1, ]  

table(split1)/length(wolfcamp_df)

---









#multilevel bi-splines 
#library(gstat)
#library(automap)
#wolfcamp_gstat <- gstat(id = "data", formula = data ~ 1, data = wolfcamp_df)
#wolfcamp_kriged <- autoKrige(wolfcamp_gstat, aq.grid)
#spplot(wolfcamp_kriged, "var1.pred")









