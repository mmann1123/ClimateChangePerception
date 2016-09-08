
rm(list=ls())

library(plyr)
library(foreign)
library(stargazer)
library(RANN)
library(rgeos)
library(spdep)
library(car)
library(rgdal)
#library(bigmemory)
library(ggplot2)
library(spgwr)
library(maptools)
library(ggmap)

setwd('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//')

  yrs =40 #30,40,50
  mis = 10 #5,10,15
  
  # calculated weighted mean 
  # calculate total area of states
  Filter= paste(yrs,mis,sep='_')     
  
  counties.p = readOGR("Boundary Files", "Counties_equidistant")
  counties.p = counties.p[!(counties.p@data$States ==72), ]   # remove puerto rico no station data or survey data
  counties.p = spTransform(counties.p,  CRS("+proj=longlat +datum=WGS84"))
  
  # add climate stats 
  climate = read.csv(paste('Regression Intermediates//county_D_Tmax_',Filter,'.csv',sep=''))
  climate = climate[,!(names(climate) %in% c('X'))]
  climate$Countycode=factor(sprintf("%05d", climate$Countycode)) # make county code 5 digits and factor to match
  counties.p@data= join(counties.p@data, climate, type="left")
  
  climate = read.csv(paste('Regression Intermediates//county_D_TmaxW_',Filter,'.csv',sep=''))
  climate = climate[,!(names(climate) %in% c('X'))]
  climate$Countycode=factor(sprintf("%05d", climate$Countycode)) # make county code 5 digits and factor to match
  counties.p@data= join(counties.p@data, climate, type="left")
  
  county_D_TmaxW_Recent= read.csv(paste('Regression Intermediates//county_D_TmaxW_Recent_',Filter,'.csv',sep=''))
  county_D_TmaxW_Recent$Countycode = sprintf("%05d", county_D_TmaxW_Recent$Countycode)   # correct problem with leading zero missing
  county_D_Tmax_Recent = read.csv(paste('Regression Intermediates//county_D_Tmax_Recent_',Filter,'.csv',sep=''))
  county_D_Tmax_Recent$Countycode = sprintf("%05d", county_D_Tmax_Recent$Countycode)
  county_D_TminW_Recent= read.csv(paste('Regression Intermediates//county_D_TminW_Recent_',Filter,'.csv',sep=''))
  county_D_TminW_Recent$Countycode = sprintf("%05d", county_D_TminW_Recent$Countycode)
  county_D_Tmin_Recent = read.csv(paste('Regression Intermediates//county_D_Tmin_Recent_',Filter,'.csv',sep=''))
  county_D_Tmin_Recent$Countycode = sprintf("%05d", county_D_Tmin_Recent$Countycode)
  
  counties.p@data= join(counties.p@data, county_D_TmaxW_Recent, type="left",by='Countycode')
  counties.p@data= join(counties.p@data, county_D_Tmax_Recent, type="left",by='Countycode')
  counties.p@data= join(counties.p@data, county_D_TminW_Recent, type="left",by='Countycode')
  counties.p@data= join(counties.p@data, county_D_Tmin_Recent, type="left",by='Countycode')
  
  # add election results data
  election = read.csv('..//Mike County Data Files//CountyElections//2012 Presidential County Results.csv',stringsAsFactors = F)
  election$Countycode = sprintf("%05d", as.numeric(election$Countycode))
  election$Per_Obama = as.numeric(election$PCT_OBM) # avoid 
  election$Per_Romney = as.numeric(election$PCT_ROM)
  election = election[election$Countycode!="00000",]
  head(election)
  length(unique(election$Countycode))
  counties.p@data = join(counties.p@data,election,type='left',by='Countycode')
  
  
  # Remove all counties with no climate change data
  counties.p = counties.p[!(as.character(counties.p@data$Statecode) %in% c('PR')),]
  counties.C_nb = poly2nb(counties.p,row.names = row.names(counties.p)) # polygon continuity$GEOID10
  centroids = gCentroid(counties.p,byid=TRUE,id = row.names(counties.p))
  counties.C_k  = knearneigh(centroids, k=3)
  counties.C_k  = lapply(1:dim(counties.C_k$nn)[1], function(x) counties.C_k$nn[x,]) # store in list
  
  # replace empty or short nearest neighbor slots with k =3 nearest
  out = lapply(1:length(counties.C_nb), function(x) if(counties.C_nb[[x]][1]==0 |length(counties.C_nb[[x]]) <3 ){ 
    counties.C_k[[x]]}  else{
      counties.C_nb[[x]]
    })
  attributes(out) = attributes(counties.C_nb) # convert to a nb class
  attr(out,"names")<-attr(out,"region.id")  # add a names attribute so you can call by ID
  #out['8860']                               # check to see if still empty
  attributes(out) = attributes(counties.C_nb) # convert to a nb class
  
  # create weights matrix row standardized
  Wneigh = nb2mat(out, style='W')
  #Wneigh 
  
  mean_neighbors <- function(values,sweights){
    # function calculates the mean value of neighbors values by using sweights
    out2=list()
    for(row in 1:length(values)){
      out2=c(out2,sum(values*sweights[row,]))
    }
    return(as.numeric(out2))
  }

  
  # People are effected by neighbor values WEIGHTED
  counties.p$D_TmaxW_neigh = mean_neighbors(values=counties.p$county_D_TmaxW,sweights=Wneigh)
 
  # People are not effected by neighbor values UNWEIGHTED
  counties.p$D_Tmax_neigh = mean_neighbors(values=counties.p$county_D_Tmax ,sweights=Wneigh)
  counties.p$D_TmaxW_neigh = mean_neighbors(values=counties.p$county_D_TmaxW ,sweights=Wneigh)
  
  # Roberts suggestion compare difference between actual %belief and country mean belief, to predicted belief - mean belief
  # Roberts suggestion compare difference between actual %belief and country mean belief, to predicted belief - mean belief
  country_belief_mn = mean(counties.p$happen_P,na.rm=T)
  counties.p$happen_P_diff = counties.p$happen_P - country_belief_mn
 
  # EXLUDE AK and HI from sample messes up GWR bandwidth selection
  counties.p =  counties.p[!(counties.p$Statename %in% c('Hawaii','Alaska')),]
  
  
  # Non spatial regression
  regN1a = lm(happen_P~ county_D_TmaxW +county_D_TmaxW_Recent,data=counties.p)
  summary(regN1a)
  
  resids<-residuals(regN1a)
  colours <- c("dark blue", "blue", "red", "dark red") 
  Centroids = gCentroid(counties.p[!is.na(counties.p$county_D_TmaxW_Recent),],byid=TRUE)
  
  #here it is assumed that your eastings and northings coordinates are stored in columns called x and y in your dataframe
  map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=as.data.frame(Centroids)) 
  #for speed we are just going to use the quick sp plot function, but you could alternatively store your residuals back in your LondonWards dataframe and plot using geom_point in ggplot2
  spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 
  
  # find kernal bandwidth
  GWRbandwidth <- gwr.sel(happen_P~ county_D_TmaxW+county_D_TmaxW_Recent, data=counties.p, coords=as.data.frame(Centroids),adapt=T) 
  
  # estimate model
  gwr.model = gwr(happen_P~ county_D_TmaxW+county_D_TmaxW_Recent, data=counties.p, coords=as.data.frame(Centroids), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
  gwr.model
  
  # add coefficients to data for plotting
  results<-as.data.frame(gwr.model$SDF)
  head(results)
  counties.p$coefcounty_D_TmaxW<-results$county_D_TmaxW
  counties.p$coefcounty_D_TmaxW_Recent<-results$county_D_TmaxW_Recent
  counties.p@data = cbind(counties.p@data,as.data.frame(Centroids))
  counties.l <- fortify(counties.p, region="GEOID10")
  
  # plot polys 
  counties.l$GEOID10 = counties.l$id
  counties.l = join(counties.l,counties.p@data[,c('GEOID10','coefcounty_D_TmaxW'),drop=F],by='GEOID10')
  
  save(list = ls(all.names = TRUE), file = "./Intermediates/GWR_env.RData", envir = .GlobalEnv)
  load("./Intermediates/GWR_env.RData")
  
  windows()
  ggplot(data=counties.p@data, aes(x=x,y=y))+geom_point(aes(colour= coefcounty_D_TmaxW))+
      scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))+ 
    coord_cartesian(xlim = c(-2.5e6,2.5e6), ylim = c(min(counties.p@data$y),1.5e6) )
  
  windows()
  ggplot(data=counties.p@data, aes(x=x,y=y))+geom_point(aes(colour= coefcounty_D_TmaxW))+
    scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))+ 
    coord_cartesian(xlim = c(-2.5e6,2.5e6), ylim = c(min(counties.p@data$y),1.5e6) )
  
  
   ggplot(data=counties.l) + 
    aes(long,lat,group=group,fill=coefcounty_D_TmaxW) + 
    geom_polygon() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))+
    geom_path(color="grey80")+coord_cartesian(xlim = c(-2.5e6,2.5e6), ylim = c(min(counties.p@data$y),1.5e6) )
    
   # background map
   
   windows()
   USA = get_map(location = "USA", zoom = 3, color = "bw", source = "stamen", maptype = "toner-lite")
    plot(USA)
   USAmap = ggmap(USA, base_layer = ggplot(aes(x=x,y=y), data = counties.p@data))
   USAmap  +geom_point(aes(colour= coefcounty_D_TmaxW))+
     scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "rgb", 
                            na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
   windows()  
   USAmap = ggmap(USA, base_layer = ggplot(aes(x=x,y=y), data = counties.p@data))
   USAmap  +geom_point(aes(colour= coefcounty_D_TmaxW_Recent))+
     scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "rgb", 
                            na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
  
  