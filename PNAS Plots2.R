
# this file creates a plot of the binomial distribution (didn't get used)
# and compares it to the distribution of observed temperatures

# it also contains miscelaneus graphs used in the revision for PNAS
rm(list=ls)

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

setwd('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//')




# PNAS revision plots -----------------------------------------------------

    yrs =40 #30,40,50
    mis = 10 #5,10,15
    
    # calculated weighted mean 
    # calculate total area of states
    Filter= paste(yrs,mis,sep='_')     
    
    counties.p = readOGR("Boundary Files", "Counties_equidistant")
    counties.p = counties.p[!(counties.p@data$States ==72), ]   # remove puerto rico no station data or survey data
    
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
    regN1 = lm(happen_P~D_TmaxW_neigh  ,data=counties.p)
    regN1a = lm(happen_P~D_TmaxW_neigh +county_D_TmaxW ,data=counties.p)
    summary(regN1)
    summary(regN1a)
    
    # People are not effected by neighbor values UNWEIGHTED
    counties.p$D_Tmax_neigh = mean_neighbors(values=counties.p$county_D_Tmax ,sweights=Wneigh)
    counties.p$D_TmaxW_neigh = mean_neighbors(values=counties.p$county_D_TmaxW ,sweights=Wneigh)
    
    # Moran's I    -----------------------------------------------------
    
    counties.C_nb = poly2nb(counties.p) # polygon continuity
    centroids = gCentroid(counties.p,byid=TRUE,id = row.names(counties.p))
    counties.K_nb = knn2nb(knearneigh(centroids, k =5) ) # k nearest neighbors 
    
    WKneigh = nb2listw(counties.K_nb)
    Wmix = nb2listw(out)
    
    
    # Roberts suggestion compare difference between actual %belief and country mean belief, to predicted belief - mean belief
    # Roberts suggestion compare difference between actual %belief and country mean belief, to predicted belief - mean belief
    country_belief_mn = mean(counties.p$happen_P,na.rm=T)
    counties.p$happen_P_diff = counties.p$happen_P - country_belief_mn
    
    COL.errW.eig9 <- lagsarlm(happen_P~county_D_TmaxW+I(county_D_TmaxW_Recent *(county_D_Tmax<=163))+ I((county_D_TmaxW_Recent) *(county_D_Tmax>163 & county_D_Tmax<=182))+I((county_D_TminW_Recent) *(county_D_Tmax>182 & county_D_Tmax<=201))+I(county_D_TminW_Recent *(county_D_Tmax>201))  , data=counties.p,
                              WKneigh, method="MC", quiet=F)  # MC also works
    
    # estimate recency effect Bi*variable for all recency variables
    variable_names = names(COL.errW.eig9$coefficients[3:length(COL.errW.eig9$coefficients)])
    Xs = as.data.frame(COL.errW.eig9$X)
    Coefs =  COL.errW.eig9$coefficients
    Rec_eff = Coefs[names(Coefs) %in% variable_names[1] ]*Xs[names(Xs) %in% variable_names[1] ]+
      Coefs[names(Coefs) %in% variable_names[2] ]*Xs[names(Xs) %in% variable_names[2] ]+
      Coefs[names(Coefs) %in% variable_names[3] ]*Xs[names(Xs) %in% variable_names[3] ]+
      Coefs[names(Coefs) %in% variable_names[4] ]*Xs[names(Xs) %in% variable_names[4] ] 
       
      
    dat = data.frame(Fitted.Values=COL.errW.eig9$fitted.values,Actual.Values=COL.errW.eig9$y,Rec_eff=Rec_eff)
    names(dat) = c('Fitted.Values','Actual.Values','Rec_eff')
    dat$Fitted.Values_diff = dat$Fitted.Values - country_belief_mn
    dat2 = (counties.p@data[,c('GEOID10','county_D_TminW_Recent','county_D_Tmax','county_D_TmaxW_Recent','Statename','State'),drop=F])  # not need to drop nas as initially true
    dat2 = dat2[,c('GEOID10','Statename','State')]
    dat = cbind(dat,dat2)
    counties.p@data = join(counties.p@data,dat)
     
    counties.p.out = counties.p[,c('GEOID10','happen_P','county_D_TmaxW','happen_P_diff','Fitted.Values_diff','Rec_eff')]
    names(counties.p.out)=c(c('GEOID10','hap_P','TmaxW','hap_diff','FVal_diff','Rec_eff'))

    writeOGR(counties.p.out, dsn = './PolygonsForGeoda', layer = paste('counties.p2','_',yrs,'_',mis,sep=''), driver = "ESRI Shapefile",overwrite=T)
    
    # read in selection of low low counties from geoda
    LowLow = readOGR("PolygonsForGeoda", "lowlow_select")
    LowLow_join = join(counties.p@data,LowLow@data, by='GEOID10') 
    LowLow_join$SELECT[is.na(LowLow_join$SELECT)]=0
    table(LowLow_join$SELECT)
    aggregate(I(VotingPop/Shape_Area)~SELECT,data=LowLow_join,FUN = mean)

        
    # Regressions requested by reviewers --------------------------------------
    
    summary(lm(happen_P~county_D_Tmax,data = counties.p))
    
    ggplot(data=counties.p@data,aes(y=happen_P,x=county_D_Tmax))+geom_point(alpha=.25)+geom_smooth(method=lm)
    
    
    COL.errW.null.tmax <- lagsarlm(county_D_TmaxW~1     , data=counties.p,
                              WKneigh, method="MC", quiet=F)  # MC also works
    
    summary(COL.errW.null.tmax,Nagelkerke=T)
    
    COL.errW.null.tmax.nb <- lagsarlm(happen_P~ county_D_TmaxW+D_TmaxW_neigh    , data=counties.p,
                                   WKneigh, method="MC", quiet=F)  # MC also works
    
    summary(COL.errW.null.tmax.nb,Nagelkerke=T)
    
    
    COL.errW.eig9 <- lagsarlm(happen_P~county_D_TmaxW+I(county_D_TmaxW_Recent *(county_D_Tmax<=163))+ I((county_D_TmaxW_Recent) *(county_D_Tmax>163 & county_D_Tmax<=182))+I((county_D_TminW_Recent) *(county_D_Tmax>182 & county_D_Tmax<=201))+I(county_D_TminW_Recent *(county_D_Tmax>201))  , data=counties.p,
                              WKneigh, method="MC", quiet=F)  # MC also works
    
    summary(COL.errW.eig9,Nagelkerke=T)
    
    # get fitted vs actual
    dat = data.frame(Fitted.Values=COL.errW.eig9$fitted.values,Actual.Values=COL.errW.eig9$y)
    dat2 = (counties.p@data[,c('county_D_TmaxW','county_D_TminW_Recent','Statename','State'),drop=F]) # this gets the same # obs as the regression
    dat = cbind(dat,dat2)
    BEA_Regions =data.frame(State = c('AK','WA','OR','CA','NV','HI',
                                         'MT','ID','WY','UT','CO',
                                         'AZ','NM','OK','TX',
                                         'ND','SD','NE','KS','MO','IA','MN',
                                         'WI','IL','MI','IN','OH',
                                         'AR','LA','KY','TN','MS','AL','WV','VA','NC','SC','GA','FL',
                                         'MD','DC','DE','NJ','PA','NY',
                                         'ME','NH','VT','MA','RI','CT'),
    
        BEA=c('Far West','Far West','Far West','Far West','Far West','Far West',
              'Rocky Mountain','Rocky Mountain','Rocky Mountain','Rocky Mountain','Rocky Mountain',
              'Southwest','Southwest','Southwest','Southwest',
              'Plains','Plains','Plains','Plains','Plains','Plains','Plains',
              'Great Lakes','Great Lakes','Great Lakes','Great Lakes','Great Lakes',
              'Southeast','Southeast','Southeast','Southeast','Southeast','Southeast','Southeast','Southeast','Southeast','Southeast','Southeast','Southeast',
              'Mideast','Mideast','Mideast','Mideast','Mideast','Mideast',
              'New England','New England','New England','New England','New England','New England'))
        
    dat = join(dat,BEA_Regions)
    
    windows()
    ggplot(data=dat,aes(y=Fitted.Values,x=Actual.Values,colour=BEA))+geom_point(aes(alpha=0.3) )+
        geom_abline(slope=1,intercept = 0,size=1.25,colour='grey40',linetype = "longdash")+
        scale_colour_discrete(name  ="Region")+xlab('Actual Values')+ylab('Fitted Values')
         #+coord_cartesian(xlim =c(44,85), ylim =c(48,85))
    
    windows()
    ggplot() +
      geom_density(data=na.omit(dat), aes(x=county_D_TmaxW, fill=BEA), alpha=.6, position="identity")+
      ylab('Density')+xlab('TMax')+
      theme(axis.text=element_text(size=13),axis.title=element_text(size=14 ))




  
  
  
  # non-spatial regression for comparison
  test <- lm(happen_P~county_D_TmaxW+I(county_D_TmaxW_Recent *(county_D_Tmax<=163))+ I((county_D_TmaxW_Recent) *(county_D_Tmax>163 & county_D_Tmax<=182))+I((county_D_TminW_Recent) *(county_D_Tmax>182 & county_D_Tmax<=201))+I(county_D_TminW_Recent *(county_D_Tmax>201))  , data=counties.p)  # MC also works
  summary(test)
  



# Distribution plots ------------------------------------------------------


  setwd('C://Users/mmann/Google Drive/Climate Change Perception/Mike County Data Files/ClimateChangePerception/')
  dist = read.csv('./Distributions Figure/Distribution_Data.csv')
  head(dist)
  
  
  
  
  for (yrs in c( 40 )){
    for (mis in c( 15 )){
        Filter= paste(yrs,mis,sep='_')    # have been using 40_15
        data = read.dbf(paste('..//Output//Census_Station_',Filter,'.dbf',sep=''), as.is=T)
        #write.dbf(data,paste('Output//Census_Station_',Filter,' - Backup.dbf',sep=''))
        data = data[!(data$State ==72), ]   # remove puerto rico no station data or survey data
    }}
  
  head(data)
  
  # build the binomial distribution
  a=dbinom(1,length(data$D_Tmax),rep(.5,length(data$D_Tmax)))
  summary(a)
  
  windows()
  ggplot() +
    geom_density(data=data, aes(x=D_Tmax, fill=State, alpha=.5), position="identity")
  
  
  windows()
  ggplot() +
    geom_density(data=data, aes(x=D_Tmax, fill=State), alpha=.5, position="identity")+
    ylab('Density')+xlab('TMax')+
    theme(axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold"))
  
  # for paper (didn't get used)
  windows()
  ggplot() +
    geom_histogram(data=data, aes(x=D_Tmax,y=..density.. ),
                   fill="grey10",binwidth =5 )+
    geom_ribbon(aes(x=TMAX,ymin =ChgBinom, ymax = max(dist$ChgBinom)), 
                data =dist[1:183,] , fill = "blue",alpha=0.3)+
    geom_ribbon(aes(x=TMAX,ymin =ChgBinom, ymax = max(dist$ChgBinom)), 
                data =dist[183:365,] , fill = "red",alpha=0.3)+
    geom_line(data=dist,aes(x=TMAX,y=ChgBinom),colour='white',size=1 ) +
    ylab('Density')+xlab('TMax')+
    theme(axis.text=element_text(size=13),axis.title=element_text(size=14 ))
  
  
