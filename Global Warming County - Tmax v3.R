
#install.packages('plyr')
#install.packages('foreign')
#install.packages('stargazer')
#install.packages('RANN')
#install.packages('rgeos')
rm(list=ls())

library(plyr)
library(foreign)
library(stargazer)
library(RANN)
library(rgeos)
library(spdep)
library(car)
library(rgdal)
library(spdep)
#library(bigmemory)


setwd('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//')

# steps  done in python Data Setup.py


# Non_spatial regressions -------------------------------------------------


for (yrs in c(30,40,50)){
    for (mis in c(5,10,15)){
        
        # calculated weighted mean 
        # calculate total area of states
        Filter= paste(yrs,mis,sep='_')    # have been using 40_15
        
        data = read.dbf(paste('Output//Census_Station_',Filter,'.dbf',sep=''), as.is=T)
        #write.dbf(data,paste('Output//Census_Station_',Filter,' - Backup.dbf',sep=''))
        data = data[!(data$State ==72), ]   # remove puerto rico no station data or survey data
        
        recent = read.csv('..//International//after2005.csv')
        names(recent)[1]="StationID"
        data = join(data,recent,type='left')
        
        # add election results data
        election = read.csv('..//Mike County Data Files//CountyElections//2012 Presidential County Results.csv',stringsAsFactors = F)
        election$Countycode = sprintf("%05d", as.numeric(election$Countycode))
        election$Per_Obama = as.numeric(election$PCT_OBM) # avoid 
        election$Per_Romney = as.numeric(election$PCT_ROM)
        election = election[election$Countycode!="00000",]
        head(election)
        length(unique(election$Countycode))
        data = join(data,election,type='left',by='Countycode')
       
    
        databackup = data
        #data = databackup 
        
        #convert to from m^2 to km^2
        data$F_AREAKM =  data$F_AREA / 1000000
         
        # Station weighted total population (redistribute people to union with weather station theissen polygons)
        census_area = aggregate(F_AREAKM~GEOID10, data=data, FUN=sum ) # area of census tracts
        names(census_area) = c("GEOID10","C_AREAKM")
        data = join(data, census_area, type="left", by='GEOID10')
        data$Cpercent = data$F_AREAKM / data$C_AREAKM # percentage of tract nearest station
        aggregate(Cpercent~GEOID10,data=data,FUN=sum) # should be all 1s 
        data$CVotingPop = data$VotingPop * data$Cpercent # allocation people base on percentage of feature as % of census tract area
        
        # County weighted mean
        # County population 
        county_pop = aggregate(VotingPop~Countycode,data=data,FUN=sum, na.omit=T)  # county voting population
        names(county_pop) = c("Countycode","county_pop")
        data= join(data, county_pop, type="left", by="Countycode")
        head(data)
        
        # calc % of county population for each feature polygon
        data$percent = data$CVotingPop / data$county_pop
        aggregate(percent~Countycode,data=data,FUN=sum)  # should be close to 1
        head(data)
        
        # calculated weighted mean
        county_D_TmaxW = ddply(data,.(Countycode),summarise, wm = weighted.mean(D_Tmax,percent))
        names(county_D_TmaxW)= c('Countycode','county_D_TmaxW')
        county_D_TmaxW[county_D_TmaxW$State ==72,"county_D_TmaxW"] = NA # remove puerto rico no station data
        write.csv(county_D_TmaxW,paste('Regression Intermediates//county_D_TmaxW_',Filter,'.csv',sep=''))
                    
        
        # calculate unweighted mean (assigned to nearest station regardless of state boundary by thessian poly)
        # CONVERT THIS SO IT CREATES MEAN FOR THE COUNTY
        county_D_Tmax = aggregate( D_Tmax ~ Countycode,data=data,FUN=mean) 
        names(county_D_Tmax)= c('Countycode','county_D_Tmax')
        county_D_Tmax[county_D_Tmax$State ==72,"county_D_Tmax"] = NA # remove puerto rico no station data
        write.csv(county_D_Tmax,paste('Regression Intermediates//county_D_Tmax_',Filter,'.csv',sep=''))
        
        # calculate unweighted Stnd Dev
        county_D_Tmax_SD = aggregate( D_Tmax ~ Countycode,data=data,FUN=sd) 
        names(county_D_Tmax_SD)= c('Countycode','county_D_Tmax_SD')
        county_D_Tmax_SD[county_D_Tmax_SD$State ==72,"county_D_Tmax_SD"] = NA # remove puerto rico no station data
        head(county_D_Tmax_SD)
        
        # Calculate stats for 2008-2013 period 
        # MAX calculated weighted mean for RECENT DATA 2008-2013
        county_D_TmaxW_Recent = ddply(data,.(Countycode),summarise, wm = weighted.mean(TMAXNEW,percent))
        names(county_D_TmaxW_Recent)= c('Countycode','county_D_TmaxW_Recent')
        county_D_TmaxW_Recent[county_D_TmaxW_Recent$State ==72,"county_D_TmaxW_Recent"] = NA # remove puerto rico no station data
        write.csv(county_D_TmaxW_Recent,paste('Regression Intermediates//county_D_TmaxW_Recent_',Filter,'.csv',sep=''))
        
        
        # MAX calculate unweighted mean for RECENT DATA 2005-2013 (assigned to nearest station regardless of state boundary by thessian poly)
        # CONVERT THIS SO IT CREATES MEAN FOR THE COUNTY
        county_D_Tmax_Recent = aggregate( TMAXNEW ~ Countycode,data=data,FUN=mean) 
        names(county_D_Tmax_Recent)= c('Countycode','county_D_Tmax_Recent')
        county_D_Tmax_Recent[county_D_Tmax_Recent$State ==72,"county_D_Tmax_Recent"] = NA # remove puerto rico no station data
        write.csv(county_D_Tmax_Recent,paste('Regression Intermediates//county_D_Tmax_Recent_',Filter,'.csv',sep=''))
        
        # MIN calculated weighted mean for RECENT DATA 2005-2013
        county_D_TminW_Recent = ddply(data,.(Countycode),summarise, wm = weighted.mean(TMINNEW,percent))
        names(county_D_TminW_Recent)= c('Countycode','county_D_TminW_Recent')
        county_D_TminW_Recent[county_D_TminW_Recent$State ==72,"county_D_TminW_Recent"] = NA # remove puerto rico no station data
        write.csv(county_D_TminW_Recent,paste('Regression Intermediates//county_D_TminW_Recent_',Filter,'.csv',sep=''))
        
        # MIN calculate unweighted mean for RECENT DATA 2005-2013 (assigned to nearest station regardless of state boundary by thessian poly)
        # CONVERT THIS SO IT CREATES MEAN FOR THE COUNTY
        county_D_Tmin_Recent = aggregate( TMINNEW ~ Countycode,data=data,FUN=mean) 
        names(county_D_Tmin_Recent)= c('Countycode','county_D_Tmin_Recent')
        county_D_Tmin_Recent[county_D_Tmin_Recent$State ==72,"county_D_Tmin_Recent"] = NA # remove puerto rico no station data
        write.csv(county_D_Tmin_Recent,paste('Regression Intermediates//county_D_Tmin_Recent_',Filter,'.csv',sep=''))
        
        
        # Join to polygons
        data= join(data, county_D_TmaxW, type="left")
        data= join(data, county_D_Tmax, type="left")
        data= join(data, county_D_TmaxW_Recent, type="left")
        data= join(data, county_D_Tmax_Recent, type="left")
        data= join(data, county_D_TminW_Recent, type="left")
        data= join(data, county_D_Tmin_Recent, type="left")
        data= join(data, county_D_Tmax_SD, type="left")
        
        head(data)
        #write.dbf(data, paste('Output//Census_Station_',Filter,'.dbf',sep=''))      # was 'Census_Station_Clip3_Area.dbf'
        
        
        # Limit regression to unique Counties 
        data_regression = data[,c('Lat','Lon','States','Countycode','Countyname','county_D_TmaxW','county_D_Tmax','county_D_TmaxW_Recent','county_D_Tmax_Recent','county_D_TminW_Recent','county_D_Tmin_Recent','county_D_Tmax_SD','happen_Pop','happen_P','happenOpp_','hapOp_P','human_Pop','human_P','humanOp_Po','humanOp_P','Per_Obama')]
        data_regression = unique(data_regression)
        
        
        # regressions
        
        reg1 = lm(happen_P~county_D_TmaxW  ,data=data_regression)
        summary(reg1)
#         windows()
#         plot(reg1$fitted.values,reg1$model[,1],main='happen_P~county_D_TmaxW')
                 
        reg2 = lm(happen_P~county_D_Tmax ,data=data_regression)
        summary(reg2)
        
        reg3= lm(happen_P~county_D_TmaxW+county_D_Tmax ,data=data_regression)
        summary(reg3)
        

        
#         reg4 = lm(happen_P~county_D_Tmax+county_D_Tmax_SD ,data=data_regression)
#         summary(reg4)
#         
#         reg5= lm(happen_P~county_D_Tmax+I(county_D_Tmax^2) ,data=data_regression)
#         summary(reg5)
        
#         reg6= lm(happen_P~county_D_TmaxW+county_D_TmaxW_Recent+I(county_D_TmaxW_Recent-county_D_TminW_Recent) ,data=data_regression)
#         summary(reg6)
        
        reg9= lm(happen_P~county_D_TmaxW+I(county_D_TmaxW_Recent *(county_D_Tmax<=163))+ I((county_D_TmaxW_Recent) *(county_D_Tmax>163 & county_D_Tmax<=182))+I((county_D_TminW_Recent) *(county_D_Tmax>182 & county_D_Tmax<=201))+I(county_D_TminW_Recent *(county_D_Tmax>201))  ,data=data_regression)
        summary(reg9)        
        
stargazer(reg9,  
          title=paste("Regression Results: Min Years =",substr(Filter,1,2),'Max Missing = ',substr(Filter,4,5)), 
          align=TRUE, 
          dep.var.labels=c("% Believe Happening"), 
          covariate.labels=c("TmaxW","TmaxW_Recent, TmaxW<=163","TmaxW_Recent, 163<TmaxW<=182","TminW_Recent, 182<TmaxW<=201","TminW_Recent, TmaxW>201"),  
          no.space=TRUE, 
          omit.stat=c("LL","ser","f", "rsq"),
          column.labels=c(sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter)), 
          dep.var.caption="", 
          model.numbers=T, 
          type = "text", out = paste("Regression Output//reg_results_recent",Filter,".txt")
          )


    regElections = lm(logit(Per_Obama)~county_D_TmaxW  ,data=data_regression)
    summary(regElections)
    
    stargazer(regElections,  
              title=paste("Regression Results Election: Min Years =",substr(Filter,1,2),'Max Missing = ',substr(Filter,4,5)), 
              align=TRUE, 
              dep.var.labels=c("% Vote 4 Obama"), 
              covariate.labels=c("TmaxW"),  
              no.space=TRUE, 
              omit.stat=c("LL","ser","f", "rsq"),
              column.labels=c(sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter)), 
              dep.var.caption="", 
              model.numbers=T, 
              type = "text", out = paste("Regression Output//reg_results_election",Filter,".txt")
    )
    

    reg10_elec= lm(logit(Per_Obama)~county_D_TmaxW ,data=data_regression)


    # check robustness, logit transform http://stats.stackexchange.com/questions/48485/what-is-the-difference-between-logit-transformed-linear-regression-logistic-reg
    reg10= lm(logit(happen_P)~county_D_TmaxW+I(county_D_TmaxW_Recent *(county_D_Tmax<=163))+ I((county_D_TmaxW_Recent) *(county_D_Tmax>163 & county_D_Tmax<=182))+I((county_D_TminW_Recent) *(county_D_Tmax>182 & county_D_Tmax<=201))+I(county_D_TminW_Recent *(county_D_Tmax>201))  ,data=data_regression)
    summary(reg10)        
    
    stargazer(reg10,  
              title=paste("Regression Logit Transform Results: Min Years =",substr(Filter,1,2),'Max Missing = ',substr(Filter,4,5)), 
              align=TRUE, 
              dep.var.labels=c("% Believe Happening"), 
              covariate.labels=c("TmaxW","TmaxW_Recent, TmaxW<=163","TmaxW_Recent, 163<TmaxW<=182","TminW_Recent, 182<TmaxW<=201","TminW_Recent, TmaxW>201"),  
              no.space=TRUE, 
              omit.stat=c("LL","ser","f", "rsq"),
              column.labels=c(sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter)), 
              dep.var.caption="", 
              model.numbers=T, 
              type = "text", out = paste("Regression Output//reg_results_recent_logit_trans",Filter,".txt")
    )

        stargazer(reg1, reg2,reg3,
                   title=paste("Regression Results: Min Years =",substr(Filter,1,2),'Max Missing = ',substr(Filter,4,5)), 
                  align=TRUE, 
                  dep.var.labels=c("% Believe Happening"), 
                  covariate.labels=c("Weighted Tmax","Unweighted Tmax"),  
                  no.space=TRUE, 
                  omit.stat=c("LL","ser","f", "rsq"),
                  column.labels=c(sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter)), 
                  dep.var.caption="", 
                  model.numbers=T, 
                  type = "text", out = paste("Regression Output//reg_results",Filter,".txt"))

    }
}


#linearHypothesis(reg4, c("state_D_TmaxW", "I(state_D_TmaxW^2)"), c(0,0))


# # export residuals 
# out = data.frame(model.frame(reg1),model.frame(reg2),resid_D_TmaxW=residuals(reg1),resid_D_Tmax=residuals(reg2))
# out = join(out,states)
# write.csv(out,'Final Outputs//resid.csv')           


# Spatial Regressions -----------------------------------------------------
# test wether or not people are affected by out of state temps

  
setwd('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//')

for (yrs in c(30,40,50)){
    for (mis in c(5,10,15)){
        
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
        county_D_Tmax_Recent = read.csv(paste('Regression Intermediates//county_D_Tmax_Recent_',Filter,'.csv',sep=''))
        county_D_TminW_Recent= read.csv(paste('Regression Intermediates//county_D_TminW_Recent_',Filter,'.csv',sep=''))
        county_D_Tmin_Recent = read.csv(paste('Regression Intermediates//county_D_Tmin_Recent_',Filter,'.csv',sep=''))

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
        regN2 = lm(happen_P~D_Tmax_neigh    ,data=counties.p)
        regN2a = lm(happen_P~D_Tmax_neigh + county_D_Tmax  ,data=counties.p)
        summary(regN2)
        summary(regN2a)
        
        stargazer(regN1, regN1a,regN2,regN2a,
                  title=paste("Regression Results: Min Years =",substr(Filter,1,2),'Max Missing = ',substr(Filter,4,5)), 
                  align=TRUE, 
                  dep.var.labels=c("% Believe Happening"), 
                  #covariate.labels=c("D_TmaxW_neigh","county_D_TmaxW","D_Tmax_neigh","county_D_Tmax"),  
                  no.space=TRUE, 
                  omit.stat=c("LL","ser","f", "rsq"),
                  column.labels=c(sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter)), 
                  dep.var.caption="", 
                  model.numbers=T, 
                  type = "text", out = paste("Regression Output//reg_neigh_results",Filter,".txt"))
    
    
    # Moran's I    -----------------------------------------------------
        
        counties.C_nb = poly2nb(counties.p) # polygon continuity
        centroids = gCentroid(counties.p,byid=TRUE,id = row.names(counties.p))
        counties.K_nb = knn2nb(knearneigh(centroids, k =5) ) # k nearest neighbors 
        
        WKneigh = nb2listw(counties.K_nb)
        Wmix = nb2listw(out)
        
        # test for spatial autocorrelation of error terms
        reg1 = lm(happen_P~county_D_TmaxW  , data=counties.p)
        summary(reg1)
        #plot(reg1$fitted.values, reg1$model[,1],labels=reg1$model)
        moran1mix    = lm.morantest(reg1,Wmix,alternative="two.sided")
        moran2kneigh = lm.morantest(reg1,WKneigh,alternative="two.sided")
        #  reject null of random distribution   
    
        # add post 2008 data 
        reg9= lm(happen_P~county_D_TmaxW+I(county_D_TmaxW_Recent *(county_D_Tmax<=163))+ I((county_D_TmaxW_Recent) *(county_D_Tmax>163 & county_D_Tmax<=182))+I((county_D_TminW_Recent) *(county_D_Tmax>182 & county_D_Tmax<=201))+I(county_D_TminW_Recent *(county_D_Tmax>201))   ,data=counties.p)
        summary(reg9) 
       # moran9mix    = lm.morantest(reg9,Wmix,alternative="two.sided")
        moran9kneigh = lm.morantest(reg9,WKneigh,alternative="two.sided")
            
    # write to files
        fileConn<-file(paste('Regression Output//MoransI-Mixed Weights',Filter,"-output.txt",sep=""))
        writeLines(c(paste(Filter,moran1mix$method,moran1mix$alternative, sep=' - '),paste('Statistic: ',moran1mix$statistic),
                     paste('P.Value: ',moran1mix$p.value), paste('estimate: ' ,moran1mix$estimate[1]) ), fileConn)
        close(fileConn)
        
        fileConn<-file(paste('Regression Output//MoransI-Kneigh',Filter,"-output.txt",sep=""))
        writeLines(c(paste(Filter,moran2kneigh$method,moran2kneigh$alternative, sep=' - '),paste('Statistic: ',moran2kneigh$statistic),
                     paste('P.Value: ',moran2kneigh$p.value), paste('estimate: ' ,moran2kneigh$estimate[1]) ), fileConn)
        close(fileConn)
    
        fileConn<-file(paste('Regression Output//MoransI-Kneigh_Reg9',Filter,"-output.txt",sep=""))
        writeLines(c(paste(Filter,moran9kneigh$method,moran9kneigh$alternative, sep=' - '),paste('Statistic: ',moran9kneigh$statistic),
                     paste('P.Value: ',moran9kneigh$p.value), paste('estimate: ' ,moran9kneigh$estimate[1]) ), fileConn)
        close(fileConn)
    
    # Lagrange Multiplier Test for spatial lag vs error -------------------------------
    # https://geodacenter.asu.edu/drupal_files/spdepintro.pdf
    
    lagrange = lm.LMtests(lm(happen_P~county_D_TmaxW,data = counties.p),WKneigh, test=c("LMerr","RLMerr","LMlag","RLMlag"))
    print(lagrange)
    
    fileConn<-file(paste('Regression Output//Lagrange-Kneigh',Filter,"-output.txt",sep=""))
    writeLines(c(paste(Filter, sep=' - '),paste('LMerr: ',lagrange$LMerr)
                 ,paste('RLMerr: ',lagrange$RLMerr)
                 ,paste('LMlag: ',lagrange$LMlag)
                 ,paste('RLMlag: ',lagrange$RLMlag)), fileConn)
    close(fileConn)
    
    # Spatial error regression    ---------------------------------------------
    
        COL.errW.eig1 <- errorsarlm(happen_P~county_D_Tmax  , data=counties.p,
                                   WKneigh, method="MC", quiet=FALSE)  # MC also works
        
        summary(COL.errW.eig1, correlation=F)
        
        COL.errW.eig2 <- errorsarlm(happen_P~county_D_TmaxW  , data=counties.p,
                                   WKneigh, method="MC", quiet=FALSE) #eigen
    
        summary(COL.errW.eig2, correlation=F)
        
        
        stargazer(COL.errW.eig1, COL.errW.eig2,
                  title=paste("Spatial Simultaneous Autoregressive Error Model Results: Min Years =",substr(Filter,1,2),'Max Missing = ',substr(Filter,4,5)), 
                  align=TRUE, 
                  dep.var.labels=c("% Believe Happening"), 
                  covariate.labels=c("Weighted Tmax","Unweighted Tmax"),  
                  no.space=TRUE, 
                  omit.stat=c("LL","ser","f", "rsq"),
                  column.labels=c(sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter)), 
                  dep.var.caption="", 
                  model.numbers=T, 
                  type = "text", out = paste("Regression Output//Spatial_reg_results",Filter,".txt"))
     
        
        
        lagrange9 = lm.LMtests( lm(happen_P~county_D_TmaxW+I(county_D_TmaxW_Recent *(county_D_Tmax<=163))+ I((county_D_TmaxW_Recent) *(county_D_Tmax>163 & county_D_Tmax<=182))+I((county_D_TminW_Recent) *(county_D_Tmax>182 & county_D_Tmax<=201))+I(county_D_TminW_Recent *(county_D_Tmax>201)) ,data = counties.p),WKneigh, test=c("LMerr","RLMerr","LMlag","RLMlag"))
        print(lagrange9) # null => alpha = 0  in alphaWe  or null => beta = 0  in betaWy
        
        fileConn<-file(paste('Regression Output//Lagrange-Kneigh-Recent',Filter,"-output.txt",sep=""))
        writeLines(c(paste(Filter, sep=' - '),paste('LMerr: ',lagrange9$LMerr)
                     ,paste('RLMerr: ',lagrange9$RLMerr)
                     ,paste('LMlag: ',lagrange9$LMlag)
                     ,paste('RLMlag: ',lagrange9$RLMlag)), fileConn)
        close(fileConn)
          
        
        COL.errW.eig9 <- lagsarlm(happen_P~county_D_TmaxW+I(county_D_TmaxW_Recent *(county_D_Tmax<=163))+ I((county_D_TmaxW_Recent) *(county_D_Tmax>163 & county_D_Tmax<=182))+I((county_D_TminW_Recent) *(county_D_Tmax>182 & county_D_Tmax<=201))+I(county_D_TminW_Recent *(county_D_Tmax>201))  , data=counties.p,
                                    WKneigh, method="MC", quiet=F)  # MC also works
    
        summary(COL.errW.eig9)
        
        
        stargazer(COL.errW.eig9,  
                  title=paste("Regression Results: Min Years =",substr(Filter,1,2),'Max Missing = ',substr(Filter,4,5)), 
                  align=TRUE, 
                  dep.var.labels=c("% Believe Happening"), 
                  covariate.labels=c("TmaxW","TmaxW_Recent, TmaxW<=163","TmaxW_Recent, 163<TmaxW<=182","TminW_Recent, 182<TmaxW<=201","TminW_Recent, TmaxW>201"),  
                  no.space=TRUE, 
                  omit.stat=c("LL","ser","f", "rsq"),
                  column.labels=c(sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter)), 
                  dep.var.caption="", 
                  model.numbers=T, 
                  type = "text", out = paste("Regression Output//Spatial_reg_results_recent",Filter,".txt")
        )
        
        
        regElections = lagsarlm(logit(Per_Obama)~county_D_TmaxW  ,data=counties.p, WKneigh, method="MC", quiet=F)
        summary(regElections)
        
        stargazer(regElections,  
                  title=paste("Spatial Regression Results Election: Min Years =",substr(Filter,1,2),'Max Missing = ',substr(Filter,4,5)), 
                  align=TRUE, 
                  dep.var.labels=c("% Vote 4 Obama - logit transform"), 
                  covariate.labels=c("TmaxW"),  
                  no.space=TRUE, 
                  omit.stat=c("LL","ser","f", "rsq"),
                  column.labels=c(sub('_','-',Filter),sub('_','-',Filter),sub('_','-',Filter)), 
                  dep.var.caption="", 
                  model.numbers=T, 
                  type = "text", out = paste("Regression Output//reg_results_election_spatial",Filter,".txt")
        )
        
            
    }
}


 




# Spatial Sampling Distance Decay Plots--------------------------------------------------------
    library(ncf)  # http://www.r-bloggers.com/spatial-correlograms-in-r-a-mini-overview/
    library(ggplot2)
    library(rgdal)
    library(foreign)
    #rm(list=ls())

    setwd('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//')

    # random sample 
    station = read.dbf('Station Data//Stations_equidistant.dbf')
    set.seed(10)
    station_sm=station[sample(dim(station)[1], 5000),]
        
    # transform to meters
    station_sm2 =station_sm
    coordinates(station_sm2) = c("Lon", "Lat")
    proj4string(station_sm2) = CRS("+proj=longlat +datum=WGS84")  ## for example
    res = spTransform(station_sm2, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "))
    head(res)
    station_sm$X = coordinates(res)[,1]
    station_sm$Y = coordinates(res)[,2]

    # create spatial correlagram 
    ncf.cor = correlog(x=station_sm$X,y=station_sm$Y,z=station_sm$D_Tmax,increment=5000, resamp=300,latlon=F,na.rm=T)
    
    save(ncf.cor,file='C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Station Data//correlogram.RData' ,version = NULL)
#     rm(list = ls())
#     load('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Station Data//correlogram.RData' )

    plotdata = data.frame(Value = c(ncf.cor$correlation, ncf.cor$p),Distance=rep(ncf.cor$mean.of.class,2),Index = c(rep('Correlation',length(ncf.cor$p)),rep('PValue',length(ncf.cor$p))))
    
    windows()
    ggplot(plotdata,aes(x=Distance,y=Value,colour=Index))+stat_smooth()
    windows()
    ggplot(plotdata,aes(x=Distance,y=Value,colour=Index))+stat_smooth()+coord_cartesian(xlim=c(0,2e6))+ylab('Moran\'s I Value')+xlab('Distance (m)')


    
    
    


# Spatial Sampling Distance Decay Plots RESIDUAL --------------------------------------------------------
    library(ncf)  # http://www.r-bloggers.com/spatial-correlograms-in-r-a-mini-overview/
    library(ggplot2)
    library(rgdal)
    library(foreign)
    #rm(list=ls())
    
    setwd('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//')
    
    # random sample 
    station = read.dbf(paste('Output//Census_Station_',Filter,'.dbf',sep=''), as.is=T)
    station = station[!(station$State ==72), ]   # remove puerto rico no station data or survey data
    station = station[!is.na(station$Lat),] # remove missing
    set.seed(10)
    station_sm=station[sample(dim(station)[1], 5000),]
    
    # transform to meters
    station_sm2 =station_sm 
    coordinates(station_sm2) = c("Lon", "Lat")
    proj4string(station_sm2) = CRS("+proj=longlat +datum=WGS84")  ## for example
    res = spTransform(station_sm2, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "))
    head(res)
    station_sm$X = coordinates(res)[,1]
    station_sm$Y = coordinates(res)[,2]
    
    lm1 = lm(human_P~D_Tmax,data=station_sm)
    
    # create spatial correlagram 
    ncf.cor = correlog(x=station_sm$X,y=station_sm$Y,z=lm1$residuals,increment=5000, resamp=300,latlon=F,na.rm=T)
    
    save(ncf.cor,file='C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Station Data//correlogram_residual.RData' ,version = NULL)
    #     rm(list = ls())
    #     load('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Station Data//correlogram_residual.RData' )
    
    plotdata = data.frame(Value = c(ncf.cor$correlation, ncf.cor$p),Distance=rep(ncf.cor$mean.of.class,2),Index = c(rep('Correlation',length(ncf.cor$p)),rep('PValue',length(ncf.cor$p))))
    
    windows()
    ggplot(plotdata,aes(x=Distance,y=Value,colour=Index))+stat_smooth()
    windows()
    ggplot(plotdata,aes(x=Distance,y=Value,colour=Index))+stat_smooth()+coord_cartesian(xlim=c(0,2e6))+ylab('Moran\'s I Value')+xlab('Distance (m)')+ geom_hline(yintercept=0.1)
    

# Spatial Subsampling with Minimum Distance -------------------------------
library(rgdal)
source('G://Faculty//Mann//Scripts//py.CreateRandomPoints_management.R')

out_path ='C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Station Data'
out_name = 'RandomStations'
constraining_feature_class ='C:\\Users\\mmann\\Google Drive\\Climate Change Perception\\Mike County Data Files\\Station Data\\Stations_equidistant.shp'
constraining_extent = 'C:\\Users\\mmann\\Google Drive\\Climate Change Perception\\Mike County Data Files\\Station Data\\Stations_equidistant.shp'
number_of_points_or_field = 1000
minimum_allowed_distance = '500 Kilometers'
random_seed =5  # allow for replicability
py.CreateRandomPoints_management(out_path, out_name, constraining_feature_class, constraining_extent, number_of_points_or_field,  minimum_allowed_distance, "POINT", "0",random_seed)


# read in station data 
if(!exists('stations')){stations = readOGR('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Station Data','Stations_equidistant')}
stations_sm = readOGR('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Station Data','RandomStations')
data_sm = stations_sm %over% stations
 


    # Reproject to meters
    coordinates(station) = c("Lon", "Lat")
    proj4string(station) = CRS("+proj=longlat +datum=WGS84")  ## for example
    station = spTransform(station, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "))
    head(station)
    dnlist = dnearneigh(coordinates(station),0,6e5)
    save(dnlist,file='C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Regression Intermediates//dnlist.RData' )
    load('C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Regression Intermediates//dnlist.RData'  )

#   dlist  = nbdists(dnlist, coordinates(station))

    # narrow down potential sample 
    potentials = 1:dim(station@data)[1]

    # select row 1 as first point
    selected  = 1 
    potentials=potentials[!(potentials %in% dnlist[[selected]])]
    #next_potential = min(potentials[[max(selected)+1]])

    potentials=potentials[!(potentials %in% dnlist[[next_potential]])]
    next_potential = min(potentials[max(selected)+1])

    all = 1:length(potentials)
    potentials = as.list(rep(F,length(dnlist)))
    potentials[[1]]=T
    selected=all[unlist(potentials)] 
    dnlist2 = lapply(1:length(dnlist),function(x) if(sum(dnlist[[x]] %in% selected)>0){
        return(NA)}else{
            potentials[[x]]=T
            return(dnlist[[x]])} )

    min(unlist(dnlist2),na.rm=T)


    
    # attempt again
    dnlist2 = dnlist
    # select row 1 as first point
    selected  = 1 
     
    all = 1:length(potentials)
    potentials = as.list(rep(NA,length(dnlist)))
    potentials[[selected]]=T
    selected=all[unlist(potentials)]   
    # NA all neighbors in dnlist[[1]], set potential to F
    # Find next potential 
    dnlist2 = lapply(1:length(dnlist),function(x) if(sum(dnlist2[[x]] %in% selected)>0){
        return(NA)}else{
            return(dnlist2[[x]])} )
    # NA out  dnlist[[selected]]
    dnlist2[[max(selected,na.rm=T)]] = NA
    
    
    library(spdep)
    coordinates(station) = c("Lon", "Lat")
    proj4string(station) = CRS("+proj=longlat +datum=WGS84")  ## for example
    station = spTransform(station, CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs "))
    head(station)
    set.seed(11)
    station_sm=station[sample(dim(station)[1], 2000),]
    dnlist_sm = dnearneigh(coordinates(station_sm),0,6e5,row.names= station_sm$ID)
    dnlist_sm = lapply(1:length(dnlist_sm),function(x) if(sum(dnlist_sm[[x]] %in% unique(unlist(dnlist_sm)))>0){
        return(NULL)}else{
            return(dnlist_sm[[x]])} )
    

# Count # of stations in each filtered group ------------------------------
library(foreign)
    
    setwd('C:\\Users\\mmann\\Google Drive\\Climate Change Perception\\Mike County Data Files\\Output\\')
    list.files('.','Census_S')
    
    count_list = list()
    yrs_list=list()
    mis_list=list()
    for (yrs in c(30,40,50)){
        for (mis in c(5,10,15)){
           file= read.dbf(paste('Census_Station_',yrs,'_',mis,'.dbf',sep=''))
           count_list =  c(count_list,length(unique(file$StationID)))
           yrs_list   = c(yrs_list,yrs)
           mis_list   = c(mis_list,mis)
        }
    }        
    data.frame(MinYrs= unlist(yrs_list),MaxMis = unlist(mis_list),StationNum = unlist(count_list))
    
    
    
                
#  These functions use bigmemory package to create large spatial weights matrix works with R\R-2.15.3   
#   my_nb2mat = function (neighbours, glist = NULL, style = "W", zero.policy = NULL) 
#   {
#     if (is.null(zero.policy)) 
#       zero.policy <- get("zeroPolicy", envir = .spdepOptions)
#     stopifnot(is.logical(zero.policy))
#     if (!inherits(neighbours, "nb")) 
#       stop("Not a neighbours list")
#     listw <- nb2listw(neighbours, glist = glist, style = style, 
#                       zero.policy = zero.policy)
#     res <- my_listw2mat(listw)
#     attr(res, "call") <- match.call()
#     res
#   }
#   
#   my_listw2mat = function (listw) 
#   {
#     require(bigmemory)
#     n <- length(listw$neighbours)
#     if (n < 1) 
#       stop("non-positive number of entities")
#     cardnb <- card(listw$neighbours)
#     if (any(is.na(unlist(listw$weights)))) 
#       stop("NAs in general weights list")
#     #res <- matrix(0, nrow = n, ncol = n)
#     res <- big.matrix(n, n, type='double', init=NULL)
#     options(bigmemory.allow.dimnames=TRUE)
#     
#     for (i in 1:n) if (cardnb[i] > 0) 
#       res[i, listw$neighbours[[i]]] <- listw$weights[[i]]
#     if (!is.null(attr(listw, "region.id"))) 
#       row.names(res) <- attr(listw, "region.id")
#     res
#   }
#   
#   a=my_nb2mat(neighbours = out, style='W',zero.policy =F )
#   
#   save(a,states.p,file='C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files//Weights.RData' ,version = NULL)
#rm(list = ls())
#load('G:\\Faculty\\Mann\\Projects\\Ethiopia Project\\GIS Data\\gapmapggplotcluster.RData' )

#  
# # from http://geokitchen.blogspot.com/
# install.packages( c( "snow", "snowfall", "spgwr", "maptools", "sp", "spdep", "lattice" ))
# library(sp) 
# library(lattice) 
# library(spgwr) 
# gpclibPermit()
# 
# 
# 
# map = SpatialPointsDataFrame(data=mydata, coords=cbind(POPEAST, POPNORTH)) 
# colours = c("dark blue", "green", "red", "dark red") 
# # Plot map
# spplot(map, "Nocars", cuts=quantile(Nocars), col.regions=colours) 
# bw = gwr.sel(Nocars ~ Unemp, data=map, adapt=T) 
# ## To now fit the model, type, 
# gwr.model = gwr(Nocars ~ Unemp, data=map, adapt=bw, hatmatrix=T, se.fit=T) 
# ## And to see a summary of it 
# gwr.model  
