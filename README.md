Sustainable Cooking: Source Local and Organic
================
Juan C. S. Herrera
July, 2019



![alt text](https://github.com/juancsherrera/TDISupplychain/blob/master/README_figs/usaodsupplychain.gif)

Title: Sustainable Cooking: Source Local and Organic

Motivation and description of the problem: A growing trend towards organic and sustainable consumption has sparked new players in the food industry: from restaurants such as Sweetgreen to organic dairy and other products that consumers have at their disposal. A growing concern in this move towards sustainability is how much does food travel in order to get to the table. The project that I am proposing tackles this challenge: how can we create food recipes for consumers, restaurants, and the industry in order to minimize the miles that food  organic food will have to travel in order to get to the consumer's table.


In order to tackle this challenge I will use a dataset of certified organic products along with a datset of food recipes in which one can either choose a recipe and then find the organic producers that could produce that recipe OR one can create a recipe by using existing organic production facilities minimizing the distance from the consumer to the producers.

Data: The main datasets are: 

1. The United States Department of Agriculture Organic Integrity Database that contains information for every organic product certified by the USDA. This dataset contains an address for each operation (handler and farmer) but not a GPS coordinate. I used the google maps API to retrieve the GPS coordinates. 

2. A large dataset ocontaining 56,498 food recipes originally scraped from allrecipes.com by a group of authors.

Methods: I use network science methodologies to infer dynamic supply chains that connect farmers -&gt; handlers -&gt; to potential consumers. These supply chains are inferred based on modeling connections between the GPS coordinates. The methods are a variation of the travel salesman problem in which 

Conclusions: I here demonstarte that this method is able to 

Shortcomings: The main shortcomings of this project are: (1) I do not have data on the size of the operation of the organic products. hence, in order to make this feasible for a company to actually source the products minimizing the distances I will have to colect data on their potential output. If the potential output does not meet the client's demands then the node could be reclassified and the minimization recalculated. (2) The distances have been computed as euclidean distances. This may be problematic in a real world application, however this has been done for efficiency and due to lack of credits on the google API. This is an easily solvable problem given some investment money to compute the actual travel distances, nevertheless euclidean distances provide a good approach.


Analysis
--------
``` r
dairyp<-as.data.frame(read.csv(paste0(directx,"/lsallUSAgeo.csv"), header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1"))
conn <- dbConnect(RSQLite::SQLite(), dbname="organics.sqlite")
dbWriteTable(conn, value = dairyp, name = "dairygeoprod", overwrite = TRUE) 
dairypgeo<-dbGetQuery(conn, "
                        SELECT A. *
                        FROM dairygeoprod A
                        WHERE TRIM(A.ci_nopScope) IN ('LIVESTOCK')
                        AND   TRIM(A.opPA_country) IN ('United States of America (the)')
                        AND   ((TRIM(UPPER(A.ci_itemList)) LIKE ('%DAIRY%')
                        OR    TRIM(UPPER(A.ci_itemList)) LIKE ('%MILK%'))
                        OR    (TRIM(UPPER(A.ci_nopCatName)) LIKE ('%DAIRY%')
                        OR    TRIM(UPPER(A.ci_nopCatName)) LIKE ('%MILK%')))
                       ")
#get years from database for producers
dairypgeo$year<-as.numeric(as.character(substr(dairypgeo$op_statusEffectiveDate,(nchar(dairypgeo$op_statusEffectiveDate)+1)-4,nchar(dairypgeo$op_statusEffectiveDate))))
dairypgeo<-dairypgeo[dairypgeo$year <= 2015,]


  url <- URLencode(paste0(url, address,keystuff, sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- matrix(nrow=2,ncol=1)
  }  out-
}
getwd()

#Create vector with all the address values
alloperations$opadresscom <- paste(alloperations$opPA_line1 , alloperations$opPA_line2 , alloperations$opPA_city , alloperations$opPA_state , alloperations$opPA_country , alloperations$opPA_zip, sep=" ")
#clean
alloperations$opadfinal<-gsub("[^0-9\\.\\^A-Z\\^a-z\\ ]", "", alloperations$opadresscom)
#Use function, get latitude and longitude from google maps
#You will need to re run this several times, excluding those for which you were able to get coordinates. Again, it is important to register for the google maps API so you don't run over the limit of queries, else you will have to pay.
head(alloperations)
dim(alloperations)

alloperations<-read.csv("/Users/juan/Dropbox/ACADEMICO/NYU PHD/Y2/Independent Study/Organic/food_journal/alloperations.csv")
geovector<-as.data.frame(alloperations$opadfinal)
dim(geovector)
geovector<-unique(geovector)
dim(geovector)

dim(alloperations)

preurl <- "https://maps.googleapis.com/maps/api/geocode/json?address="

#################################################
##########       WARNING          ###############
#################################################
#keystuff <- !!!PROVIDE YOUR OWN GOOGLE API KEY!!!, otherwise this will not work. I have deleted mine

geovector$lat<--999
geovector$lon<--999
head(geovector)
dim(geovector)
for(i in 10001:15629)
{
  url<-URLencode(paste0(preurl, as.character(geovector[i,1]),keystuff, sep = ""))
  x <- fromJSON(url, simplify = FALSE)

  if (x$status == "OK") {
    geovector[i,2]<-as.numeric(x$results[[1]]$geometry$location$lat)
    geovector[i,3]<-as.numeric(x$results[[1]]$geometry$location$lng)
  } else {
    geovector[i,2]<--888
    geovector[i,3]<--888
  } 
}

write.csv(geovector,"geovector.csv")
isitgoodornot <- read.csv("geovector.csv") 

head(isitgoodornot,1000)

head(geovector,1000)

table(geovector$lat)

xx<-URLencode(paste0(url, as.character(geovector[i,1]),keystuff, sep = ""))

for(i in 1:1)
{
  lxxx <- paste(t(paste0(print(geocodeAdddress(geovector[i,1])))),sep="xxx")
  lxxx<-as.data.frame(t(lxxx))
  lxxx[1,3]<-paste(lxxx[1,1],lxxx[1,2],sep="xxx")
  lxxx
  geovector[i,2] <- (lxxx[1,3])
}
head(geovector)
write.csv(alloperations, "alloperations.csv")



#Unpack lat lon from data Google Geo Coded data
dairypgeo$lat<-sub(".*xxx", "", dairypgeo$geo)
dairypgeo$lon<-sub("xxx.*", "", dairypgeo$geo)
dairypgeo$lat<-sub(".*yyy", "", dairypgeo$lat)
dairypgeo$lon<-sub("yyy.*", "", dairypgeo$lon)

#check size of dataset: consistent: OK
#dim(dairypgeo)
```

Load Previously Geo coded data for Handlers
===========================================

``` r
#Load Handlers
dairyh<-as.data.frame(read.csv(paste0(directx,"/handlallUSAtemp.csv"),stringsAsFactors = FALSE, fileEncoding="latin1"))
dairyhgeo<-dairyh
#get years from database for producers
dairyhgeo$year<-as.numeric(as.character(substr(dairyh$op_statusEffectiveDate,(nchar(dairyh$op_statusEffectiveDate)+1)-4,nchar(dairyh$op_statusEffectiveDate))))
class(dairyh$year)
#dairypgeo$year<-as.numeric(levels(dairypgeo$year))[dairypgeo$year]
#head(dairypgeo$year)
#plot(table(dairypgeo$year))

#geat years from database for handlers
dairyhgeo$year<-as.numeric(substr(dairyhgeo$op_statusEffectiveDate,(nchar(dairyhgeo$op_statusEffectiveDate)+1)-4,nchar(dairyhgeo$op_statusEffectiveDate)))
#dairyh$year<-as.numeric(levels(dairyh$year))[dairyh$year]

#Unpack lat lon from data Google Geo Coded data
dairyhgeo$lat<-sub(".*xxx", "", dairyhgeo$geo)
dairyhgeo$lon<-sub("xxx.*", "", dairyhgeo$geo)
dairyhgeo$lat<-sub(".*yyy", "", dairyhgeo$lat)
dairyhgeo$lon<-sub("yyy.*", "", dairyhgeo$lon)
dim(dairyhgeo)
```

``` r
#load Whole Foods Geocoded data
wf<-wf[,2:3]
# now you will have something like this where you find the location and the gps coordinates:
wf$lat<-as.numeric(sub(".*xxx", "", wf$V3))
wf$lon<-as.numeric(sub("xxx.*", "", wf$V3))
dim(wf)
```

Connecting Producers and Handlers
---------------------------------

-   MxN matrix where *N*<sub>1</sub>, ..., *N*<sub>*n*</sub> = *p**r**o**d**u**c**e**r**s* and *M*<sub>1</sub>, ..., *M*<sub>*n*</sub> = *h**a**n**d**l**e**r**s*
-   Use Haversine distance to Calculate distance between each observation *M**N*
-   The Haversine distance formula in kilometers is:

*A**C**O**S*(*S**I**N*(*L**a**t*1)\**S**I**N*(*L**a**t*2)+*C**O**S*(*L**a**t*1)\**C**O**S*(*L**a**t*2)\**C**O**S*(*L**o**n*2 − *L**o**n*1)) \* 6371

``` r
#####################################################################
# Connect Producers to Handlers year by year: new entrants     ###############
#####################################################################

### IMPORTANT IMPORTANT
#CLEANS: 
#  1 Only Certified
#  2. Only those with GPS Coordinates

producers<-dairypgeo[dairypgeo$dataloss=="GPS & Certified",]
producers<-producers[producers$year <= 2015,]
producers$year[producers$year <= 2002] <- 2002
producers<-producers[,c("op_nopOpID","op_name","year","lat","lon")]
producers<-unique(producers)

handlers<-dairyhgeo[dairyhgeo$dataloss=="GPS & Certified",]
handlers<-handlers[handlers$year <= 2015,]
handlers$year[handlers$year <= 2002] <- 2002
handlers<-handlers[,c("op_nopOpID","op_name","year","lat","lon")]
handlers<-unique(handlers)

#dim(producers)
#dim(handlers)

#matrix to fill
tempdistances<-matrix(nrow=1, ncol=11)
#dimensions of producers and handlers, makes sure that every produucer is matched with a handler
dimprod<-(as.matrix(dim(producers)))[1,1]
dimhandl<-(as.matrix(dim(handlers)))[1,1]

table(producers$year)
table(handlers$year)
colnames(producers)
colnames(handlers)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
deg2rad <- function(deg) return(deg*pi/180)
# Haversine formula (hf)
haversine <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

for(i in 2002:2015)
{
  producersi<-producers[producers$year == i,]
  handlersi<-handlers[handlers$year <= i,]
  
  dimprod<-(as.matrix(dim(producersi)))[1,1]
  dimhandl<-(as.matrix(dim(handlersi)))[1,1]
  
  for(j in 1:dimprod)
  {
    for(k in 1:dimhandl)
    {
      #create matrix to populate with all calculated distances
      
      #calculate, populate, bring data for calculations: point pairs, point lat lon, distance calculated
      
      tempdistances[1,1]<-(as.numeric(producersi[j,1]))
      tempdistances[1,2]<-(as.numeric(handlersi[k,1]))
      tempdistances[1,3]<-(as.character(producersi[j,2]))
      tempdistances[1,4]<-(as.character(handlersi[k,2]))
      tempdistances[1,5]<-(as.numeric(producersi[j,3]))
      tempdistances[1,6]<-(as.numeric(handlersi[k,3]))
      
      tempdistances[1,7]<-(as.numeric(producersi[j,4]))
      tempdistances[1,8]<-(as.numeric(handlersi[k,4]))
      tempdistances[1,9]<-(as.numeric(producersi[j,5]))
      tempdistances[1,10]<-(as.numeric(handlersi[k,5]))
      
      tempdistances[1,11]<-(as.numeric(haversine(deg2rad(as.numeric(producersi[j,5])),
                                                 deg2rad(as.numeric(producersi[j,4])),
                                                 deg2rad(as.numeric(handlersi[k,5])), 
                                                deg2rad(as.numeric(handlersi[k,4])))))
      if (k == 1)
      {
        distancesf<-tempdistances
      }
      if (k > 1)
      {
        distancesf<-rbind(distancesf,tempdistances)
      }
    }
    
    #distancesf <- as.data.frame(distancesf, stringsAsFactors = FALSE)
    #distancesf$V11 <- as.numeric(distancesf$V11)
    #distancesf<-distancesf[order(distancesf$V11),]
    #distancesf$p_dis_closest<-distancesf$V11/distancesf[1,11]
    #distancesf<-na.omit(distancesf)
    #distancesf$iteration<-j
    #print(j)
    if (j == 1)
    {
      distancesff<-distancesf
    }
    if (k > 1)
    {
      distancesff<-rbind(distancesff,distancesf)
    }
    #distancesff<-distancesff[distancesff$p_dis_closest <= threshholdpercdist, ]
    #distancesff<-distancesff[distancesff$p_dis_closest > 1, ]
  }
  if (i == 2002)
  {
    distancesfff<-distancesff
  }
  if (i > 2002)
  {
    distancesfff<-rbind(distancesfff,distancesff)
  }
  print(i)
}

distancesfff<-as.data.frame(distancesfff, stringsAsFactors = FALSE)
write.csv(distancesfff,paste0(directx,"/prod_handl_year.csv"))
remove(distancesf,distancesff)

#head(distancesfff)
#tolookifallok<-distancesfff[distancesfff$V1=="9950008750",]
#tail(distancesfff,3000)
#head(tolookifallok)
#table(tolookifallok$V5)

#table(tolookifallok$)
beep()
#remove(distancesfff, distancesf, distancesff)
```

``` r
#####################################################################
# Connect Hanldlers to Whole Foods (Proxy for consumers): new entrants     
#####################################################################

### IMPORTANT IMPORTANT
#CLEANS: 
#  1 Only Certified
#  2. Only those with GPS Coordinates
handlers<-dairyhgeo[dairyhgeo$dataloss=="GPS & Certified",]
handlers<-handlers[handlers$year <= 2015,]
handlers$year[handlers$year <= 2002] <- 2002
handlers<-handlers[,c("op_nopOpID","op_name","year","lat","lon")]
handlers<-unique(handlers)

dim(handlers)
dim(wf)
colnames(wf)
head(wf)

#matrix to fill
tempdistances<-matrix(nrow=1, ncol=11) 
#dimensions of producers and handlers, makes sure that every produucer is matched with a handler
dimwf<-(as.matrix(dim(wf)))[1,1]
dimhandl<-(as.matrix(dim(handlers)))[1,1]

table(handlers$year)
colnames(handlers)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
deg2rad <- function(deg) return(deg*pi/180)
# Haversine formula (hf)
haversine <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

  for(j in 1:dimwf)
  {
    for(k in 1:dimhandl)
    {
      #create matrix to populate with all calculated distances
      
      #calculate, populate, bring data for calculations: point pairs, point lat lon, distance calculated
      
      tempdistances[1,1]<-(as.character(wf[j,1]))
      tempdistances[1,2]<-(as.numeric(handlersi[k,1]))
      tempdistances[1,3]<-(as.character(wf[j,2]))
      tempdistances[1,4]<-(as.character(handlersi[k,2]))
      tempdistances[1,5]<-(as.numeric(wf[j,3]))
      tempdistances[1,6]<-(as.numeric(handlersi[k,3]))
      
      tempdistances[1,7]<-(as.numeric(wf[j,4]))
      tempdistances[1,8]<-(as.numeric(handlersi[k,4]))
      tempdistances[1,9]<-(as.numeric(wf[j,3]))
      tempdistances[1,10]<-(as.numeric(handlersi[k,5]))
      
      tempdistances[1,11]<-(as.numeric(haversine(deg2rad(as.numeric(wf[j,4])), 
                                                 deg2rad(as.numeric(wf[j,3])), 
                                             deg2rad(as.numeric(handlersi[k,5])),                                             deg2rad(as.numeric(handlersi[k,4])))))
      if (k == 1)
      {
        distancesf<-tempdistances
      }
      if (k > 1)
      {
        distancesf<-rbind(distancesf,tempdistances)
      }
    }
    
    #distancesf <- as.data.frame(distancesf, stringsAsFactors = FALSE)
    #distancesf$V11 <- as.numeric(distancesf$V11)
    #distancesf<-distancesf[order(distancesf$V11),]
    #distancesf$p_dis_closest<-distancesf$V11/distancesf[1,11]
    #distancesf<-na.omit(distancesf)
    #distancesf$iteration<-j
    print(j)
    if (j == 1)
    {
      distancesff<-distancesf
    }
    if (k > 1)
    {
      distancesff<-rbind(distancesff,distancesf)
    }
    #distancesff<-distancesff[distancesff$p_dis_closest <= threshholdpercdist, ]
    #distancesff<-distancesff[distancesff$p_dis_closest > 1, ]
  }

distancesff<-as.data.frame(distancesff)
write.csv(distancesff,paste0(directx,"/wf_handl_year.csv"))
head(distancesff)

beep(sound=11)

#remove(distancesfff, distancesf, distancesff)
```

Mapping and Network Visualization
=================================

-   You can import the data from the previous steps here.

This code imports all connections. In this database every single producer is connected to another one. In the code I filter this to include only those producers connected with other ones within 50 miles around them or less.

WARNING: Takes some time to run, you can skip this chunk and load the data (eval = T if you want to run it)

``` r
##Load Network
net<-read.csv(paste0(directx,"/prod_handl_year.csv"),stringsAsFactors = FALSE)
net2<-read.csv(paste0(directx,"/wf_handl_year.csv"),stringsAsFactors = FALSE)
dim(net)
dim(net2)
head(net)

#CHOPS THE DATABASE TO CONNECT ONLY WITH HANDLERS 50 MILES AROUND
net <- net[net$V11<= (50*1.6),] #Distance in kilometers, therefore the *1.6
#CHOPS THE DATABASE TO CONNECT ONLY WITH HANDLERS 300 MILES AROUND
net2 <- net2[net2$V11<= (300*1.6),] #Distance in kilometers, therefore the *1.6
dim(net)
dim(net2)


#################
#Edges
#################

#get connections Producer-Handler

#get producer-handler net
net<-net[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11")]
#get handler-WF net
net2<-net2[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11")]

head(net)

#gets in edge format
dimforloop<-as.matrix(dim(net))[1,1]
connectingmap<-matrix(nrow=0, ncol=7) 
a<-matrix(nrow=1, ncol=7) 
b<-matrix(nrow=1, ncol=7) 
for(i in 1:dimforloop)
{
  a[1,1]<-net[i,9]
  a[1,2]<-net[i,7]
  a[1,3]<-i
  a[1,4]<-net[i,1]
  a[1,5]<-net[i,11]
  a[1,6]<-net[i,5]
  a[1,7]<-c("farmer")
  
  b[1,1]<-net[i,10]
  b[1,2]<-net[i,8]
  b[1,3]<-i
  b[1,4]<-net[i,2]
  b[1,5]<-net[i,11]
  b[1,6]<-net[i,6]
  b[1,7]<-c("handler")
  
  connectingmap<-rbind(connectingmap,a,b)
  #print(i/dimforloop)
}

lenghtprodhan<-as.matrix(dim(connectingmap))[1,1]

remove(a,b)
head(connectingmap)
##Add connections Handler-Whole Foods
dimforloop<-as.matrix(dim(net2))[1,1]
a<-matrix(nrow=1, ncol=7) 
b<-matrix(nrow=1, ncol=7) 
#dim(connectingmap)

for(i in 1:dimforloop)
{
  a[1,1]<-net2[i,7]
  a[1,2]<-net2[i,9]
  a[1,3]<-i+lenghtprodhan
  a[1,4]<-net2[i,1]
  a[1,5]<-net2[i,11]
  a[1,6]<-2002
  a[1,7]<-c("handler")
  
  b[1,1]<-net2[i,10]
  b[1,2]<-net2[i,8]
  b[1,3]<-i+lenghtprodhan
  b[1,4]<-net2[i,2]
  b[1,5]<-net2[i,11]
  b[1,6]<-net2[i,6]
  b[1,7]<-c("supermarket")
  
  connectingmap<-rbind(connectingmap,a,b)
  #print(i/dimforloop)
}

#remove(a,b)
head(connectingmap)

connectingmap<-as.data.frame(connectingmap)
colnames(connectingmap) <- c("lon", "lat","iteration","op_id","distance","year","node_type")
connectingmap$lon<-as.numeric(as.character(connectingmap$lon))
connectingmap$lat<-as.numeric(as.character(connectingmap$lat))
connectingmap$iteration<-as.numeric(as.character(connectingmap$iteration))
connectingmap$distance<-as.numeric(as.character(connectingmap$distance))
connectingmap$year<-as.numeric(as.character(connectingmap$year))
connectingmap$op_id<-as.character(connectingmap$op_id)
connectingmap<-connectingmap[connectingmap$lon>=-140,]

#remove HI
nodes_supply_chain<-nodes_supply_chain[nodes_supply_chain$lon>=-140,]
#head(nodes_supply_chain)

#Farmer - Handler network

remove(fig1fig,fig1tot)
usa <- map_data("state")
fig1tot<-connectingmap[connectingmap$year<=2005,]
fig1tot<-fig1tot[fig1tot$node_type!="supermarket",]

theme_base(base_size = 200)
fig1fig <- ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group, label = "Fig 1"), fill = "white", color = "#9fa9a3") + coord_fixed(1.3) #load USA map data
fig1fig <- fig1fig + geom_path(data= fig1tot, aes(x=lon, y=lat, group = iteration), color="black", size=0.05)
fig1fig <- fig1fig + geom_point(data = fig1tot, aes(x = lon, y = lat, shape=node_type, color=node_type), size = 1.5) #add nodes
fig1fig <- fig1fig + theme_map()
fig1fig <- fig1fig + ggtitle(label = c("Simulated Organic Dairy Supply Chain (Farmer - Handler Network 2005"), subtitle = c("Figure 1. 2005           "))
fig1fig
ggsave((paste0(directx,"/f_h_fig1_2005.eps")))

#Supply Chain

remove(fig1fig,fig1tot)
usa <- map_data("state")
fig1tot<-connectingmap[connectingmap$year<=2005,]
theme_base(base_size = 200)
fig1fig <- ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group, label = "Fig 1"), fill = "white", color = "#9fa9a3") + coord_fixed(1.3) #load USA map data
fig1fig <- fig1fig + geom_path(data= fig1tot, aes(x=lon, y=lat, group = iteration), color="black", size=0.05)
fig1fig <- fig1fig + geom_point(data = fig1tot, aes(x = lon, y = lat, shape=node_type, color=node_type), size = 1.5) #add nodes
fig1fig <- fig1fig + theme_map()
fig1fig <- fig1fig + ggtitle(label = c("Simulated Organic Dairy Supply Chain. 2002 and 2015"), subtitle = c("Figure 1. 2005           "))
fig1fig
ggsave((paste0(directx,"/fig1_2005.eps")))

#Import USA map data
remove(fig2fig,fig2tot)
usa <- map_data("state")
fig2tot<-connectingmap
theme_base(base_size = 200)
fig2fig <- ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group, label = "Fig 1"), fill = "white", color = "#9fa9a3") + coord_fixed(1.3) #load USA map data
fig2fig <- fig2fig + geom_path(data= fig2tot, aes(x=lon, y=lat, group = iteration), color="black", size=0.05)
fig2fig <- fig2fig + geom_point(data = fig2tot, aes(x = lon, y = lat, shape=node_type, color=node_type), size = 1.5) #add nodes
fig2fig <- fig2fig + theme_map()
fig2fig <- fig2fig + ggtitle(label = c("Simulated Organic Dairy Supply Chain. 2002 and 2015"), subtitle = c("Figure 2. 2015           "))
fig2fig
ggsave((paste0(directx,"/fig2_2015.eps")))

fig2a <- fig1fig + coord_fixed(xlim = c(-78.006,    -70.006), ylim = c(38.2128, 43.2128)) 
fig2a
ggsave((paste0(directx,"/2005_SC_detail_NYC.eps")))


fig2b <- fig2fig + coord_fixed(xlim = c(-78.006,    -70.006), ylim = c(38.2128, 43.2128)) 
fig2b
ggsave((paste0(directx,"/2015_SC_detail_NYC.eps")))

#Create animated plot, this creates an animation with one frame per year.
theme_base(base_size = 200)
map <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "white", color = "#9fa9a3") + coord_fixed(1.3) #load USA map data

map <- map + geom_path(data= connectingmap, aes(x=lon, y=lat, group = iteration, frame = year, cumulative = T), color="gray", size=0.4)

map <- map + geom_point(data = connectingmap, aes(x = lon, y = lat, frame = year, shape=node_type, color=node_type, cumulative = T), size = 5) #add nodes

mapn <- map + theme_map()


 #create animation and export, for the whole USA
gganimate(mapn, interval = .3, ani.width= 2000, ani.height=1500 ,filename = 'usaodsupplychain.gif')

beep()

tail(connectingmap,100)
#remove(connectingmap)


```
