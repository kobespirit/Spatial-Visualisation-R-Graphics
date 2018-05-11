# Group Members
# Shen Zhang    707496
# Gang Fu       709659
# Jie Pu        765316
# Changjian Ma  653909


# install necessary packages
install.packages("tmap")
install.packages("leaflet")
install.packages("scales")
install.packages("htmlwidgets")

# enable the packages
library("htmlwidgets")
library("scales")
library("tmap")
library("leaflet")



# import files
datafile <- read.csv("election_2012_by_state.csv")
usshapefile <- "Shapefile_us/Clipped_Shapefile_us_1.shp"
usgeo <- read_shape(file=usshapefile)

attach(datafile)


# divide the screen
par(mfrow = c(2 ,2))

# get the top three counties with most votes for the candidate Gingrich
topThree = head(sort(Gingrich, decreasing = T), 3)
  
# define an empty vector for storing the names of the counties for the top three for Gingrich
topNames = c()
  
# append the names of the top three counties with the top votes for Gingrich
for(number in topThree) {
  for (i in 1:nrow(datafile)) {
    if (datafile[i, "Gingrich"] == number) {
      topNames = c(topNames, as.character(datafile[i, "State"]))
    }
  }
}

# bar charts of vote numbers in the top three countries for the candidate Gingrich
barplot(topThree, names = topNames, main = "Top Three Counties for Gingrich", xlab = "Vote Number", ylab = "State", col = heat.colors(3), horiz = T, cex.main = 1, cex.lab = 0.8, cex.axis = 0.5, las = 2)

# get the top three counties with most votes for the candidate Paul
topThree = head(sort(Paul, decreasing = T), 3)

# define an empty vector for storing the names of the counties for the top three for Paul
topNames = c()

# append the names of the top three counties with the top votes for Paul
for(number in topThree) {
  for (i in 1:nrow(datafile)) {
    if (datafile[i, "Paul"] == number) {
      topNames = c(topNames, as.character(datafile[i, "State"]))
    }
  }
}

# bar charts of vote numbers in the top three countries for the candidate Paul
barplot(topThree, names = topNames, main = "Top Three Counties for Paul", xlab = "Vote Number", ylab = "State", col = heat.colors(3), horiz = T, cex.main = 1, cex.lab = 0.8, cex.axis = 0.5, las = 2)

# get the top three counties with most votes for the candidate Romney
topThree = head(sort(Romney, decreasing = T), 3)

# define an empty vector for storing the names of the counties for the top three for Romney
topNames = c()

# append the names of the top three counties with the top votes for Romney
for(number in topThree) {
  for (i in 1:nrow(datafile)) {
    if (datafile[i, "Romney"] == number) {
      topNames = c(topNames, as.character(datafile[i, "State"]))
    }
  }
}

# bar charts of vote numbers in the top three countries for the candidate Romney
barplot(topThree, names = topNames, main = "Top Three Counties for Romney", xlab = "Vote Number", ylab = "State", col = heat.colors(3), horiz = T, cex.main = 1, cex.lab = 0.8, cex.axis = 0.5, las = 2)

# get the top three counties with most votes for the candidate Santorum
topThree = head(sort(Santorum, decreasing = T), 3)

# define an empty vector for storing the names of the counties for the top three for Santorum
topNames = c()

# append the names of the top three counties with the top votes for Santorum
for(number in topThree) {
  for (i in 1:nrow(datafile)) {
    if (datafile[i, "Santorum"] == number) {
      topNames = c(topNames, as.character(datafile[i, "State"]))
    }
  }
}

# bar charts of vote numbers in the top three countries for the candidate Santorum
barplot(topThree, names = topNames, main = "Top Three Counties for Santorum", xlab = "Vote Number", ylab = "State", col = heat.colors(3), horiz = T, cex.main = 1, cex.lab = 0.8, cex.axis = 0.5, las = 2)

# re-define 'divide': avoid divided by o errors
"/" <- function(x, y) ifelse(y==0, 0, base:::"/"(x, y))

# create new attributes containing the percentage of the votes
datafile$GingrichPct <- datafile$Gingrich / (datafile$Gingrich+datafile$Paul+datafile$Romney+datafile$Santorum+datafile$Others)
datafile$PaulPct <- datafile$Paul / (datafile$Gingrich+datafile$Paul+datafile$Romney+datafile$Santorum+datafile$Others)
datafile$RomneyPct <- datafile$Romney / (datafile$Gingrich+datafile$Paul+datafile$Romney+datafile$Santorum+datafile$Others)
datafile$SantorumPct <- datafile$Santorum / (datafile$Gingrich+datafile$Paul+datafile$Romney+datafile$Santorum+datafile$Others)
datafile$OthersPct <- datafile$Others / (datafile$Gingrich+datafile$Paul+datafile$Romney+datafile$Santorum+datafile$Others)

# find out the winner for each state
for (i in 1:nrow(datafile)){
  if(datafile$Gingrich[i] == datafile$Paul[i] && 
     datafile$Paul[i] == datafile$Romney[i] && 
     datafile$Romney[i] == datafile$Santorum[i] && 
     datafile$Santorum[i] == datafile$Others[i]){
    index <- 5
  }
  else{
    index <- which.max(c(datafile$Gingrich[i], datafile$Paul[i], datafile$Romney[i], datafile$Santorum[i], datafile$Others[i]))
  }
  if(index == 1){datafile$Winner[i] <- "Gingrich"}
  else if(index ==2){datafile$Winner[i] <- "Paul"}
  else if(index ==3){datafile$Winner[i] <- "Romney"}
  else if(index ==4){datafile$Winner[i] <- "Santorum"}
  else {datafile$Winner[i] <- "N/A"}
}


# sort the matching ids for comparison
usgeo <- usgeo[order(usgeo@data$STUSPS),]
datafile <- datafile[order(datafile$State),]

# test if the common attributes match
identical(usgeo@data$STUSPS,datafile$State )

# merge the datasets
map <- append_data(usgeo, datafile, key.shp = "STUSPS", key.data="State")

# find the maximum and minimun values for the palettes
min = min(c(map$GingrichPct, map$PaulPct, map$RomneyPct, map$SantorumPct, map$OthersPct))
max = max(c(map$GingrichPct, map$PaulPct, map$RomneyPct, map$SantorumPct, map$OthersPct))

# generate the palettes for the map
GingrichPalette <- colorNumeric(palette = "Oranges", domain = c(min, max))
PaulPalette <- colorNumeric(palette = "Purples", domain=c(min, max))
RomneyPalette <- colorNumeric(palette = "Blues", domain = c(min, max))
SantorumPalette <- colorNumeric(palette = "Greens", domain = c(min, max))
OthersPalette <- colorNumeric(palette = "Reds", domain = c(min, max))
WinnerPalette <- colorFactor(palette=c("#e96d20", "#e18484","#0004e6", "#0004e6", "#25c516"), domain = map$Winner)

# define popup of the map
uspopup <- paste0("<div><b>State: ", map@data$NAME, "</b></div>",
                  "<div><b>Winner: ", map@data$Winner, "</b></div>",
                  "<div><b>Ginrich: </b>", percent(map@data$GingrichPct),"</div>",
                  "<div><b>Paul: </b>", percent(map@data$PaulPct),"</div>",
                  "<div><b>Romney: </b>", percent(map@data$RomneyPct),"</div>",
                  "<div><b>Santorum: </b>", percent(map@data$SantorumPct),"</div>",
                  "<div><b>Others: </b>", percent(map@data$OthersPct), "</div>")

# create the interactive map for election results
InteractiveMap <- leaflet(map) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # main layer, indicates the winner of each state, marked by winner's colour
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=uspopup, 
              color= ~WinnerPalette(map@data$Winner),
              group="Winners"
  ) %>% 
  addLegend(position="bottomleft", 
            colors=c("#e96d20","#202020","#0004e6", "#25c516", "#e18484"), 
            labels=c("Gingrich", "Paul", "Romney", "Santorum", "N/A"))%>%
  
  # layer Gingrich
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=uspopup, 
              color= ~GingrichPalette(map@data$GingrichPct),
              group="Gingrich"
  ) %>%
  
  # Layer Paul
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=uspopup, 
              color= ~PaulPalette(map@data$PaulPct),
              group="Paul"
  ) %>%
  
  # Layer Romney
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=uspopup, 
              color= ~RomneyPalette(map@data$RomneyPct),
              group="Romney"
  ) %>%
  
  # Layer Santorum
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=uspopup, 
              color= ~SantorumPalette(map@data$SantorumPct),
              group="Santorum"
  ) %>%
  
  # Layer Others
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=uspopup, 
              color= ~OthersPalette(map@data$OthersPct),
              group="Others"
  ) %>%
  
  # layers change control panel
  addLayersControl(
    baseGroups=c("Winners", "Gingrich", "Paul", "Romney", "Santorum", "Others"),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) 

# save the map
saveWidget(widget=InteractiveMap, file="InteractiveMap_Election.html")


