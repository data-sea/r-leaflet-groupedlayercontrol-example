#
# This script produces leaflet map exported to html, and demonstrates use of the JS add-on
# called groupedlayercontrol, allowing for exclusive group selection and/or toggling multiple groups of layers on or off
# More on the JS add-on here:: https://github.com/ismyrnow/leaflet-groupedlayercontrol
#
# SCRIPT by David Christensen
#
#####################################################################
library(leaflet)
library(readxl)
library(sf)
library(rgdal)
library(tidyverse)
library(htmlwidgets)
library(rattle)
library(htmltools)
library(RColorBrewer)
library(htmltools)

#
# Load King County, WA census tracts, and filter to tracts of interest.
SEAtracts <- st_read("./input/tracts10.shp") %>% 
  st_transform(., 4326)
SEAtracts<-SEAtracts[which(SEAtracts$TRACT_INT< 12500),]

#Load zip code shapefile for same area.
zipMap <- st_read("./input/zipcode_shore.shp") %>% 
  st_transform(., 4326)

#Get the languages preferences by zipcode
SPS_language<-readxl::read_xlsx("./input/Seattle Public School_Student Language Proficiency_2-13-2020 FINAL.xlsx")

#Summarize language by zip code summary in public schools
SPS_lang_summary <-SPS_language %>% .[!.$PREFERENCE_CONTACT_LANGUAGE=="English",] %>%
  select(ZIP,PREFERENCE_CONTACT_LANGUAGE) %>%
  group_by(ZIP,PREFERENCE_CONTACT_LANGUAGE) %>%
  summarize(top_n =n()) %>%  ungroup() %>%
  arrange(ZIP,desc(top_n))
  
#Get top language``
SPStop1<-SPS_lang_summary%>% group_by(ZIP)%>% slice(1) %>%
  rename(c("lang_top" = "PREFERENCE_CONTACT_LANGUAGE", "n_st"="top_n"))

#get 2nd from top language
SPS_2nd<- SPS_lang_summary %>% group_by(ZIP)%>% slice(2) %>%
 rename(c("lang_2nd" = "PREFERENCE_CONTACT_LANGUAGE", "n_2nd"="top_n"))

#get 3rd from top language
SPS_3rd<- SPS_lang_summary %>% group_by(ZIP)%>% slice(3) %>%
  rename(c("lang_3rd" = "PREFERENCE_CONTACT_LANGUAGE", "n_3rd"="top_n"))

#join top language and 2nd top by zipcode
SPStop2<-left_join(SPStop1,SPS_2nd, by='ZIP')
SPStop2<-left_join(SPStop2,SPS_3rd, by='ZIP')

#fill in NAs with blanks
SPStop2$lang_2nd[is.na(SPStop2$lang_2nd)]<-"None"
SPStop2$lang_3rd[is.na(SPStop2$lang_3rd)]<-"None"
SPStop2$n_2nd[is.na(SPStop2$n_2nd)]<-0
SPStop2$n_3rd[is.na(SPStop2$n_3rd)]<-0

#merge with zipcode map
zipMap <- merge(zipMap, SPStop2, by="ZIP")
zipMap<-sf::st_as_sf(zipMap)
zipMap<- zipMap[zipMap$n_st >2,]

#make a label for SPS popups
label_SPS_language<-paste("<b>Top 3 languages by zipcode</b><br />", zipMap$lang_top,": <b>", 
                          zipMap$n_st, "</b> students<br />",zipMap$lang_2nd,": <b>", 
                          zipMap$n_2nd, "</b> students<br /> ",zipMap$lang_3rd,": <b>", 
                          zipMap$n_3rd, "</b> students<br /><i>excludes English<br>Source:<br/>Seattle Public School_Student Language Proficiency_2-13-2020 FINAL.xlsx</i>")

#SPS "Super Reader" elementary schools
SPS_priority<-read_xlsx("./input/SPS_priority_schools.xlsx")
#ALL SPS schoools
SPS<-read_xlsx("./input/Title1_schools_19_20_GIS.xlsx")
#subset of just the title 1 schools
SPS_title1<-SPS[SPS$Is_title_1=="yes",]
#make sure GIS values are numeric
SPS_title1$latitude<-as.numeric(as.character(SPS_title1$latitude))
SPS_title1$longitude<-as.numeric(as.character(SPS_title1$longitude))

#Add sPL branch location data
SPL<-read.csv("./input/SPL Library Locations.csv")
SPL_subgroup1<-SPL[SPL$Branch_3CAP  %in% c("RBE","HIP","COL","LCY","DTH","SPA"),]
NOTSPL_subgroup1<-SPL[SPL$Branch_3CAP  %in% c("NHY","NGA","BEA", "BRO"),]
SPL_subgroup1<-SPL_subgroup1 |> mutate(hh = c(" -LB", " -LB", " -LB", " -HH", " -HH", " -HH"))
SPL_subgroup1$hh<-NULL

#make custom point icons for various locations
SPL_point_grey <- makeIcon(
  iconUrl = "./input/SPL_point_grey.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY = 50)

SPL_point_green <- makeIcon(
  iconUrl = "./input/SPL_point_green.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY = 50)


SPS_school_point <- makeIcon(
  iconUrl = "./input/SPS_point.png",
  iconWidth = 25, iconHeight = 33,
  iconAnchorX = 0, iconAnchorY = 50)

SPL_point <- makeIcon(
  iconUrl = "./input/SPL_point.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY = 50)

#Some basic popup labels
popup_label_SPL<-paste0("<u><b>",SPL$Branch_Full_Name,"</u></b>")
popup_SPL_subgroup1<-paste(SPL_subgroup1$Branch_3CAP,SPL_subgroup1$hh)
popup_SPL_subgroup2<-paste(NOTSPL_subgroup1$Branch_3CAP)

#load seattle race and social equity index
RaSEI<-read.csv("https://data-seattlecitygis.opendata.arcgis.com/datasets/225a4c2c50e94f2cb548a046217f49f7_0.csv") %>% 
  select("GEOID10", "NAME10", "NAMELSAD10","PCT_PEOPLE_OF_COLOR","COMPOSITE_QUINTILE")
RaSEI$COMPOSITE_QUINTILE<-factor(RaSEI$COMPOSITE_QUINTILE) %>% 
  ordered(., levels = c("Lowest","Second lowest","Middle", "Second highest","Highest priority/Most disadvantaged"))

#load area census tract file.
RaSEI<-RaSEI %>% dplyr::rename(., "GEO_ID_TRT" = "GEOID10")
SEAtracts <- st_read("./input/tracts10.shp") %>% 
  st_transform(., 4326)
#filter non-Seattle tracts.
SEAtracts<-SEAtracts[which(SEAtracts$TRACT_INT< 12500),]
SEAtracts <- merge(SEAtracts,RaSEI,by="GEO_ID_TRT")
SEAtracts<-sf::st_as_sf(SEAtracts)

#create various color fill bins for the shapefiles
bins <- c(0, 35, 70,  100, 150, 170, Inf)
palette_color<- c("#7B3014","#d85a13","#ffc2a1","#b7cde2","#3679a8", "#26456E") 
pal <- colorBin(palette_color, domain = SEAtracts$score, bins = bins)

POC_bins <- c(0, .15, .25,  .35, .45, .55, Inf)
palette_color_POC<- c("#FFFFFF","#DBF3FA","#92DFF3","#04A8E0","#0080FF", "#0000FF") 
pal_POC <- colorBin(palette_color_POC, domain = SEAtracts$PCT_PEOPLE_OF_COLOR, bins = POC_bins)

palette_color_RSEI<- c("#FFFFFF","#DBF3FA","#92DFF3","#0080FF", "#0000FF") 
pal_RSEI <- colorFactor(palette_color_RSEI, domain = SEAtracts$COMPOSITE_QUINTILE)

#create popup labels
popup_label_POC<-paste("People of color:<b>",
                       round(SEAtracts$PCT_PEOPLE_OF_COLOR,3)*100,
                      "%</b><br>(",SEAtracts$TRACT_LBL,")
                       <br>Source:<br> https://data.seattle.gov/Land-Base/Racial-and-Social-Equity-Composite-Index/q9y2-kzsi")


popup_label_SPS<-paste("<u>",
                       SPS_priority$School,
                       "</u><br>Title 1: <b>",
                       SPS_priority$Is_title_1 ,"
                       </b>" )

popup_label_SPS_title1<-paste("<b><u>",
                              SPS_title1$School,
                              "</u></b><br>",
                              SPS_title1$address,
                              "<br><a href='",
                              SPS_title1$URL,
                              "'>School website.</a>")

popup_label_SPL<-paste("<u>",SPL$Branch_Full_Name,"</u>")


popup_SPL_subgroup1<-paste(SPL_subgroup1$Branch_Full_Name)
popup_SPL_subgroup2<-paste(NOTSPL_subgroup1$Branch_Full_Name)

popup_label_RSEI<-paste("<b>",SEAtracts$COMPOSITE_QUINTILE,"</b><br>(Tract: ",SEAtracts$TRACT_INT,")<br>Source:<br>https://data.seattle.gov/Land-Base/Racial-and-Social-Equity-Composite-Index/q9y2-kzsi")

#Download and use the leaflet-groupedlayercontrol js to group layers in the menu
urlf <- 'https://raw.githubusercontent.com/ismyrnow/leaflet-groupedlayercontrol/gh-pages/dist/%s'

#Edit this script save temp location to your preference
#Download and use the leaflet-groupedlayercontrol js to group layers in the menu
urlf <- 'https://raw.githubusercontent.com/ismyrnow/leaflet-groupedlayercontrol/gh-pages/dist/%s'
download.file(sprintf(urlf,'leaflet.groupedlayercontrol.min.js'), 'C:/Temp/L.Control.groupedlayer.js', mode="wb")
download.file(sprintf(urlf,'leaflet.groupedlayercontrol.min.css'), 'C:/Temp/L.Control.groupedlayer.css', mode="wb")

#Decare a JS dependency that will be used in the output
ctrlGrouped <- htmltools::htmlDependency(
  name = 'ctrlGrouped',
  version = "1.0.0",
  # works in R and Shiny - download js/css files, then use this:
  src = c(file = normalizePath('C:/Temp')),
  script = "L.Control.groupedlayer.js",
  stylesheet = "L.Control.groupedlayer.css"
)
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

#BUILD THE LEAFLET MAP LAYERS
m<- leaflet() %>%
  setView(-122.3, 47.58, zoom = 12) %>%
  registerPlugin(ctrlGrouped) %>%
  # Add your custom JS logic here. The `this` keyword
  # refers to the Leaflet (JS) map object.
  addTiles() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "map2")%>%
  addProviderTiles(providers$CartoDB.Positron, group = "map")%>% 
  addPolygons(data=zipMap, weight = 1,fillColor = "grey", fillOpacity = zipMap$n_st/max(zipMap$n_st), color = "#555555",opacity = 1,popup =label_SPS_language,group = "Preferance contact language (SPS)",highlightOptions = highlightOptions(color = "white",weight = 2, bringToFront = FALSE)  )  %>%
  addPolygons(data=SEAtracts, weight = 1,fillColor = ~pal_POC(SEAtracts$PCT_PEOPLE_OF_COLOR), fillOpacity = 0.5, color = "#555555",opacity = 1,popup = popup_label_POC,group = "People of color (%)",highlightOptions = highlightOptions(color = "white",weight = 2, bringToFront = FALSE)  )  %>%
  addPolygons(data=SEAtracts, weight = 1,fillColor = ~pal_RSEI(SEAtracts$COMPOSITE_QUINTILE), fillOpacity = 0.5, color = "#555555",opacity = 1,popup = popup_label_RSEI,group = "Racial and Social Equity Composite Index",highlightOptions = highlightOptions(color = "white",weight = 2, bringToFront = FALSE)  )  %>%
  addMarkers(lng = SPS_priority$longitude, lat=SPS_priority$latitude, popup = popup_label_SPS, icon = SPS_school_point,group="SPS Prioritized") %>%
  addMarkers(lng = SPS_title1$longitude, lat=SPS_title1$latitude, popup = popup_label_SPS_title1, icon = SPS_school_point,group="SPS Title 1 (ES)") %>%
  addMarkers(lng = SPL$Longitude, lat=SPL$Latitude, popup = popup_label_SPL, icon = SPL_point_grey, group="SPL (Branch Locations)")%>%
  addMarkers(lng = SPL_subgroup1$Longitude, lat=SPL_subgroup1$Latitude, popup = popup_SPL_subgroup1, icon = SPL_point_green, group="HH",labelOptions = labelOptions(noHide = T,direction = "bottom", style = list(
    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
    "font-size" = "12px",
    "border-color" = "rgba(0,0,0,0.5)"))) %>% 
  addMarkers(lng = NOTSPL_subgroup1$Longitude, lat=NOTSPL_subgroup1$Latitude, popup = popup_SPL_subgroup2,icon = SPL_point, group="subgroup2",labelOptions = labelOptions(noHide = T)) %>%
      addLegend("bottomleft", colors = palette_color_RSEI ,group = "Racial and Social Equity Composite Index",labels = c("Lowest Priority / Most advantaged","Second lowest","Middle","Second highest","Highest priority/Most disadvantaged"),
            title = "Racial and Social Equity Composite Index", opacity = 1) %>%
  addLegend("bottomleft", colors = palette_color_POC ,group = "People of color (%)",labels = c("Lower % of population", "", "","","","Higher % of population"),
            title = "People of Color (% of population)", opacity = .5) %>%
  addLegend("bottomleft", colors = c('#808080','#9e9e9d','#b0b0af','#edf1ed') ,group = "Preferance contact language (SPS)",labels = c("More students", "","","Fewer students"),
            title = "Preferance contact language (SPS)", opacity = .5) %>%
  #addLayersControl(overlayGroups = c("SPL (Branch Locations)"), options = layersControlOptions(collapsed = TRUE))%>%  
  hideGroup(c("Racial and Social Equity Composite Index","SPS Prioritized", "Social Service / Non-profit", "People of color (%)","Racial and Social Equity Composite Index","SPS Title 1 (ES)","Preferance contact language (SPS)", "HH","subgroup2"
            ))%>% 
      clearControls() %>%
   addEasyButton(easyButton(
  icon="fa-globe", title="Zoom to SER",
  onClick=JS("function(btn, map){map.setView([47.57,-122.3]) ;}")))  %>%
  addEasyButton(easyButton(
    states = list(
      easyButtonState(
        stateName="A",
        icon="fa-question-circle", title="Source Information",
        onClick=JS('function(btn, map){
       var div = document.createElement("div")
       div.style.backgroundColor = "white";
       div.innerHTML = "<b>To request updates/edits, submit an issue.<br/>Last edited JULY 30, 2022.";
       div.style.font= "Arial";
       div.style.position = "absolute";
       div.style.opacity=".8";
       div.style.padding = "6px";
       div.style.left = "50px";
       div.style.top = "50px";
       div.style.height = "225x";
       div.style.width = "600px";
       document.getElementsByTagName("body")[0].appendChild(div).setAttribute("id", "sourceinfo");
        btn.state("B");                                 }')),
      easyButtonState(
        stateName="B",
        icon="fa-question-circle-o",
        title="Close Source info box",
        onClick = JS("
          function(btn, map) {
                var element = document.getElementById('sourceinfo');
              element.parentNode.removeChild(element);
            btn.state('A');
          }")
      )
    )
  )) %>% # and now some JS to handle the grouped layers
  onRender("function(el, x) {
         var groups = {
             Value31: this.layerManager.getLayerGroup('Racial and Social Equity Composite Index'),
             Value35: this.layerManager.getLayerGroup('SPS Prioritized'),
             Value36: this.layerManager.getLayerGroup('SPS Title 1 (ES)'),
              Value32: this.layerManager.getLayerGroup('People of color (%)'),
              Value41: this.layerManager.getLayerGroup('Preferance contact language (SPS)'),
              Value43: this.layerManager.getLayerGroup('HH'),
              Value44: this.layerManager.getLayerGroup('subgroup2'),
              Value42: this.layerManager.getLayerGroup('SPL (Branch Locations)')

            };
            
        var baseLayers = {
             'ESRI': this.layerManager.getLayerGroup('map2'),
            'CartoDB': this.layerManager.getLayerGroup('map'),

        };

        var groupedOverlays = {
            
            'Demographic': {
                   'Racial and Social Equity Composite Index': groups.Value31,
                   'People of color (%)': groups.Value32,
                    'Preferance contact language (SPS)': groups.Value41,
                    'none': L.layerGroup('')
                   
                   },

                    'School': {
                  'SPS Prioritized': groups.Value35,
                  'SPS Title 1 (ES)': groups.Value36,
      
                   },
           
                    'SPL': {
                  'SPL': groups.Value42,
                  'Subgroup1': groups.Value43,
                  'Subgroup2': groups.Value44
            
                   }
        };

        var Options = {
        exclusiveGroups:['Demographic'],
          groupCheckboxes: true,
          collapsed:false
        };

        console.log(L.control.groupedLayers);
        L.control.groupedLayers(baseLayers, groupedOverlays, Options).addTo(this);
        this.removeLayer(L.control.groupedLayers);
    }") 

#view map in viewer
m

htmlwidgets::saveWidget(m, "./output/YOUR_MAP.html", selfcontained = FALSE)
