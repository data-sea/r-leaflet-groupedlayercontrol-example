# r-leaflet-groupedlayercontrol-example
This R code demonstrates adding leaflet-groupedlayercontrol that extend layer grouping and selection. This provides a ton of flexibility in determining which layers are shown. Have a look at the output folder if you would like to view the interactive map  in a browser without running code. 

To output, I make use of `htmlwidgets::saveWidget()` which saves the leaflet map to HTML along with various supporting files. This makes it easy to throw the resulting map up on a web server.

# Why groupedlayercontrol?
The [leaflet-groupedlayercontrol](https://github.com/ismyrnow/leaflet-groupedlayercontrol) add-on provides a nice solution for grouping overlay layers. You can:
- Create exclusive group (handy for only showing just one choropleth overlay at a time,
- Create multiple grouped layers. 
- Ability to have multiple group titles shown in the layer menu.
- Able to toggle all group layers on or off with a single checkbox click. 
- Able to toggle individual layers on or off.

# Screenshots
<div> Closeup of the grouped layer control menu</div>
  <img src="https://github.com/data-sea/r-leaflet-groupedlayercontrol-example/blob/d676f46c9fc89bb764c6f875e5ff499ba83baebc/images/screenshot1.PNG"></img>

<div> Screenshot showing part of map and layers selected</div>
  <img src="https://github.com/data-sea/r-leaflet-groupedlayercontrol-example/blob/d676f46c9fc89bb764c6f875e5ff499ba83baebc/images/screenshot2.PNG"></img>

