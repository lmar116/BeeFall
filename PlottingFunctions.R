sessionInfo()
#R version 4.2.2 (2022-10-31 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22621)

#attached packages:      
#[1] RColorBrewer_1.1-3 
#[2] leaflet_2.1.1       
#[3] sf_1.0-9            
#[4] rgdal_1.6-4 

#_____________________________________________________________________________________________________________________________________________
library(leaflet
library(dplyr)
library(RColorBrewer)
library(rgdal)
luibrary(sf)

#_____________________________________________________________________________________________________________________________________________
sfs <- c(
  "Distribution (Wallacean)" = "Distribution",
  "Coverage (Wallacean)" = "Coverage",
  "Checklist (Wallacean)" = "Checklist",
  "Trait Incompleteness (Raunkiærian)" = "TraitsC",
  "Missing Traits (Raunkiærian)" = "TraitsM",
  "Population (Prestonian)" = "Population",
  "Data Deficient (Prestonian)" = "Deficient",
  "Genes (Darwinian)" = "Genes",
  "Interactions (Eltonian)" = "Interactions",
  "Photos Flickr (Keartonian)" = "Photosf",
  "Photos iNat (Keartonian)" = "Photosi",
  "Descriptions (Linnean)"= "Description"
)

sfstable <-
  data.frame(
    Name = c(
      "Distribution",
      "Coverage",
      "Checklist",
      "TraitsC",
      "TraitsM",
      "Population",
      "Deficient",
      "Genes",
      "Interactions",
      "Photosf",
      "Photosi",
      "Description"
    ),
    Longname = c(
      "Distribution (Wallacean)",
      "Coverage (Wallacean)" = "Coverage",
      "Checklist (Wallacean)" = "Checklist",
      "Trait Incompleteness (Raunkiærian)" = "TraitsC",
      "Missing Traits (Raunkiærian)" = "TraitsM",
      "Population (Prestonian)" = "Population",
      "Data Deficient (Prestonian)" = "Deficient",
      "Genes (Darwinian)",
      "Interactions (Eltonian)",
      "Photos Flickr (Keartonian)" = "Photosf",
      "Photos iNat (Keartonian)" = "Photosi",
      "Descriptions (Linnean)"
    ),
    Shortfall = c(
      "Wallacean",
      "Wallacean",
      "Wallacean",
      "Raunkiærian",
      "Raunkiærian",
      "Prestonian",
      "Prestonian",
      "Darwinian",
      "Eltonian",
      "Keartonian",
      "Keartonian",
      "Linnean"
    ),
    Description = c(
      "Predicted Sampling",
      "Coverage",
      "Checklist",
      "Trait Completeness",
      "Missing Trait",
      "Population Trend",
      "Red List (DD)",
      "COI Barcode",
      "Interaction",

      "Photos (Flickr)",
      "Photos (iNaturalist)",
      "Species Description"
    )
  )

#_____________________________________________________________________________________________________________________________________________
tempcol <- colorRampPalette(
  c(
          "blue",
          "turquoise3",
          "chartreuse4",
          "yellowgreen",
          "gold2",
          "orange",
          "firebrick2",
          "tomato4"
  )
)

monocol <- colorRampPalette(
  c(
             "#FAF7F5",
             "#FBEEE6",
             "#F6DDCC",
             "#EDBB99",
             "#E59866",
             "#DC7633",
             "#D35400",
             "#BA4A00",
             "#A04000",
             "#873600",
             "#6E2C00",
             "#381701"
  )
)

#_____________________________________________________________________________________________________________________________________________
plotmapfunc <- function(layer, type = "Other", colour = tempcol, legend= "On") {
  # #value as to whether == distribution map
  # grepval <- as.integer(grepl("dist", deparse(substitute(layer))))
  
  if (type == "Distribution") {
    labels <-
      sprintf(
        "<strong>%s</strong><br/>%s%% comparatively undersampled",
        layer$name,
        round(layer$sr, 0)
      ) %>% lapply(htmltools::HTML)
    title= "Comparative Rank of Undersampling (%)"
  }
  
  if (type == "Checklist") {
    labels <-
      sprintf(
        "<strong>%s</strong><br/>%s%% of checklist missing distribution data",
        layer$name,
        round(layer$sr, 0)
      ) %>% lapply(htmltools::HTML)
    title="Percentage of checklist missing distribution data"
  }
  
  if (type == "Description") {
    labels <-
      sprintf(
        "<strong>%s</strong><br/>%s%% rate of new species descriptions since 1990",
        layer$name,
        round(layer$sr, 2)
      ) %>% lapply(htmltools::HTML)
    title="Rate of new species descriptions since 1990 (%)"
  }
  
  if (type == "Coverage") {
    labels <-
      sprintf(
        "<strong>%s</strong><br/>%s%% sample incompleteness",
        layer$name,
        round(layer$sr, 2)
      ) %>% lapply(htmltools::HTML)
    title="Average sample incompleteness (inverse coverage) (%)"
  }
  
  if (type == "TraitsC") {
    labels <-
      sprintf(
        "<strong>%s</strong><br/>%s%% trait incompleteness",
        layer$name,
        round(layer$sr, 2)
      ) %>% lapply(htmltools::HTML)
    title="Average missing trait data (%)"
  }
  
  
  if (type != "Distribution" & type != "Checklist"& type != "Description"& 
      type != "Coverage"& type != "TraitsC") {
    labels <-
      sprintf("<strong>%s</strong><br/>%s species missing data",
              layer$name,
              layer$sr) %>% lapply(htmltools::HTML)
    title="Number of species"
  }
  
  
  
  factpal <- colorNumeric(colour(length(layer$sr)), layer$sr)
  
  
  leaflet(options = leafletOptions(zoomControl = T,
                                   minZoom = 4)) %>%
    setView(11.5 , 55, 3.5) %>%
    #addProviderTiles(providers$Stamen.Toner) %>%
    #addProviderTiles("Esri.WorldImagery") %>%
    addProviderTiles("Esri.WorldTopoMap",
                     group = "Topo") %>%
    #addProviderTiles("CartoDB.Positron",     group = "CartoDB") %>%
    addPolygons(
      data = layer,
      color = "grey",
      weight = 1,
      smoothFactor = 0,
      highlightOptions = highlightOptions(
        color = "white",
        weight = 4,
        bringToFront = TRUE
      ),
      fillColor = ~ factpal(layer$sr),
      layerId = ~ ID,
      fillOpacity = 0.5,
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    #if(legend=="On"){
    addLegend(
      pal = factpal,
      values = layer$sr,
      opacity = 0.7,
      title = title,
      position = "bottomright"
    )
  #}
}

#_____________________________________________________________________________________________________________________________________________
plotmapfunc.bivar <- function(layer, 
                              var1_name, 
                              var2_name, 
                              ntiles = 4,
                              var1_label = NA, 
                              var2_label = NA,
                              region_names = NA,
                              paletteFunction = pals::arc.bluepink){

#get correct axis labels including description from sfstable
  if (is.na(var1_label)) var1_label <- rlang::enexpr(var1_name)
  if(is.na(var2_label))  var2_label <- rlang::enexpr(var2_name)
    
  var1_lab <- as.character(sfstable %>%  filter(Name==var1_label) %>% dplyr::select(Description))
  var2_lab <- as.character(sfstable %>%  filter(Name==var2_label) %>% dplyr::select(Description))
  
  var1_pal_label <- paste0(var1_lab," Gaps", " \U2192")
  var2_pal_label <- paste0(var2_lab," Gaps", " \U2192")

  bivar_pal <- function(x) paletteFunction(n=ntiles^2)[x]
  
  forplot <- layer %>%
    dplyr::rename (var1 = {{var1_name}},
                   var2 = {{var2_name}}) %>%
    dplyr::mutate(var1_ntile = dplyr::ntile(var1, n = ntiles),
                  var2_ntile = dplyr::ntile(var2, n = ntiles),
                  pal_num = var1_ntile + (var2_ntile - 1)*ntiles,#(ntiles -var1_ntile)*3 + var2_ntile,
                  pal_colour = bivar_pal(pal_num))
  
  if (!is.na(region_names)) forplot <- dplyr::rename(forplot,
                                                     region_name_label = region_names)

  palette_size_px <- 120
  swatch_size_px <- round(palette_size_px / ntiles)
  
  row_col_px <- rep(paste0(swatch_size_px,"px"), times = ntiles) %>%
    stringr::str_flatten(collapse = " ") %>%
    paste0(., ";")
  
  div_var1 <- paste0('<div class = "var1-label" style="grid-row-start:1; grid-row-end:',(ntiles+1),'; text-align: center; writing-mode: tb-rl;
        transform: rotate(-180deg);">',var1_pal_label,'</div>')
  div_var2 <- paste0('<div style="text-align:center; grid-column:2 / ',(ntiles+2),';">',var2_pal_label,'</div>')
  
  # set up the indices for the palette
  div_indices <- matrix((1:ntiles^2),
                        nrow=ntiles,
                        ncol = ntiles,
                        byrow = TRUE)
  
  div_indices <- div_indices[,c(ntiles:1)]
  
  # set up the divs for the palette squares
  divs <- paste0('<div style="background-color:',bivar_pal(div_indices),
                 '; color:',bivar_pal(div_indices),
                 ';">',div_indices,' </div>') %>%
    stringr::str_flatten()
  
  # combine the above bits with a css grid wrapper for the html palette
  palette_html <- paste0(
    '<style> .grid-container { display: grid;
    grid-template-columns: 40px ',row_col_px,
    'grid-auto-rows: ',row_col_px,' 40px;','}
    </style>
    <div class="grid-container">',
    div_var1,
    divs,
    div_var2,
    '</div>')
  
  #get full name of shortfall
  st1 <- sfstable %>% filter(Name==var1_label)
  Shortfall1 <- st1$Shortfall
  st2 <- sfstable %>% filter(Name==var2_label)
  Shortfall2 <- st2$Shortfall
  
  vl1 <- paste(Shortfall1, "Shortfall")
  vl2 <- paste(Shortfall2, "Shortfall")
  
  if (var1_label == "Distribution") {
    labfillvl1 <- sprintf("%s%% comparatively undersampled",
                          round(forplot$var1, 0))
  }
  if (var2_label == "Distribution") {
    labfillvl2 <- sprintf("%s%% comparatively undersampled",
                          round(forplot$var2, 0))
  }
  
  if (var1_label == "Checklist") {
    labfillvl1 <- sprintf("%s%% of checklist missing distribution data",
                          round(forplot$var1, 0))
  }
  if (var2_label == "Checklist") {
    labfillvl2 <- sprintf("%s%% of checklist missing distribution data",
                          round(forplot$var2, 0))
  }
  
  if (var1_label == "Description") {
    labfillvl1 <- sprintf("%s%% rate of new species descriptions since 1990",
                          round(forplot$var1, 2))
  }
  if (var2_label == "Description") {
    labfillvl2 <- sprintf("%s%% rate of new species descriptions since 1990",
                          round(forplot$var2, 2))
  }
  if (var1_label == "Coverage") {
    labfillvl1 <- sprintf("%s%% of sampling incompleteness",
                          round(forplot$var1, 0))
  }
  if (var2_label == "Coverage") {
    labfillvl2 <- sprintf("%s%% of sampling incompleteness",
                          round(forplot$var2, 0))
  }
  
  if (var1_label == "TraitsC") {
    labfillvl1 <- sprintf("%s%% of trait incompleteness",
                          round(forplot$var1, 2))
  }
  if (var2_label == "TraitsC") {
    labfillvl2 <- sprintf("%s%% of trait incompleteness",
                          round(forplot$var2, 2))
  }
  
  
  
  if (var1_label != "Distribution" & var1_label != "Checklist"& var1_label != "Description"
      & var1_label != "Coverage"& var1_label != "TraitsC") {
    labfillvl1 <- sprintf("%s species missing data",
                          forplot$var1)
  }
  
  if (var2_label != "Distribution" & var2_label != "Checklist"& var2_label != "Description"
      & var2_label != "Coverage"& var2_label != "TraitsC") {
    labfillvl2 <- sprintf("%s species missing data",
                          forplot$var2)
  }
  
  labs <- paste0("<b>",vl1,"</b> ",
                 "<br>#Tile: ", forplot$var1_ntile,
                 "<br>",labfillvl1  %>% lapply(htmltools::HTML),
                 "<br><b>",vl2,"</b> ",
                 "<br>#Tile: ",forplot$var2_ntile,"</b> ",
                 "<br>",labfillvl2  %>% lapply(htmltools::HTML))
  
  
  if(!is.na(region_names)) labs <- paste0("<b>",forplot$region_name_label,"</b><br>",labs)
  
  labs <- purrr::map(labs, htmltools::HTML)
    
  leaflet(options = leafletOptions(zoomControl = T,
                                   minZoom = 4)) %>%
    setView(11.5 , 55, 3.5) %>%
    addProviderTiles("Esri.WorldTopoMap",
                     group = "Topo") %>%
    leaflet::addPolygons(data = forplot,
                         label = labs,
                         fillColor = ~pal_colour,
                         color = "grey",
                         weight = 1,
                         smoothFactor = 0,
                         fillOpacity = 0.5,
                         highlightOptions = highlightOptions(
                           color = "white",
                           weight = 4,
                           bringToFront = TRUE
                         ),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"
                         )
    )%>%
    addControl(
      html = palette_html,
      position = "bottomright",
    )
}
