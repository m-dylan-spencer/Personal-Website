Crime and Place Course: Intro to Crime Maps in R
================

## GitHub Documents

This is part of a lecture given to the Crime and Place course at the
University of South Carolina. Topics covered are spatial data handling
and introductory mapping.

## Including Code

You can include R code in the document as follows:

    ibrary(tidyverse)
    library(lubridate)
    library(sf)
    library(tmap)
    library(pdftools)
    library(RSocrata)
    
    socrata.file <-"https://data.cityofchicago.org/resource/qzdf-xmn8.csv"
    crime.2020 <- read.socrata(socrata.file)
    head(crime.2020)
    tail(crime.2020)
    dim(crime.2020)
    class(crime.2020$Date)
    
    #filter data
    
    crime.jj <- crime.2020 %>% filter(Date < as.POSIXct("2020-07-31 23:59:59"))
    dim(crime.jj)
    
    #or
    
    crime.jj <- crime.2020 %>% filter(Date >= as.POSIXct("2020-01-01 00:00:00")
                                      & Date <= as.POSIXct("2020-07-31 23:59:59"))
    dim(crime.jj)
    head(crime.jj)
    tail(crime.jj)
    
    rob <- filter(crime.jj, Primary.Type == "ROBBERY")
    dim(rob)
    
    names(rob)
    rob.filter <- rob %>% select(comm = Community.Area,
                                 lat = Latitude, lon = Longitude)
    head(rob.filter)
    dim(rob.filter) 
    
    #creating point object
    
    rob.coord <- rob.filter %>% filter(!(is.na(lon))) #getting rid of null values
    dim(rob.filter)
    
    rob.points = st_as_sf(rob.coord, coords = c("lon", "lat"), crs = 4326
                          , agr = "constant")
    
    class(rob.points)
    plot(rob.points)
    st_crs(rob.points)
    
    #community data
    
    comm.file <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson"
    chicago.comm <- read_sf(comm.file)
    class(chicago.comm)
    st_crs(chicago.comm)
    plot(chicago.comm)
    head(chicago.comm)
    
    chicago.comm <- st_transform(chicago.comm,32616)
    st_crs(chicago.comm)
    rob.points <- st_transform(rob.points, 32616)
    st_crs(rob.points)
    
    #spatial join
    
    comm.pts <- st_join(rob.points,chicago.comm["area_num_1"])
    head(comm.pts)
    comm.pts2 <- comm.pts %>% filter(!(is.na(area_num_1)))
    dim(comm.pts)
    dim(comm.pts2)
    
    is.numeric(comm.pts$area_num_1)
    comm.pts$area_num_1 <- as.integer(comm.pts$area_num_1)
    is.integer(comm.pts$area_num_1)
    chicago.comm$area_num_1 <- as.integer(chicago.comm$area_num_1)
    
    st_geometry(comm.pts) <- NULL
    class(comm.pts)
    
    rob.cnts <- comm.pts %>% count(area_num_1)
    head(rob.cnts)
    plot(rob.cnts)
    
    rob.cnts <- rob.cnts %>% rename(comm = area_num_1, AGG.COUNT = n)
    head(rob.cnts)
    
    chicago.comm <- left_join(chicago.comm,rob.cnts, by = c("area_num_1" = "comm"))
    head(chicago.comm)
    
    tm_shape(chicago.comm) +
      tm_polygons("AGG.COUNT")
    
    comm <- chicago.comm %>% filter(!is.na(AGG.COUNT)) #encountered some missing values from above
    
    tm_shape(comm) +
      tm_polygons("AGG.COUNT") #community area 9 has no reported robbery and is removed
    # logic is no break includes a zero instance and may be overrepresented by the map
    
    tmap_mode("view") #interactive viewing mode for map
    ttm() #switch back to plot mode
    tm_shape(comm) +
      tm_polygons("AGG.COUNT", style = "fixed", palette = "Reds", #used colorbrewer2 for pallete
                  breaks = c(1, 76, 152, 228, 304, 380))
    
    
    pdf.file <- "https://www.cityofchicago.org/content/dam/city/depts/zlup/Zoning_Main_Page/Publications/Census_2010_Community_Area_Profiles/Census_2010_and_2000_CA_Populations.pdf"
    pop.dat <- pdf_text(pdf.file)
    class(pop.dat)
    length(pop.dat)
    length(pop.dat[[1]])
    nnlist <- ""
    nnlist
    ppage <- strsplit(pop.dat[[1]],split="\n")
    ppage[[1]]
    nni <- ppage[[1]]
    nni <- nni[-(1:4)]
    nni
    nnu <- unlist(nni)
    nnlist <- c(nnlist,nnu)
    nnlist
    nnlist <- ""
    for (i in 1:2) {
      ppage <- strsplit(pop.dat[[i]],split="\n")
      nni <- ppage[[1]]
      nni <- nni[-(1:4)]
      nnu <- unlist(nni)
      nnlist <- c(nnlist,nnu)
    }
    nnlist
    nnlist <- nnlist[2:(length(nnlist)-1)]
    
    # extracting population values
    
    nnpop <- vector(mode="numeric",length=length(nnlist))
    for (i in (1:length(nnlist))) {
      popchar <- substr(nnlist[i],start=27,stop=39)
      popval <- as.numeric(gsub(",","",popchar))
      nnpop[i] <- popval
    }
    nnpop
    
    # creating a data frame with population values
    
    nnid <- (1:length(nnlist))
    nnid
    neighpop <- data.frame(as.integer(nnid),nnpop)
    names(neighpop) <- c("NID","POP2010")
    head(neighpop)
    
    ### Mapping community area robbery per cap
    #using comm instead of chicago.comm
    
    comm <- left_join(comm, neighpop, by = c("area_num_1" = "NID"))
    head(comm)
    comm <- comm %>% mutate(robpcap = (AGG.COUNT / POP2010) * 1000) 
    head(comm)
    
    # final choropleth map
    
    tm_shape(comm) + 
      tm_bubbles(size = "robpcap", co = "blueviolet") +
      tm_layout(bg.color = "black", inner.margins = c(0, .02, .02, .02)) #just messing around
    
    tmap_mode("view") #interactive viewing mode for map
    ttm() #switch back to plot mode
    
    tm_shape(comm) +
      tm_polygons("robpcap", style = "fixed", palette = "Reds", #used colorbrewer2 for pallete
                  breaks = c(0.1, 1.86, 3.72, 5.58, 7.44, 9.4))
    
    #using these summary stats is how I determined breaks for maps above
    comm.sum <- as_tibble(comm)
    comm.sum %>%
      summarise(
        count = n(),
        max_agg = max(AGG.COUNT, na.rm = TRUE),
        min_agg = min(AGG.COUNT, na.rm = TRUE))
    
    comm.sum %>%
      summarise(
        count = n(),
        max_pcap = max(robpcap, na.rm = TRUE),
        min_pcap = min(robpcap, na.rm = TRUE))
    
    379/5
    9.33/5
    
    # carto map
    
    carto.dorling <- cartogram_dorling(comm, "robpcap")
    class(carto.dorling)
    
    tm_shape(carto.dorling) +
      tm_fill("robpcap") +
      tm_borders()
    
    carto.cont <- cartogram_cont(comm, "robpcap")
    
    tm_shape(carto.cont) + tm_fill("robpcap") + tm_borders()
    
    carto.ncont <- cartogram_ncont(comm,"robpcap") 
    tm_shape(carto.ncont) +
      tm_fill("robpcap") + tm_borders()

## Including Plots

You can also embed plots, for example:

![](Mapping_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
