#########WIMS functions##########
##
#  Kieran Khamis 230325
#
#
################################

library(tidyverse)
library(httr2)
library(jsonlite)
library(tidyplots)
library(ggsci)
library(leaflet)
library(htmltools)
library(sf)
library(terra)

###############################
#load EA info files

file_paths<-list.files(path = "EA_files/", full.names=TRUE)

catch<-st_read("Shapefiles/WFD_Surface_Water_Management_Catchments_Cycle_3.shp" )

file_paths_s<-file_paths|> str_split(pattern = "/",simplify = T)

EA_codes<-tibble(files=file_paths, properties= file_paths_s[,2])

EA_codes$info <- file_paths |> 
                map(read_csv)
####Define key areas and codes

variables_WFD<-c("Alky pH 4.5" , "Ammonia(N)"  , "Cond @ 20C"  , "BOD ATU", "NH3 un-ion"  ,
                 "Nitrate-N", "Orthophospht", "Oxygen Diss",  "pH",  "O Diss %sat", "Temp Water" )

codes <-c("0076","0180","0111","9901","0085","0062","9924","0061")


base<-"https://environment.data.gov.uk/water-quality/id/sampling-point/"

###REM sites
Blythe<-"MD-61648150"
Tame_nr<-"MD-59014180"
Tame_wb<-"MD-59014950"

#################################################################

wims_api_all <-function(base,n,site){
  raw <- request(base_url = base) |> 
    req_url_path_append(paste(site),paste("measurements?_limit=",n,sep = "")) |> 
    req_perform()
  
  foo <- fromJSON(rawToChar(raw$body),simplifyDataFrame = T)
  foo$date<-parse_date_time(foo$items$sample$sampleDateTime, "ymd HMS")
  foo$vars<-foo$items$determinand$label
  foo$vars_code<-foo$items$determinand$notation
  foo$val<-foo$items$result
  foo$easting<-foo$items$sample$samplingPoint$easting
  foo$northing<-foo$items$sample$samplingPoint$northing
  
  df<-tibble(date=foo$date,vars=foo$vars,var_code = foo$vars_code ,
             val=foo$val,easting = foo$easting , 
             northing = foo$northing)
  return(df)
}

wims_api_vars <-function(base,n,site,vars){
  raw <- request(base_url = base) |> 
    req_url_path_append(paste(site),
                        paste("measurements?_limit=",n,"&determinand=",codes[1],
                              "&determinand=" ,codes[2] ,"&determinand=",codes[3], 
                              "&determinand=" ,codes[4] ,"&determinand=",codes[5], 
                              "&determinand=" ,codes[6] ,"&determinand=",codes[7],
                              "&determinand=" ,codes[8] , sep = "")) |> 
    req_perform()
  foo <- fromJSON(rawToChar(raw$body),simplifyDataFrame = T)
  foo$date<-parse_date_time(foo$items$sample$sampleDateTime, "ymd HMS")
  foo$vars<-foo$items$determinand$label
  foo$val<-foo$items$result
  foo$easting<-foo$items$sample$samplingPoint$easting
  foo$northing<-foo$items$sample$samplingPoint$northing
  df<-tibble(date=foo$date,vars=foo$vars,val=foo$val,
             easting = foo$easting , northing = foo$northing)
  df<-df|> mutate(yr=year(date),month= month(date))
  
  return(df)
}




#########Load open rivers data####################################

catchments<-readRDS(file = "test_catchments.rds")

O_rivers_info<- st_read("../ORN_v2.gpkg")

O_rivers_layers<-st_layers("../ORN_v2.gpkg")

#test_catchments<- st_read("../ORN_v2.gpkg", layer = "ORN_Catchments") 
O_rivers_points<- st_read("../ORN_v2.gpkg", layer = "ORN_Sampled_100m") 


#writeRDS(O_rivers_points,"O_rivers_points.rds")

##reorder based on sample id

open_catchments<-dplyr::arrange(test_catchments,SampleID)

############Read landcover raster

lcm_2023<-terra::rast(x = "C:\\Users\\khamisk\\Dropbox\\LCM_2023_10m\\lcm-2023-10m_5816930\\gblcm2023_10m.tif")

Catch_Tame<-catch |> filter(MNCAT_NAME=="Tame Anker and Mease"|MNCAT_NAME=="Trent Valley Staffordshire" |
                              MNCAT_NAME== "Dove") 

lcm_2023_Tame <- terra::crop(lcm_2023, Catch_Tame, mask=TRUE)   #clip raster 

rast<-lcm_2023_Tame[[1]]

coords <- xyFromCell(rast, seq_len(ncell(rast)))
dat <- terra::values(rast, dataframe = TRUE)
dat <- cbind(coords, dat)
names(dat) <- c("x", "y", "value")

dat$value[dat$value==0]<-NA  


dat<-dat[complete.cases(dat),]


ggplot(data = dat) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = LCcolors,
                    labels = LCNames,
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void() + theme(legend.text = element_text(size = 8), 
                       legend.title = element_text(size = 10),legend.title.position = "top",
                       legend.position="bottom",legend.key.height= unit(0.2  , 'cm'))

########## load upper Tame ###############



api_upTame<-
  request(base_url = "https://environment.data.gov.uk/water-quality/id") |>  
  req_url_path_append(paste("sampling-point?_limit=5000&subArea=2-29-B")) |>  
  req_perform()


df_Tame<-fromJSON(rawToChar(api_upTame$body),simplifyDataFrame = T)

df_Tame$items$samplingPointType$label

FW_samples<-df_Tame$items$samplingPointType$label=="FRESHWATER - RIVERS"

df_Tame_FW<-df_Tame$items[FW_samples,]

df_Tame_FW<-df_Tame_FW[df_Tame_FW$samplingPointStatus$label=="open",]

###Leaflet view##############

leaflet(df_Tame_FW) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~htmlEscape(label))

##Create sf object

df_Tame_sf<-st_as_sf(df_Tame_FW, coords = c("easting", "northing"), crs = 27700, agr = "constant")

ggplot(df_Tame_sf)+geom_sf()+theme(legend.position = "none")



catch |> filter(MNCAT_NAME=="Tame Anker and Mease"|MNCAT_NAME=="Trent Valley Staffordshire" |MNCAT_NAME== "Dove") |> 
  ggplot()+geom_sf()+theme(legend.position = "none")+geom_sf(data=df_Tame_sf, colour= "red")+
  theme(legend.position = "none")

###filter
Tame_catch <-catch |> filter(MNCAT_NAME=="Tame Anker and Mease"|MNCAT_NAME=="Trent Valley Staffordshire" |MNCAT_NAME== "Dove") 
Tame_points_OR<-st_intersection(Tame_catch,test_points)
Tame_points_OR<-st_intersects(Tame_catch,test_points,sparse = T)

Tame_catch|> 
  ggplot()+geom_sf()+theme(legend.position = "none")+geom_sf(data=Tame_points_OR, colour= "red")+
  theme(legend.position = "none")+geom_sf(data=df_Tame_sf, colour= "black")



########NEW CODE to link datasets based on distance!

nearest_Tame<-st_is_within_distance(Tame_points_OR, df_Tame_sf, 
                                           dist = units::set_units(70, "m"))

nearest_Tame_join<-st_join(df_Tame_sf, Tame_points_OR,st_is_within_distance, 
        dist = units::set_units(70, "m"))


ID<-nearest_Tame_join$SampleID

Tame_catchments<- test_catchments |> filter(SampleID %in% ID) 
