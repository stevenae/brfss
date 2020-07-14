bg <- read.csv('~/Downloads/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2019_release.csv')
names(bg)

library(sf)
bg02 <- st_read("~/Downloads/BRFSS2002/brfss_mmsa_2002_download.shp",stringsAsFactors=F)
plot(bg02['tax_distri'],border=rgb(0, 0, 0, 0))

pdd <- readxl::read_xls('~/Downloads/BRFSS2002/prevalence_data_dictionary_2002.xls')
mdd <- readxl::read_xls('~/Downloads/BRFSS2002/mmsa_data_dictionary_2002.xls')
names(bg02)[!(names(bg02) %in% paste0('X',unique(c(pdd$ColumnName,mdd$ColumnName))))]

