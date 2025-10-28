seatrack_database_format_newDBformat <- function(df,species,colony){

all_tracks<-df
Species<-species
Colony<-colony

logger_info<-getLoggerInfo(asTibble = T)
logger_info<-logger_info[logger_info$deployment_species%in%Species & logger_info$colony%in%Colony,]
logger_info$id_column<-paste(logger_info$logger_serial_no,year(logger_info$deployment_date),year(logger_info$retrieval_date),sep="_")
all_tracks$id_column<-paste(all_tracks$logger_id,all_tracks$year_deployed,all_tracks$year_retrieved,sep="_")

library(dplyr)

all_tracks <- all_tracks %>%
  left_join(logger_info %>% dplyr::select(id_column, session_id), by = "id_column")

posdata_template<-as.data.frame(all_tracks$date_time)
colnames(posdata_template)<-c("date_time")
posdata_template$date_time<-as_datetime(posdata_template$date_time)
posdata_template$year_tracked<-all_tracks$year_tracked
posdata_template$session_id<-all_tracks$session_id
posdata_template$lon_raw<-all_tracks$lon_unsmooth
posdata_template$lat_raw<-all_tracks$lat_unsmooth
posdata_template$lon<-all_tracks$lon
posdata_template$lat<-all_tracks$lat
posdata_template$eqfilter<-all_tracks$eqfilter
posdata_template$tfirst<-all_tracks$tfirst
posdata_template$tsecond<-all_tracks$tsecond
posdata_template$twl_type<-all_tracks$twl_type
posdata_template$sun<-all_tracks$sun
posdata_template$light_threshold<-as.integer(all_tracks$light_threshold)
posdata_template$analyzer<-all_tracks$analyzer
posdata_template$data_version<-all_tracks$script_version

output<-posdata_template
return(output)
}
