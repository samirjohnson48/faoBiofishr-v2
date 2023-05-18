#' Format input dataset to be use by report_content()
#'
#' This function reformate input csv to be use by report_conten
#'
#' @param file Path to the input file
#' @return A csv
#' @export

data_formatting = function(file){

#Inport dataset
#fao<-read.csv(file)

#Validate format of dataset
#TODO

# #Convert wide table to long table
# fao_v <- fao[,!sapply(colnames(fao), startsWith, "S")]
# colnames(fao_v)[1:4] <- c("flag", "species", "f_area", "unit")
# tmp<-colnames(fao_v)
# fao_v <- melt(fao_v, id=c("flag", "species", "f_area", "unit"),value.name="capture",variable.name = "year")
# fao_v$year <- gsub('\\D','',fao_v$year)
# fao_s <- fao[,!sapply(colnames(fao), startsWith, "X")]
# colnames(fao_s)<- tmp
# fao_s <- melt(fao_s, id=c("flag", "species", "f_area", "unit"),value.name="info",variable.name = "year")
# fao_s$year <- gsub('\\D','',fao_s$year)
# fao_n <- merge(fao_v,fao_s)
# fao_n <- fao_n[!is.na(fao_n$flag),]
# fao_n <- fao_n[fao_n$flag!="",]
# fao_n <- fao_n[!is.na(fao_n$species),]
# fao_n <- fao_n[fao_n$species!="",]

fao_n<-read.csv(file)
fao_n<-fao_n[,-4]

names(fao_n)<-c("flag","species","f_area","year","capture","info")

#Be sure to keep 2 character length format to area code
fao_n$f_area<-sprintf("%02d", as.numeric(fao_n$f_area))

#Be sure to keep 3 character length format to flag code
fao_n$flag<-sprintf("%03d", as.numeric(fao_n$flag))

#Keep information about inland/marine area base on
fao_n$f_area_type <- ifelse(fao_n$f_area%in%c("01","02","03","04","05","06","07"),"inland","marine")


#ISSCAAP Group register
gr<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_GROUPS.csv")
#gr<-readr::read_csv(system.file("extdata","report_template.Rmd", package="faoBiofishr"))
gr<-subset(gr,select=c('3A_Code','Taxonomic_Code','Name_En','Major_Group_En','ISSCAAP_Group_En'))
names(gr)<-c('species','Taxonomic_Code','Name_En','Major_Group_En','ISSCAAP_Group_En')

data<-merge(fao_n,gr,all.x = T,all.y=F)

data<-subset(data,Major_Group_En %in% c("PISCES","MOLLUSCA","CRUSTACEA","INVERTEBRATA AQUATICA","PLANTAE AQUATICAE")) #Exclude of analysis ISSCAAP groups: Amphibia, reptilia and mammalia

data$Major_Group_En <- ifelse(data$Major_Group_En=="PISCES","Fishes",
                              ifelse(data$Major_Group_En=="MOLLUSCA","Molluscs",
                                     ifelse(data$Major_Group_En=="CRUSTACEA","Crustaceans",
                                            ifelse(data$Major_Group_En=="INVERTEBRATA AQUATICA","Aquatic Invertebrates",
                                                   ifelse(data$Major_Group_En=="PLANTAE AQUATICAE","Aquatic Plants",
                              data$Major_Group_En)))))

data$Major_Group_En<-factor(data$Major_Group_En,
                            levels = c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))



div<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_DIVISION.csv")
div<-subset(div,select=c('Identifier','Name_En'))
names(div)<-c('div_id','ISSCAAP_Division_En')

grp<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_GROUP.csv")
grp<-subset(grp,select=c('Identifier','Name_En'))
names(grp)<-c('grp_id','ISSCAAP_Group_En')


div_grp<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/HCL_FI_SPECIES_ISSCAAPDIV_ISSCAAPGRP.csv")
names(div_grp)<-c('div_id','grp_id')

dg<-merge(div,div_grp,all.x = T,all.y=T)
dg<-merge(dg,grp,all.x = T,all.y=T)
dg<-subset(dg,select = c("ISSCAAP_Group_En","ISSCAAP_Division_En"))

data<-merge(data,dg,all.x = T,all.y=F)

#Species register
sp<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ITEM.csv", col_names = T)
sp<-subset(sp,select=c('Alpha3_Code','Family_mapping','Order_mapping','Scientific_Name','Name_En'))
names(sp)<-c('species','family','order','scientific_name','sp_name_En')

data<-merge(data,sp,all.x = T,all.y=F)

#Ocean register
oc<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/waterarea/CL_FI_WATERAREA_MAJOR.csv", col_names = T)
oc<-subset(oc,select=c('Code','Name_En'))
names(oc)<-c('f_area','ocean')
oc$ocean<-gsub("(.*),.*", "\\1", oc$ocean)
oc$ocean<-ifelse(oc$f_area%in%c("01","02","03","04","05","06","07"),NA,oc$ocean)

data<-merge(data,oc,all.x = T,all.y=F)

#FAO Major Area register
ar<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/waterarea/CL_FI_WATERAREA_MAJOR.csv", col_names = T)
ar<-subset(ar,select=c('Code','Name_En'))
names(ar)<-c('f_area','f_area_name')

data<-merge(data,ar,all.x = T,all.y=F)
data$f_area_label<-paste0(data$f_area_name," [",data$f_area,"]")

#FAO Countries register
ct<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_M49.csv", col_names = T)
ct<-subset(ct,select=c('UN_Code','Name_En','ISO3_Code'))
names(ct)<-c('flag','flag_name','flag_iso')

data<-merge(data,ct,all.x = T,all.y=F)

#Data filter
data <- data %>%
  filter(capture != 0|(capture ==0 & info =="N"))

data <- data%>%
  filter(!(f_area_type=="marine"&ISSCAAP_Division_En=="Freshwater fishes"))

  data <- data%>%
    filter(!(f_area_type=="inland"&ISSCAAP_Division_En=="Marine fishes"))

data$Major_Group_En<-factor(data$Major_Group_En)

return(data)
}
