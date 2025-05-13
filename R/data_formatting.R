#' Format input dataset to be use by report_content()
#'
#' This function reformat input csv to be use by report_content
#'
#' @param file Path to the input file
#' @return A csv
#' @export

data_formatting = function(file){
# Import data from Fishstat library
# Use table 'capture' which gives global capture production
fao_n <- capture %>%
  rename(flag = country,
         f_area = area,
         capture = value,
         unit = measure,
         info = status) %>%
  select(c("flag","species","f_area","unit", "year","capture","info"))

fao_n <- fao_n[!is.na(fao_n$year),]
fao_n <- fao_n[!is.na(fao_n$flag),]
fao_n <- fao_n[fao_n$flag!="",]
fao_n <- fao_n[!is.na(fao_n$species),]
fao_n <- fao_n[fao_n$species!="",]


#Be sure to keep 2 character length format to area code
fao_n$f_area<-sprintf("%02d", as.numeric(fao_n$f_area))

#Be sure to keep 3 character length format to flag code
fao_n$flag<-sprintf("%03d", as.numeric(fao_n$flag))

#Keep information about inland/marine area base on
fao_n$f_area_type <- ifelse(fao_n$f_area%in%c("01","02","03","04","05","06","07"),"inland","marine")

#ISSCAAP Group register
gr<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_GROUPS.csv")
#gr<-readr::read_csv(system.file("extdata","report_template.Rmd", package="faoBiofishr"))
gr<-subset(gr,select=c('3A_Code','Taxonomic_Code','Name_En','Major_Group','ISSCAAP_Group_En'))
names(gr)<-c('species','Taxonomic_Code','Name_En','Major_Group','ISSCAAP_Group_En')

data<-merge(fao_n,gr,all.x = T,all.y=F)

data<-subset(data,Major_Group %in% c("PISCES","MOLLUSCA","CRUSTACEA","INVERTEBRATA AQUATICA","PLANTAE AQUATICAE")) #Exclude of analysis ISSCAAP groups: Amphibia, reptilia and mammalia

data$Major_Group <- ifelse(data$Major_Group=="PISCES","Fishes",
                              ifelse(data$Major_Group=="MOLLUSCA","Molluscs",
                                     ifelse(data$Major_Group=="CRUSTACEA","Crustaceans",
                                            ifelse(data$Major_Group=="INVERTEBRATA AQUATICA","Aquatic Invertebrates",
                                                   ifelse(data$Major_Group=="PLANTAE AQUATICAE","Aquatic Plants",
                              data$Major_Group)))))

data$Major_Group<-factor(data$Major_Group,
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

#Cast proper data types
data$Major_Group<-factor(data$Major_Group)
data$year <- as.numeric(data$year)

return(data)
}
