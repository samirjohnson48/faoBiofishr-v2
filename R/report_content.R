#' Compute content of report
#'
#' This function compute all the output elements to include of report
#'
#' @param file Path to the input file
#' @param out Path to export destination
#' @return A list of element
#' @export

report_content<-function(out=NULL){

  #Data cleaning
  content<-list()

  #Output repository creation
  path<-if(is.null(out)) getwd() else out
  report_folder<-file.path(path,"BIODIVERSITY_TREND_REPORT")
  report_version<-file.path(report_folder,Sys.Date())
  report_output<-file.path(report_version,"OUTPUT")
  fig_path<-file.path(report_output,"FIGURES")
  tab_path<-file.path(report_output,"TABLES")
  report_path<-file.path(report_output,"REPORT")

  content$path$fig<-fig_path
  content$path$tab<-tab_path
  content$path$report<-report_path

  dir.create(report_output,recursive = T,showWarnings = F)
  dir.create(fig_path,showWarnings = F)
  dir.create(tab_path,showWarnings = F)
  dir.create(report_path,showWarnings = F)

  data<-data_formatting(file)
  write.table(data,file.path(tab_path,"structured_dataset.csv"),row.names = FALSE,quote=FALSE,sep="\t")

  #Graphical rules
  ## ISSCAAP Group colors
  ISSCAAPColors <- as.character(c("Fishes"="#AED6F1",
                                  "Molluscs"="#D7BDE2",
                                  "Crustaceans"="#F5B7B1",
                                  "Aquatic Invertebrates"="#F9E79F",
                                  "Aquatic Plants"="#ABEBC6"))
  ISSCAAPScale <- scale_fill_manual(name="Major_Group", values=ISSCAAPColors)

  ISSCAAPGroupLvl <- c("Freshwater fishes",
                "Carps, barbels and other cyprinids",
                "Tilapias and other cichlids",
                "Miscellaneous freshwater fishes",

                "Diadromous fishes",
                "Sturgeons, paddlefishes",
                "River eels",
                "Salmons, trouts, smelts",
                "Shads",
                "Miscellaneous diadromous fishes",

                "Marine fishes",
                "Flounders, halibuts, soles",
                "Cods, hakes, haddocks",
                "Miscellaneous coastal fishes",
                "Miscellaneous demersal fishes",
                "Herrings, sardines, anchovies",
                "Tunas, bonitos, billfishes",
                "Miscellaneous pelagic fishes",
                "Sharks, rays, chimaeras",
                "Marine fishes not identified")


  SubGrLvl <- c("Freshwater fishes"="white",
    "Carps, barbels and other cyprinids"="#ffd400",
    "Tilapias and other cichlids"="#ffb600",
    "Miscellaneous freshwater fishes"="#ff9e00",

  "Diadromous fishes"="white",
    "Sturgeons, paddlefishes"="#aacc00",#C1FF1C",
    "River eels"="#80b918",#9BE931",
    "Salmons, trouts, smelts"="#55a630",#58BB43",
    "Shads"="#2b9348",#3AA346",
    "Miscellaneous diadromous fishes"="#007f5f",#007542",


  "Marine fishes"="white",
    "Flounders, halibuts, soles"="#80ffdb",
    "Cods, hakes, haddocks"="#72efdd",
    "Miscellaneous coastal fishes"="#64dfdf",
    "Miscellaneous demersal fishes"="#56cfe1",
    "Herrings, sardines, anchovies"="#48bfe3",
    "Tunas, bonitos, billfishes"="#4ea8de",
    "Miscellaneous pelagic fishes"="#5390d9",
    "Sharks, rays, chimaeras"="#5e60ce",
    "Marine fishes not identified"="#6930c3")

  SubGrlabel <- c("Freshwater fishes"="<br/>**FRESHWATER FISHES**",
                "Carps, barbels and other cyprinids"="Carps, barbels and other cyprinids",
                "Tilapias and other cichlids"="Tilapias and other cichlids",
                "Miscellaneous freshwater fishes"="Miscellaneous freshwater fishes",

                "Diadromous fishes"="<br/>**DIADROMOUS FISHES**",
                "Sturgeons, paddlefishes"="Sturgeons, paddlefishes",
                "River eels"="River eels",
                "Salmons, trouts, smelts"="Salmons, trouts, smelts",
                "Shads"="Shads",
                "Miscellaneous diadromous fishes"="Miscellaneous diadromous fishes",

                "Marine fishes"="<br/>**MARINE FISHES**",
                "Flounders, halibuts, soles"="Flounders, halibuts, soles",
                "Cods, hakes, haddocks"="Cods, hakes, haddocks",
                "Miscellaneous coastal fishes"="Miscellaneous coastal fishes",
                "Miscellaneous demersal fishes"="Miscellaneous demersal fishes",
                "Herrings, sardines, anchovies"="Herrings, sardines, anchovies",
                "Tunas, bonitos, billfishes"="Tunas, bonitos, billfishes",
                "Miscellaneous pelagic fishes"="Miscellaneous pelagic fishes",
                "Sharks, rays, chimaeras"="Sharks, rays, chimaeras",
                "Marine fishes not identified"="Marine fishes not identified")

  SubGrScale <- scale_fill_manual(values=SubGrLvl,labels=SubGrlabel)

   myColors <- scales::hue_pal()(17)
   names(myColors) <- levels(as.factor(unique(data%>%filter(Major_Group=="Fishes")%>%pull(ISSCAAP_Group_En))))
   colScale <- scale_fill_manual(name = "ISSCAAP_Group_En",values = myColors)

  ## Taxonomic line type
  # TaxonomicLine <- as.character(c("species"="solid",
  #                                 "family"="longdash",
  #                                 "nei"="dotted"))
  # TaxonomicScale <- scale_linetype_manual(name="level", values=TaxonomicLine)

  TaxonomicColors <- as.character(c("species"="#516091",
                                  "family"="#74bec1",
                                 # "nei"="#adebbe")
                                    "nei"="#ff4d4d"))
  TaxonomicScale <- scale_color_manual(name="level", values=TaxonomicColors)

  #generic indicator

  period_start<-min(data$year)
  period_end<-max(data$year)
  source_text<-sprintf("SOURCE : FAO Global Capture Production (%s-%s)",period_start,period_end)

  # Introduction

  #content$text$intro_text<-Intro_text(data)
  content$text$intro_text<-glue("The FAO Fisheries and Aquaculture Division (NFI) provides advice and objective information to FAO Members to help promote responsible fisheries and aquaculture. To fulfill this role, the Division compiles, analyses and disseminates fishery and aquaculture data structured within data collections. To ensure quality assurance, each collection is documented to highlight definitions and to specify the structure, sources, coverage, processes, intended use, etc.

  The Global Capture Production Dataset is one of the data collections. Capture fisheries are activities of harvesting aquatic organisms from marine, coastal and inland areas. The reporting of capture production species implies biodiversity mainstreaming in fisheries management, assuming that more reported taxa imply a better mainstreaming. This mainstreaming can help understand biodiversity of the wild aquatic environment, including Commercially Exploited Aquatic Species (CEAS) and other species impacted by fishing. Aquaculture is the farming of aquatic organisms and thus is not included for the purpose of this report.

  Current version of the Global Capture Production Dataset contains the volume of aquatic species caught from {period_start} to {period_end} by country or territory of capture, by species items, by FAO major fishing areas, and year, for all commercial, industrial, recreational and subsistence purposes. It provides a unique and complete record of countries’ efforts to report on species of importance to the management of their natural living resources.

  The dataset can be used to highlight geographic and taxonomic biases and gaps in global reporting. The report contains analyses of reported species by geographic areas (continents and FAO areas), and aggregated taxonomic groups found in ISSCAAP, [reference to ASIFS and ISSCAAP]. The dataset has also significantly evolved over time, providing opportunities to investigate the time series in countries’ reporting to FAO Fisheries and Aquaculture Division.")


  #Indicators

  start_date_country <- data %>%
    select(c("year","flag","flag_name","flag_iso")) %>%
    distinct() %>%
    mutate(flag=sprintf("%s[%s]",flag_name,flag_iso))%>%
    group_by(flag)%>%
    summarise(year = min(year)) %>%
    group_by(year)%>%
    summarise(flag = paste0(flag,collapse = ";"))

  tab_name<-"flag_list_by_start_year.csv"
  write.table(start_date_country,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

area_in_ocean<-data %>%
  filter(!is.na(ocean))%>%
  select(c("ocean","f_area","f_area_label")) %>%
  distinct() %>%
  group_by(ocean)%>%
  mutate(f_area=as.numeric(f_area))%>%
  arrange(f_area)%>%
  summarise(area = paste0(f_area_label,collapse = ";"))

tab_name<-"area_list_by_ocean.csv"
write.table(area_in_ocean,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

  # Part 1

  ## Commonness of Taxonomic Group records in FishStatJ
  ### Output :com_isscaap_global
  name<-"com_isscaap_global"
  #### Data

  com_global <- data %>%
    select(c("Major_Group","capture")) %>%
    count(Major_Group) %>%
    mutate(sum = sum(n)) %>%
    mutate(pour = n/sum*100)%>%
    mutate(n=n/1000)


  ###Table

  com_global_table<-

    com_global%>%
    select(c("Major_Group","n","pour"))%>%
    mutate(pour = round(pour,1))%>%
    mutate(n = round(n,1))%>%
    kable(format = "html",escape = FALSE,booktabs=T,align = "lcc",
          col.names=c("", "(number in thousands)","(percentage)"))%>%
    kable_styling("condensed")%>%
   add_header_above(setNames(3,sprintf("Total records (%s-%s)",period_start,period_end)),include_empty=F,angle=0,line=T,escape = F,italic = T,align="c",color = "white", background = "#0091a6")%>%
    row_spec(0, bold = T,italic = T, color = "white", background = "#0091a6")%>%
    column_spec(1:3, width = "1in",extra_css = "border-bottom: solid;border-bottom-width: 2px;")%>%
    footnote(general='',general_title = paste0("<b>",source_text,"</b>"),escape=F)%>%
    save_kable(file = file.path(fig_path,sprintf("table_global_%s_%s.png",period_start,period_end)), bs_theme = "flatly")

  data %>%
    filter(year %in% seq(period_end-2,period_end))%>%
    select(c("Major_Group","capture")) %>%
    count(Major_Group) %>%
    mutate(sum = sum(n)) %>%
    mutate(pour = n/sum*100)%>%
    mutate(n=n/1000)%>%
    select(c("Major_Group","n","pour"))%>%
    mutate(pour = round(pour,1))%>%
    mutate(n = round(n,1))%>%
    kable(format = "html",escape = FALSE,booktabs=T,align = "lcc",
          col.names=c("", "(number in thousands)","(percentage)"))%>%
    kable_styling("condensed")%>%
    add_header_above(setNames(3,sprintf("Total records (%s-%s)",period_end-2,period_end)),include_empty=F,angle=0,line=T,escape = F,italic = T,align="c",color = "white", background = "#0091a6")%>%
    row_spec(0, bold = T,italic = T, color = "white", background = "#0091a6")%>%
    column_spec(1:3, width = "1in",extra_css = "border-bottom: solid;border-bottom-width: 2px;")%>%
    footnote(general='',general_title = paste0("<b>",source_text,"</b>"),escape=F)%>%
    save_kable(file = file.path(fig_path,sprintf("table_global_%s_%s.png",period_end-2,period_end)), bs_theme = "flatly")

  test<-data %>%
    mutate(year =ifelse(as.numeric(substring(as.character(year),4,4))%in%0:4,
       sprintf('%s-%s',paste0(substring(as.character(year),1,3),0),paste0(substring(as.character(year),1,3),4)),
      sprintf('%s-%s',paste0(substring(as.character(year),1,3),5),paste0(substring(as.character(year),1,3),9)))
      )%>%
    select(c("Major_Group","year","capture")) %>%
    count(year,Major_Group) %>%
    group_by(year)%>%
    mutate(sum = sum(n)) %>%
    group_by(year,Major_Group)%>%
    mutate(pour = n/sum*100)%>%
    mutate(n=n/1000)%>%
    select(c("Major_Group","year","n","pour"))%>%
    mutate(pour = round(pour,1))%>%
    mutate(n = round(n,1))

  test_table_v<- test%>%
    rename(value=n)%>%
    select(-pour)%>%
    pivot_wider(names_from = year,values_from = value,values_fill = 0)%>%
    ungroup()

  test_table_p<- test%>%
    mutate(value=paste0("(",round(pour,2),"%)"))%>%
    select(-n,-pour)%>%
    pivot_wider(names_from = year,values_from = value,values_fill = "(O%)")%>%
    ungroup()

  test_table<-rbind(test_table_v,test_table_p)

  test_table$Major_Group<-factor(test_table$Major_Group,
         levels = c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))


  table<-test_table[order(test_table$Major_Group),]

    table%>%
    kable(format = "html",escape = FALSE,booktabs=T,align = "l",
          col.names=c("", names(table)[2:ncol(table)]))%>%
    kable_styling("condensed")%>%
    row_spec(0, bold = T,italic = T, color = "white", background = "#0091a6")%>%
    column_spec(1:ncol(table), width = "1in",extra_css = "border-bottom: solid;border-bottom-width: 2px;")%>%
    collapse_rows(columns = 1, valign = "m")%>%
    footnote(general='',general_title = paste0("<b>",source_text,"</b>"),escape=F)%>%
    save_kable(file = file.path(fig_path,sprintf("table_global_%s_%s_by_5.png",period_start,period_end)), bs_theme = "flatly")


  #### Figure

  fig<-ggplot(data=com_global,aes(x=Major_Group,y=n,fill=Major_Group))+
    geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
    labs(x=NULL,y="Number of records (thousands)",fill=NULL)+
    geom_text(aes(label=scales::comma(n)), vjust=-0.3, color="grey40", size=9 / .pt)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
    ISSCAAPScale+
    theme(text=element_text(size=9,family = 'sans'),
          axis.title.y = element_text(size=9),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9,angle = 0, vjust = 0.5, hjust=0.5),
          panel.background = element_rect(fill="#f8f8f8"),#"#efeff0"
          panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
          plot.margin = unit(c(0,0,0,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width = 6.5,height=4)
  content$fig[name]<-list(file.path(fig_path,fig_name))


  #### Table

  tab_name<-paste0("ref_",name,".csv")
  write.table(com_global%>%
            select(Major_Group,n,pour)%>%
            rename(ISSCAAP_Group = Major_Group)%>%
            rename(Number_thousand = n)%>%
            rename(Pourcentage = pour)
              ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
  content$text[name]<-list("Text Place Holder")
  content$title[name]<-list("Average number of Unique Records (Species Items) per Country reported 19XX to 201X — by Taxonomic Groups and Ocean Basin")
  #glue("In FAO the word 'fish' is a term used as a collective term, that includes fish, molluscs, Crustaceans and any aquatic animal which is harvested. Definition source: FAO Fisheries and Aquaculture Department, FAO, 2014.
  #FishStatJ capture production records (**{ min(as.numeric(data$year))}**-**{max(as.numeric(data$year))}** inclusive) contains **{round(subset(com_global,Major_Group=='Fishes')$pour,2)}**% vertebrate fish records more than records of molluscs **{round(subset(com_global,Major_Group=='Molluscs')$pour,2)}**% and Crustaceans **{round(subset(com_global,Major_Group=='Crustaceans')$pour,2)}**%. Aquatic plants has the least records in capture production dataset **{round(subset(com_global,Major_Group=='Aquatic Plants')$pour,2)}**%.")


####Global record by fao area

  name<-"record_by_fao_area"

  com_global_f_area <- data %>%
    select(c("f_area_label","capture")) %>%
    count(f_area_label) %>%
    mutate(sum = sum(n)) %>%
    mutate(pour = n/sum*100)%>%
    mutate(n=n/1000)%>%
    arrange(n)

  com_global_f_area$f_area_label <- factor(com_global_f_area$f_area_label, levels=unique(as.character(com_global_f_area$f_area_label)) )

  fig<-ggplot(data=com_global_f_area,aes(x=f_area_label,y=n))+
    geom_bar(stat="identity",width = 0.6,show.legend = FALSE,fill="#5390d9")+
    labs(x=NULL,y=NULL,fill=NULL)+
    scale_y_continuous(limits=c(0,max(com_global_f_area$n)),breaks = seq(0, max(com_global_f_area$n), by = 10))+
    coord_flip()+
    theme(text=element_text(size=9,family = 'sans'),
          axis.title.y = element_text(size=9),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9,angle = 0, vjust = 0.5, hjust=0.5),
          panel.background = element_rect(fill="#f8f8f8"),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype="dotted",color="grey60"),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
          plot.margin = unit(c(0,0,0,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width = 6.5,height=5)

  #### Table

  tab_name<-paste0("ref_",name,".csv")
  write.table(com_global_f_area,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")


###################
#Global by ISSCAAP group
  #By default
  com_global <- data %>%
    select(c("ISSCAAP_Group_En","Major_Group","capture")) %>%
    count(Major_Group,ISSCAAP_Group_En) %>%
    mutate(sum = sum(n)) %>%
    mutate(pour = n/sum*100)%>%
    mutate(n=n/1000)

  fig<-ggplot(data=com_global,aes(x=Major_Group,y=n,fill=ISSCAAP_Group_En))+
    geom_bar(stat="identity",position="stack",width = 0.8,show.legend = T)+
    labs(x=NULL,y="Number of records (thousands)",fill=NULL)+
    #geom_text(aes(label=scales::comma(n)), vjust=-0.3, color="grey40", size=9 / .pt)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
    #ISSCAAPScale+
    theme(text=element_text(size=9,family = 'sans'),
          axis.title.y = element_text(size=9),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9,angle = 0, vjust = 0.5, hjust=0.5),
          panel.background = element_rect(fill="#f8f8f8"),
          panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
          plot.margin = unit(c(0,0,0,0), "cm"))

      #By 5

  com_global <- data %>%
    select(c("ISSCAAP_Group_En","Major_Group","capture")) %>%
    count(Major_Group,ISSCAAP_Group_En) %>%
    mutate(n=n/1000)%>%
    group_by(Major_Group)%>%
    mutate(rank = rank(-n)) %>%
    arrange(Major_Group,rank)%>%
    ungroup()

  com_global<-rbind(as.data.frame(com_global%>%filter(rank<=5)),
           as.data.frame(com_global%>%filter(rank>5) %>% group_by(Major_Group)%>%summarise(ISSCAAP_Group_En=paste0("others ", tolower(unique(Major_Group))),n=sum(n),rank=6))
  )

  # library(ggalluvial)
  #
  # ggplot(com_global,aes(y = n, axis1 = ISSCAAP_Group_En, axis2 = Major_Group)) +
  #      geom_alluvium(aes(fill = ISSCAAP_Group_En), width = 1/12,show.legend = F) +
  #      geom_stratum(width = 1/12, fill = "grey", color = "grey") +
  #      geom_text(stat = "stratum", aes(label = after_stat(stratum)),angle=0,size=7 / .pt) +
  #      scale_x_discrete(limits = c("Group", "Major"), expand = c(.05, .05))+
  #   theme(text=element_text(size=9,family = 'sans'),
  #         axis.text.x = element_blank())


###############################################################################################################################
  ## Commonness of Marine and anadromous vs Inland records in FishStatJ
  ### Output :com_isscaap_area_type
  name<-"com_isscaap_area_type"
  #### Data

  com_area_type <- data %>%
    select(c("Major_Group","f_area_type","capture")) %>%
    count(Major_Group,f_area_type)%>%
    group_by(Major_Group)%>%
    arrange(Major_Group,desc(f_area_type))%>%
    mutate(n=n/1000)%>%
    mutate(pour=(n/sum(n))*100)%>%
    mutate(lab_ypos = ifelse(f_area_type=="marine",-10, cumsum(n)+10))

  #### Figure

  fig<-ggplot(data=com_area_type,aes(x=Major_Group,y=n,fill=f_area_type))+
    geom_bar(stat="identity",position="stack")+
    geom_text(aes(y=lab_ypos,label=sprintf("%s (%s%%)",scales::comma(n),format(round(pour,1), nsmall = 1)),colour=f_area_type), size=9 / .pt,show.legend = F)+
    scale_fill_manual(values=c("#F7DC6F", "#85C1E9"))+
    scale_color_manual(values=c("#C3A606", "#054478"))+
    labs(x=NULL,y="Number of records (thousands)",fill=NULL)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
    theme(text=element_text(size=9,family = 'sans'),
          axis.title.y = element_text(size=9),
          axis.text.y = element_text(size=9),
          axis.text.x = element_text(size=9,angle = 0, vjust = 0.5, hjust=0.5),
          # legend.background = element_rect(fill="transparent"),
          # legend.position = c(.95, .95),
          # legend.justification = c("right", "top"),
          # legend.direction = "horizontal",
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.text=element_text(size=9),
          legend.background = element_rect(fill="transparent"),
          legend.position = "bottom",
          legend.justification = "left",
          legend.direction = "horizontal",
          panel.background = element_rect(fill="#f8f8f8"),
          panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
          plot.margin = unit(c(0,0,0,0), "cm"))

  # fig_blue<-ggplot(data=com_area_type,aes(x=Major_Group,y=n,fill=f_area_type))+
  #   geom_bar(stat="identity",position="stack")+
  #   geom_text(aes(y=lab_ypos,label=scales::comma(n),colour=f_area_type), size=9 / .pt,show.legend = F)+
  #   scale_fill_manual(values=c("#80dfff", "#0099cc"))+
  #   scale_color_manual(values=c("#1ac6ff", "#0086b3"))+
  #   labs(x=NULL,y="Number of records in FishstatJ\n (in thousands)",caption="SOURCE : FAO Global Capture Production",fill=NULL)+
  #   scale_y_continuous(labels = scales::comma)+
  #   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
  #   theme(axis.title.y = element_text(size=9),
  #         axis.text.y = element_text(size=9),
  #         axis.text.x = element_text(size=9,angle = 0, vjust = 0.5, hjust=0.5),
  #         legend.background = element_rect(fill="transparent"),
  #         legend.position = c(.95, .95),
  #         legend.justification = c("right", "top"),
  #         legend.direction = "horizontal",
  #         panel.background = element_rect(fill="#f8f8f8"),
  #         panel.grid.major.y = element_line(linetype="dotted",color="grey50"),
  #         panel.grid.major.x = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
  #         plot.margin = unit(c(0,0,0,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width = 6.5,height=4)
  #ggsave(paste0(name,"_blue.png"),fig_blue,"png",path=fig_path,width=7,height=5)
  content$fig[name]<-list(file.path(fig_path,fig_name))

  #### Table

  tab_name<-paste0("ref_",name,".csv")

  write.table(com_area_type%>%
              select(Major_Group,f_area_type,n)%>%
              rename(ISSCAAP_Group = Major_Group)%>%
              rename(Number_thousand = n)%>%
              rename(FAO_Area_Type = f_area_type)
            ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
  content$text[name]<-list("Text Place Holder")

###############################################################################################################################
  ## Commonness of Taxonomic Group records in FishStatJ — by Ocean Basin
  ### Output :com_isscaap_ocean
  name<-"com_isscaap_ocean"
  ####
  content$title[name]<-list(paste0("Average number of Unique Records (Species Items) per Country reported ",period_start," to ",period_end," — by Taxonomic Groups and Ocean Basin"))
  #### Data

  ###Kim Method -

   com_ocean_group<- data %>%
     filter(!is.na(ocean))%>%
     select(c("ocean","flag","year","Major_Group","capture","species")) %>%
     group_by(ocean,flag,year,Major_Group)%>%
     summarise(n=length(unique(species)))%>%
     ungroup()%>%
     complete(nesting(ocean,flag,year),Major_Group,fill=list(n=0))%>%
     group_by(ocean,flag,year)%>%
     mutate(sum=sum(n))%>%
     group_by(ocean,flag,year,Major_Group)%>%
     mutate(pourc=n/sum)%>%
     group_by(ocean,year,Major_Group)%>%
     summarise(meanOfFlag= mean(pourc))%>%
     group_by(ocean,Major_Group)%>%
     summarise(meanOfYear= mean(meanOfFlag),maxOfYear= max(meanOfFlag),minOfYear= min(meanOfFlag),nbOfYear= n(),sdOfYear=sd(meanOfFlag),seOfYear=sd(meanOfFlag)/sqrt(n()))%>%
     ungroup()

   com_ocean_group_isscaap2<- data %>%
     filter(Major_Group=="Fishes")%>%
     mutate(ISSCAAP_Group_En=factor(ISSCAAP_Group_En,levels = ISSCAAPGroupLvl))%>%
     filter(!is.na(ocean))%>%
     #filter(ocean!="Arctic Sea")%>%
     select(c("ocean","flag","year","ISSCAAP_Group_En","capture","species")) %>%
     group_by(ocean,flag,year,ISSCAAP_Group_En)%>%
     summarise(n=length(unique(species)))%>%
     group_by(ocean,year,ISSCAAP_Group_En)%>%
     summarise(meanOfFlag= mean(n))%>%
     group_by(ocean,ISSCAAP_Group_En)%>%
     summarise(meanOfYear= mean(meanOfFlag))%>%
     group_by(ocean)%>%
     mutate(sum=sum(meanOfYear))%>%
     ungroup()%>%
     mutate(pour=meanOfYear/sum)

   add<-com_ocean_group_isscaap2[1,]
   add$ISSCAAP_Group_En<-"Diadromous fishes"
   add$pour<-0
   add2<-add
   add2$ISSCAAP_Group_En<-"Marine fishes"

   com_ocean_group_isscaap2<-rbind(com_ocean_group_isscaap2,add)
   com_ocean_group_isscaap2<-rbind(com_ocean_group_isscaap2,add2)



#######################################################

  nb_col<-2
  nb_height<-ceiling(length(unique(com_ocean_group$ocean))/nb_col)

  #### Figure

   fig<-ggplot(data=com_ocean_group%>%filter(ocean!="Arctic Sea"),aes(x=Major_Group,y=meanOfYear,fill=Major_Group))+
     geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
     geom_errorbar(aes(ymin=meanOfYear-seOfYear, ymax=meanOfYear+seOfYear), width=.1,color="grey50",position=position_dodge(1))+
     geom_text(aes(label=scales::percent(meanOfYear, accuracy=0.01,suffix =""),y=meanOfYear+seOfYear), vjust=-0.5, color="grey40", size=9 / .pt)+
     facet_wrap(~ocean,ncol=2)+
     ISSCAAPScale+
     geom_label(data=com_ocean_group%>%filter(ocean!="Arctic Sea")%>%filter(Major_Group=="Fishes"),mapping=aes(x = Inf, y = Inf, label = ocean), fill="#f8f8f8", label.size=0,hjust=1.05, vjust=1.5, size= 9 / .pt)+
     labs(x=NULL,y="Percentage",fill=NULL)+
     scale_y_continuous(labels = function(x) scales::percent(x,suffix=""), limits=c(0,1),breaks = seq(0, 1, by = 0.25))+
     scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
     theme(text=element_text(family = 'sans'),
           axis.title.y = element_text(size=9),
           axis.text.y = element_text(size=9),
           axis.text.x = element_text(size=7,angle =0, vjust = 0.5, hjust=0.5),
           strip.background = element_blank(),
           strip.text.x = element_blank(),
           panel.background = element_rect(fill="#f8f8f8"),
           panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(),
           plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
           plot.margin = unit(c(0,0,0,0), "cm"))


   fig_isscaap2<-ggplot(data=com_ocean_group_isscaap2,aes(x=ocean,y=pour,fill=ISSCAAP_Group_En))+
     geom_bar(stat="identity",position = 'stack',width = 0.8,show.legend = T)+
     labs(x=NULL,y="Percentage",fill=NULL)+
     SubGrScale+
     scale_y_continuous(labels = function(x) scales::percent(x,suffix=""), limits=c(0,1),breaks = seq(0, 1, by = 0.25))+
     scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
     theme(text=element_text(family = 'sans'),
           axis.title.y = element_text(size=9),
           axis.text.y = element_text(size=9),
           axis.text.x = element_text(size=9,angle =0, vjust = 0.5, hjust=0.5),
           legend.key = element_rect(fill = "white"),
           legend.key.size = unit(0.2, 'cm'),
           legend.title = element_blank(),
           legend.text=element_markdown(size=7),
           legend.background = element_rect(fill="transparent"),
           strip.background = element_blank(),
           strip.text.x = element_blank(),
           panel.background = element_rect(fill="#f8f8f8"),
           panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(),
           plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
           plot.margin = unit(c(0,0,0,0), "cm"))

   ggsave(paste0(name,".png"),fig,"png",path=fig_path,dpi=900,width=6.5,height=4)
   ggsave(paste0(name,"_subgroup.png"),fig_isscaap2,"png",path=fig_path,dpi=900,width=6.5,height=4)

  # for(i in com_ocean_group%>%select(ocean)%>%filter(ocean!="Arctic Sea")%>%distinct()%>%pull()){
  # fig<-ggplot(data=com_ocean_group%>%filter(ocean==i),aes(x=Major_Group,y=meanOfYear,fill=Major_Group))+
  #   geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
  #   geom_errorbar(aes(ymin=meanOfYear-sdOfYear, ymax=meanOfYear+sdOfYear), width=.1,color="grey40",position=position_dodge(1))+
  #   geom_text(aes(label=scales::percent(meanOfYear, accuracy=0.01)), vjust=-1.1, color="grey40", size=9 / .pt)+
  #   #facet_wrap(~ocean,ncol=nb_col)+
  #   ISSCAAPScale+
  #   geom_text(aes(x = Inf, y = Inf, label = i), hjust=1.05, vjust=1.5, size= 9 / .pt)+
  #   labs(x=NULL,y="Average number of annual unique records per country\n(in percent)",caption=source_text,fill=NULL)+
  #   scale_y_continuous(labels = scales::percent, limits=c(0,1))+
  #   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
  #   theme(text=element_text(family = 'sans'),
  #         axis.title.y = element_text(size=6),
  #         axis.text.y = element_text(size=9),
  #         axis.text.x = element_text(size=6,angle = 0, vjust = 0.5, hjust=0.5),
  #         strip.background = element_blank(),
  #         strip.text.x = element_blank(),
  #         panel.background = element_rect(fill="#f8f8f8"),
  #         panel.grid.major.y = element_line(linetype="solid",color="grey60"),
  #         panel.grid.major.x = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         plot.caption = element_text(face = "italic",hjust=0,size=6,color="grey20"),
  #         plot.margin = unit(c(0,0,0,0), "cm"))
  #
  # # fig_per_2<-ggplot(data=com_ocean_group_per%>%filter(ocean!="Arctic Sea"),aes(x=ocean,y=percent,fill=Major_Group))+
  # #   geom_bar(stat="identity",width = 0.8,position="stack",show.legend = TRUE)+
  # #   ISSCAAPScale+
  # #   labs(x=NULL,y="Average number of unique records per country (in percent)",caption="SOURCE : FAO Global Capture Production",fill=NULL)+
  # #   scale_y_continuous(labels = scales::percent)+
  # #   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
  # #   coord_cartesian(clip = 'off') +
  # #   theme(axis.title.y = element_text(size=10*nb_col),
  # #         axis.text.y = element_text(size=12),
  # #         axis.text.x = element_text(size=12,angle = 0, vjust = 0.5, hjust=0.5),
  # #         panel.background = element_rect(fill="#f8f8f8"),
  # #         panel.grid.major.y = element_line(linetype="dotted",color="grey50"),
  # #         panel.grid.major.x = element_blank(),
  # #         panel.grid.minor = element_blank(),
  # #         plot.caption = element_text(face = "italic",hjust=0,size=7*nb_col,color="grey20"),
  # #         plot.margin = unit(c(0,0.5,0.5,0), "cm"))
  #
  # fig_name<-paste0(name,"_",i,".png")
  # ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width=3.25,height=2)
  #
  # content$fig[name]<-list(file.path(fig_path,fig_name))
  # }


  ####Table

  tab_name<-paste0("ref_",name,".csv")

  write.table(com_ocean_group%>%
              select(Major_Group,ocean,meanOfYear)
            ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
  content$text[name]<-list("Text Place Holder")

  tab_name<-paste0("ref_",name,"_subgroup",".csv")

  write.table(com_ocean_group_isscaap2,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
  content$text[name]<-list("Text Place Holder")

###############################################################################################################################
  ## Commonness of Taxonomic Group records in FishStatJ — by FAO Fishery Areas
  ### Output :com_isccap_f_area
  #### Data

  # com_area_group <- data %>%
  #   select(c("f_area_type","f_area","f_area_label","flag","Major_Group","capture")) %>%
  #   count(f_area_type,f_area,f_area_label,flag,Major_Group)%>%
  #   group_by(f_area_type,f_area,f_area_label,Major_Group)%>%
  #   mutate(n=n/1000)%>%
  #   summarise(median = median(n))%>%
  #   group_by(f_area_type,f_area,f_area_label)%>%
  #   complete(Major_Group,fill=list(median=0))


  com_area_group<- data %>%
    select(c("f_area_type","f_area","f_area_label","flag","year","Major_Group","capture","species")) %>%
    group_by(f_area_type,f_area,f_area_label,flag,year,Major_Group)%>%
    summarise(n=length(unique(species)))%>%
    ungroup()%>%
    complete(nesting(f_area_type,f_area,f_area_label,flag,year),Major_Group,fill=list(n=0))%>%
    group_by(f_area_type,f_area,f_area_label,flag,year)%>%
    mutate(sum=sum(n))%>%
    group_by(f_area_type,f_area,f_area_label,flag,year,Major_Group)%>%
    mutate(pourc=n/sum)%>%
    group_by(f_area_type,f_area,f_area_label,year,Major_Group)%>%
    summarise(meanOfFlag= mean(pourc))%>%
    group_by(f_area_type,f_area,f_area_label,Major_Group)%>%
    summarise(meanOfYear= mean(meanOfFlag),maxOfYear= max(meanOfFlag),minOfYear= min(meanOfFlag),nbOfYear= n(),sdOfYear=sd(meanOfFlag),seOfYear=sd(meanOfFlag)/sqrt(n()))%>%
    ungroup()

  # for(i in unique(com_area_group$f_area_type)){
  #
  #   name<-paste0("com_isccap_f_area_",i)
  #
  #    com_area_type <- com_area_group %>%
  #     filter(f_area_type==i)
  #
  #    nb_col<-3
  #    nb_height<-ceiling(length(unique(com_area_type$f_area_label))/nb_col)
  #
  # #### Figure
  #
  #    fig<-ggplot(data=com_area_type%>%filter(f_area!="07"),aes(x=Major_Group,y=meanOfYear,fill=Major_Group))+
  #       geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
  #       geom_errorbar(aes(ymin=meanOfYear-sdOfYear, ymax=meanOfYear+sdOfYear), width=.1,color="grey40",position=position_dodge(1))+
  #       geom_text(aes(label=scales::percent(meanOfYear, accuracy=0.01)), vjust=-0.3, color="grey40", size=4)+
  #       facet_wrap(~f_area_label,ncol=nb_col)+
  #       ISSCAAPScale+
  #      geom_text(data=com_area_type%>%filter(f_area!="07")%>%filter(Major_Group=="Fishes"),mapping=aes(x = Inf, y = Inf, label = f_area_label), hjust=1.05, vjust=1.5, size= 5)+
  #      labs(x=NULL,y="Average number of unique records per country (in percent)",caption="SOURCE : FAO Global Capture Production",fill=NULL)+
  #      scale_y_continuous(labels = scales::percent)+
  #       scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
  #       theme(axis.title.y = element_text(size=10*nb_col),
  #             axis.text.y = element_text(size=10),
  #             axis.text.x = element_text(size=10,angle = 0, vjust = 0.5, hjust=0.5),
  #             strip.background = element_blank(),
  #             strip.text.x = element_blank(),
  #             panel.background = element_rect(fill="#f8f8f8"),
  #             panel.grid.major.y = element_line(linetype="dotted",color="grey50"),
  #             panel.grid.major.x = element_blank(),
  #             panel.grid.minor = element_blank(),
  #             plot.caption = element_text(face = "italic",hjust=0,size=8*nb_col,color="grey20"),
  #             plot.margin = unit(c(0,0.5,0.5,0), "cm"))
  #
  #     fig_name<-paste0(name,".png")
  #     ggsave(fig_name,fig,"png",path=fig_path,width=7*nb_col,height=5*nb_height)
  #     content$fig[name]<-list(file.path(fig_path,fig_name))
  #
  # #### Table
  #
  #     tab_name<-paste0("ref_",name,".csv")
  #
  #     write.table(com_area_type,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE)
  #
  #     content$tab[name]<-list(file.path(tab_path,tab_name))
  #
  # #### Text
  #     content$text[name]<-list("Text Place Holder")
  # }

     name<-paste0("com_isccap_f_area_inland")

      com_area_type <- com_area_group %>%
       filter(f_area_type=="inland")

  # #### Figure
  #
      fig<-ggplot(data=com_area_type%>%filter(f_area!="07"),aes(x=Major_Group,y=meanOfYear,fill=Major_Group))+
         geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
         geom_errorbar(aes(ymin=meanOfYear-seOfYear, ymax=meanOfYear+seOfYear), width=.1,color="grey50",position=position_dodge(1))+
         geom_text(aes(label=scales::percent(meanOfYear, accuracy=0.01,suffix=""),y=meanOfYear+seOfYear), vjust=-0.5, color="grey40", size=9 /.pt)+
         facet_wrap(~f_area_label,ncol=2)+
         ISSCAAPScale+
        geom_label(data=com_area_type%>%filter(f_area!="07")%>%filter(Major_Group=="Fishes"),mapping=aes(x = Inf, y = Inf, label = f_area_label), fill="#f8f8f8", label.size=0, hjust=1.05, vjust=1.5, size= 9 /.pt)+
        labs(x=NULL,y="Percentage",fill=NULL)+
        scale_y_continuous(labels = function(x) scales::percent(x,suffix=""), limits =c(0,1.1),breaks = seq(0, 1, by = 0.25))+
         scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
         theme(text=element_text(family = 'sans'),
               axis.title.y = element_text(size=9),
               axis.text.y = element_text(size=9),
               axis.text.x = element_text(size=7,angle = 0, vjust = 0.5, hjust=0.5),
               strip.background = element_blank(),
               strip.text.x = element_blank(),
               panel.background = element_rect(fill="#f8f8f8"),
               panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
               panel.grid.major.x = element_blank(),
               panel.grid.minor = element_blank(),
               plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
               plot.margin = unit(c(0,0,0,0), "cm"))

       fig_name<-paste0(name,".png")
       ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width=6.5,height=6)

       #INLAND TABLE
       tab_name<-paste0("ref_",name,".csv")
       write.table(com_area_type,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

      #ISSCAAP

       com_area_group2<- data %>%
         filter(f_area!="07")%>%
         filter(Major_Group=="Fishes")%>%
         mutate(ISSCAAP_Group_En=factor(ISSCAAP_Group_En,levels = ISSCAAPGroupLvl))%>%
         select(c("ocean","f_area_type","f_area","f_area_label","flag","year","ISSCAAP_Group_En","capture","species")) %>%
         group_by(ocean,f_area_type,f_area,f_area_label,flag,year,ISSCAAP_Group_En)%>%
         summarise(n=length(unique(species)))%>%
         group_by(ocean,f_area_type,f_area,f_area_label,year,ISSCAAP_Group_En)%>%
         summarise(meanOfFlag= mean(n))%>%
         group_by(ocean,f_area_type,f_area,f_area_label,ISSCAAP_Group_En)%>%
         summarise(meanOfYear= mean(meanOfFlag))%>%
         group_by(ocean,f_area_type,f_area,f_area_label)%>%
         mutate(sum=sum(meanOfYear))%>%
         ungroup()%>%
         mutate(pour=meanOfYear/sum)

       com_area_type2 <- com_area_group2 %>%
         filter(f_area_type=="inland")

       add<- com_area_type2[1,]
       add$ISSCAAP_Group_En<-"Freshwater fishes"
       add$pour<-0
       add1<- add
       add1$ISSCAAP_Group_En<-"Diadromous fishes"
       #add2<-add
       #add2$ISSCAAP_Group_En<-"Marine fishes"

       com_area_type2 <-rbind(com_area_type2 ,add)
       com_area_type2 <-rbind(com_area_type2 ,add1)
       #com_area_type2 <-rbind(com_area_type2 ,add2)

       fig_isscaap<-ggplot(data=com_area_type2,aes(x=f_area_label,y=pour,fill=ISSCAAP_Group_En))+
         geom_bar(stat="identity",position='stack',width = 0.8,show.legend = T)+
         labs(x=NULL,y="Percentage",fill=NULL)+
         SubGrScale+
         scale_y_continuous(labels = function(x) scales::percent(x,suffix=""), limits =c(0,1.1),breaks = seq(0, 1, by = 0.25))+
         scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
         theme(text=element_text(family = 'sans'),
               axis.title.y = element_text(size=9),
               axis.text.y = element_text(size=9),
               axis.text.x = element_text(size=7,angle = 0, vjust = 0.5, hjust=0.5),
               strip.background = element_blank(),
               strip.text.x = element_blank(),
               legend.key = element_rect(fill = "white"),
               legend.key.size = unit(0.2, 'cm'),
               legend.title = element_blank(),
               legend.text=element_markdown(size=7),
               legend.background = element_rect(fill="transparent"),
               panel.background = element_rect(fill="#f8f8f8"),
               panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
               panel.grid.major.x = element_blank(),
               panel.grid.minor = element_blank(),
               plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
               plot.margin = unit(c(0,0,0,0), "cm"))

       fig_name<-paste0(name,"_subgroup.png")
       ggsave(fig_name,fig_isscaap,"png",path=fig_path,dpi=900,width=6.5,height=4)

       #INLAND TABLE SUBGROUP
       tab_name<-paste0("ref_",name,"_subgroup",".csv")

       write.table(com_area_type2,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

       content$tab[name]<-list(file.path(tab_path,tab_name))

      #Marine
       name<-paste0("com_isccap_f_area_marine")

       com_area_type <- com_area_group %>%
         filter(f_area_type=="marine")

      com_area_type$part<-ifelse(com_area_type$f_area_label%in%sort(unique(com_area_type$f_area_label))[1:10],1,2)

       # #### Figure
       for (i in unique(com_area_type$part)){

       fig<-ggplot(data=com_area_type%>%filter(part==i),aes(x=Major_Group,y=meanOfYear,fill=Major_Group))+
         geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
         geom_errorbar(aes(ymin=meanOfYear-seOfYear, ymax=meanOfYear+seOfYear), width=.1,color="grey50",position=position_dodge(1))+
         geom_text(aes(label=scales::percent(meanOfYear, accuracy=0.01,suffix=""),y=meanOfYear+seOfYear), vjust=-0.5, color="grey40", size=9 /.pt)+
         facet_wrap(~f_area_label,ncol=2)+
         ISSCAAPScale+
         geom_label(data=com_area_type%>%filter(part==i)%>%filter(Major_Group=="Fishes"),mapping=aes(x = Inf, y = Inf, label = f_area_label), fill="#f8f8f8", label.size=0, hjust=1.05, vjust=1.5, size= 9 /.pt)+
         labs(x=NULL,y="Percentage",fill=NULL)+
         scale_y_continuous(labels = function(x) scales::percent(x,suffix=""), limits =c(0,1.1),breaks = seq(0, 1, by = 0.25))+
         scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
         theme(text=element_text(family = 'sans'),
               axis.title.y = element_text(size=9),
               axis.text.y = element_text(size=9),
               axis.text.x = element_text(size=7,angle = 0, vjust = 0.5, hjust=0.5),
               strip.background = element_blank(),
               strip.text.x = element_blank(),
               panel.background = element_rect(fill="#f8f8f8"),
               panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
               panel.grid.major.x = element_blank(),
               panel.grid.minor = element_blank(),
               plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
               plot.margin = unit(c(0,0,0,0), "cm"))

       fig_name<-paste0(name,"_part",i,".png")
       ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width=6.5,height=9)
       }

      #MARINE TABLE
      tab_name<-paste0("ref_",name,".csv")
      write.table(com_area_type,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

      com_area_type2 <- com_area_group2 %>%
        filter(f_area_type=="marine")

      add<- com_area_type2[1,]
      add$ISSCAAP_Group_En<-"Diadromous fishes"
      add$pour<-0
      add2<-add
      add2$ISSCAAP_Group_En<-"Marine fishes"

      com_area_type2 <-rbind(com_area_type2 ,add)
      com_area_type2 <-rbind(com_area_type2 ,add2)

      fig_issccap<-ggplot(data=com_area_type2,aes(x=f_area_label,y=pour,fill=ISSCAAP_Group_En))+
        geom_bar(stat="identity",position='stack',width = 0.8,show.legend = TRUE)+
        SubGrScale+
        labs(x=NULL,y="Percentage",fill=NULL)+
        scale_y_continuous(labels = function(x) scales::percent(x,suffix=""), limits =c(0,1),breaks = seq(0, 1, by = 0.25))+
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
        coord_flip()+
        theme(text=element_text(family = 'sans'),
              axis.title.y = element_text(size=9),
              axis.text.y = element_text(size=9),
              axis.text.x = element_text(size=7,angle = 0, vjust = 0.5, hjust=0.5),
              strip.background = element_blank(),
              strip.text.x = element_blank(),
              legend.key = element_rect(fill = "white"),
              legend.key.size = unit(0.2, 'cm'),
              legend.title = element_blank(),
              legend.text=element_markdown(size=7),
              legend.background = element_rect(fill="transparent"),
              panel.background = element_rect(fill="#f8f8f8"),
              panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
              plot.margin = unit(c(0,0,0,0), "cm"))

      fig_name<-paste0(name,"_subgroup",".png")
      ggsave(fig_name,fig_issccap,"png",path=fig_path,dpi=900,width=6.5,height=9)

      #MARINE TABLE SUBGROUP
      tab_name<-paste0("ref_",name,"_subgroup",".csv")
      write.table(com_area_type2,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")


      ###############################################################################################################################
      #Part 2.A Biodiversity reporting development in FAO Fisheries
      ## By Number of unique taxa
      ## Commonness among Fishes [vertebrate fish] records in FishStatJ — Global
      ### Output :com_sp_global
      name<-"com_sp_nbtax_global"

      #### Data
      # com_sp_lvl <- data %>%
      #   filter(Major_Group %in% c("Fishes")) %>%
      #   select(c("species","scientific_name","family","order","Taxonomic_Code","year")) %>%
      #   mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
      #   distinct()%>%
      #   count(year,level)%>%
      #   group_by(year)%>%
      #   mutate(sum = sum(n))%>%
      #   mutate(pour = n/sum)%>%
      #   group_by(level) %>%
      #   group_by(level,grp = as.integer(gl(n(), 3, n()))) %>%
      #   summarise(year = last(year), pour = mean(pour),nb_sp=mean(sum)) %>%
      #   select(-grp)

      com_sp_lvl <- data %>%
        filter(Major_Group %in% c("Fishes")) %>%
        select(c("species","scientific_name","family","order","Taxonomic_Code","year")) %>%
        mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
        distinct()%>%
        count(year,level)%>%
        group_by(year)%>%
        mutate(sum = sum(n))%>%
        mutate(pour = n/sum)%>%
        group_by(level) %>%
        mutate(pour = zoo::rollapplyr(pour, 3, mean, partial=TRUE),nb_sp=zoo::rollapplyr(sum, 3, mean, partial=TRUE))

      com_sp_lvl$level<-factor(com_sp_lvl$level,levels = c("species","family","nei"))

      #### Figure

      # fig<-ggplot(data=com_sp_lvl,aes(x=year,y=pour,group=level,color=level))+
      #   geom_line(size =1.5,lineend = "round",alpha=0.7)+
      #   geom_text(data=com_sp_lvl%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.5, color="#c6a67a",size=8 /.pt)+
      #   TaxonomicScale+
      #   labs(x=NULL,y="Percentage",caption=source_text,color=NULL)+
      #   scale_y_continuous(labels = scales::percent,limits=c(0,1),breaks = seq(0, 1, by = 0.25))+
      #   scale_x_discrete(labels = as.character(com_sp_lvl$year), breaks = com_sp_lvl$year)+
      #   theme(text=element_text(family = 'sans'),
      #         axis.title.y = element_text(size=9),
      #         axis.text.y = element_text(size=9),
      #         axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
      #         legend.key = element_rect(fill = "white"),
      #         legend.title = element_blank(),
      #         legend.text=element_text(size=9),
      #         legend.background = element_rect(fill="transparent"),
      #         legend.position = "bottom",
      #         legend.justification = "left",
      #         legend.direction = "horizontal",
      #         panel.background = element_rect(fill="#efeff0"),
      #         panel.grid.major.y = element_line(linetype="dashed",color="grey50"),
      #         panel.grid.major.x = element_blank(),
      #         panel.grid.minor = element_blank(),
      #         plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
      #         plot.margin = unit(c(0,0,0,0), "cm"))


      fig<-ggplot(data=com_sp_lvl,aes(x=year,group=level))+
        geom_line(aes(y=pour,color=level),size =0.5,lineend = "round",alpha=0.7)+
        geom_line(aes(y=nb_sp*(max(pour)/max(nb_sp))),size =0.5,lineend = "round",color="#c6a67a",linetype="11")+
        #geom_text(data=com_sp_lvl%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.5, color="#c6a67a",size=8 /.pt)+
        TaxonomicScale+
        labs(x=NULL,y="Aggregated groups (percentages)",color=NULL)+
        scale_y_continuous(labels = function(x) scales::percent(x,suffix=""),limits=c(0,1),breaks = seq(0, 1, by = 0.25),
                           sec.axis = sec_axis(~./(max(com_sp_lvl$pour)/max(com_sp_lvl$nb_sp)), name="Total taxa (number)"))+
        scale_x_continuous(breaks = seq(min(com_sp_lvl$year),max(com_sp_lvl$year),5))+
        theme(text=element_text(family = 'sans'),
              axis.title.y = element_text(size=9),
              axis.text.y = element_text(size=9),
              axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
              axis.title.y.right = element_text(color = "#c6a67a" , size=9),
              axis.text.y.right = element_text(color = "#c6a67a" , size=9),
              #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
              #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
              legend.key = element_rect(fill = "white"),
              legend.title = element_blank(),
              legend.text=element_text(size=9),
              legend.background = element_rect(fill="transparent"),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "horizontal",
              panel.background = element_rect(fill="#f8f8f8"),
              panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
              plot.margin = unit(c(0,0,0,0), "cm"))

      fig_name<-paste0(name,".png")

      ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width = 6.5,height=4)

      content$fig[name]<-list(file.path(fig_path,fig_name))

      #### Table

      tab_name<-paste0("ref_",name,".csv")

      write.table(com_sp_lvl%>%
                  rename(Pourcentage = pour)
                ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")
      content$tab[name]<-list(file.path(tab_path,tab_name))

      #### Text

      content$text[name]<-list("Text Place Holder")

      ###############################################################################################################################
      ## Commonness among fish records in FishStatJ — by Ocean Basins
      ### Output :com_sp_ocean
      name<-"com_sp_nbtax_ocean"
      #### Data

      # com_sp_ocean <- data %>%
      #   filter(f_area_type=="marine") %>%
      #   filter(Major_Group %in% c("Fishes")) %>%
      #   select(c("flag","ocean","species","scientific_name","family","order","Taxonomic_Code","year",)) %>%
      #   mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
      #   distinct()%>%
      #   count(flag,ocean,year,level)%>%
      #   group_by(ocean,year,level)%>%
      #   summarise(n=median(n))%>%
      #   ungroup()%>%
      #   complete(ocean, nesting(year, level),fill=list(n=0))%>%
      #   group_by(ocean,year)%>%
      #   mutate(sum = sum(n))%>%
      #   mutate(pour = n/sum)%>%
      #   group_by(ocean,level) %>%
      #   group_by(ocean,level,grp = as.integer(gl(n(), 3, n()))) %>%
      #   summarise(year = as.numeric(first(year)), pour = mean(pour),nb_sp=mean(sum))%>%
      #   select(-grp)

      com_sp_ocean <- data %>%
        filter(f_area_type=="marine") %>%
        filter(Major_Group %in% c("Fishes"))%>%
        select(c("ocean","species","scientific_name","family","order","Taxonomic_Code","year",)) %>%
        mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
        distinct()%>%
        count(ocean,year,level)%>%
        ungroup()%>%
        complete(ocean, nesting(year, level),fill=list(n=0))%>%
        group_by(ocean,year)%>%
        mutate(sum = sum(n))%>%
        mutate(pour = n/sum)%>%
        group_by(ocean,level) %>%
        mutate(pour = zoo::rollapplyr(pour, 3, mean, partial=TRUE),nb_sp=zoo::rollapplyr(sum, 3, mean, partial=TRUE))%>%
        filter(ocean!="Arctic Sea")

      com_sp_ocean$level<-factor(com_sp_ocean$level,levels = c("species","family","nei"))

      ##### Figure

      fig<-ggplot(data=com_sp_ocean,aes(x=year,group=level))+
        geom_line(aes(y=pour,color=level),size =0.5,lineend = "round",alpha=0.7)+
        geom_line(aes(y=nb_sp*(max(pour)/max(nb_sp))),size =0.5,lineend = "round",color="#c6a67a",linetype="11")+
        #geom_text(data=com_sp_ocean%>%filter(ocean!="Arctic Sea")%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.5, color="#c6a67a",size=8 /.pt)+
        TaxonomicScale+
        labs(x=NULL,y="Aggregated groups (percentages)",color=NULL)+
        geom_label(data=com_sp_ocean%>%filter(level=="species"),mapping=aes(x = Inf, y = Inf, label = ocean), fill="#f8f8f8", label.size=0, hjust=1.05, vjust=1.5, size= 9 /.pt)+
        facet_wrap(~ocean,ncol=2)+
        scale_y_continuous(labels = function(x) scales::percent(x,suffix=""),limits=c(0,1.2),breaks = seq(0, 1, by = 0.25),
                           sec.axis = sec_axis(~./(max(com_sp_ocean$pour)/max(com_sp_ocean$nb_sp)), name="Total taxa (number)"))+
        scale_x_continuous(breaks = seq(min(com_sp_ocean$year),max(com_sp_ocean$year),5))+
        theme(text=element_text(family = 'sans'),
              axis.title.y = element_text(size=9),
              axis.text.y = element_text(size=9),
              axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
              axis.title.y.right = element_text(color = "#c6a67a" , size=9),
              axis.text.y.right = element_text(color = "#c6a67a" , size=9),
              #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
              #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
              legend.key = element_rect(fill = "white"),
              legend.title = element_blank(),
              legend.text=element_text(size=9),
              legend.background = element_rect(fill="transparent"),
              legend.position = "bottom",
              legend.justification = "left",
              legend.direction = "horizontal",
              strip.background = element_blank(),
              strip.text.x = element_blank(),
              panel.background = element_rect(fill="#f8f8f8"),
              panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
              plot.margin = unit(c(0,0,0,0), "cm"))

      fig_name<-paste0(name,".png")
      ggsave(fig_name,fig,"png",path=fig_path,width=6.5,height=4,dpi=900)
      content$fig[name]<-list(file.path(fig_path,fig_name))

      #### Table

      tab_name<-paste0("ref_",name,".csv")

      write.table(com_sp_ocean%>%
                  rename(Pourcentage = pour)
                ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

      content$tab[name]<-list(file.path(tab_path,tab_name))

      #### Text

      content$text[name]<-list("Text Place Holder")

      ###############################################################################################################################
      ## Commonness among fish records in FishStatJ — By FAO Fisheries Areas
      ### Output :com_sp_f_area
      #### Data

      #inland

        name<-paste0("com_sp_nbtax_f_area_inland")

        com_sp_area_type <- data %>%
          filter(Major_Group %in% c("Fishes")) %>%
          filter(f_area_type == "inland") %>%
          filter(f_area!="07")%>%
          select(c("f_area_label","species","scientific_name","family","order","Taxonomic_Code","year")) %>%
          mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
          distinct()%>%
          count(f_area_label,year,level)%>%
          ungroup()%>%
          complete(f_area_label, nesting(year, level),fill=list(n=0))%>%
          group_by(f_area_label,year)%>%
          mutate(sum = sum(n))%>%
          mutate(pour = n/sum)%>%
          group_by(f_area_label,level) %>%
          mutate(pour = zoo::rollapplyr(pour, 3, mean, partial=TRUE),nb_sp=zoo::rollapplyr(sum, 3, mean, partial=TRUE))

        com_sp_area_type$level<-factor(com_sp_area_type$level,levels = c("species","family","nei"))

        nb_col<-2

        #### Figure

        fig<-ggplot(data=com_sp_area_type,aes(x=year,group=level))+
          geom_line(size =0.5,lineend = "round",aes(color=level,y=pour),alpha=0.7)+
          geom_line(aes(y=nb_sp*(max(pour)/max(nb_sp))),size =0.5,lineend = "round",color="#c6a67a",linetype="11")+
          #geom_text(data=com_sp_area_type%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.5, color="#c6a67a",size=8 /.pt)+
          TaxonomicScale+
          labs(x=NULL,y="Aggregated groups (percentages)",color=NULL)+
          geom_label(data=com_sp_area_type%>%filter(level=='species'),mapping=aes(x = Inf, y = Inf, label = f_area_label), fill="#f8f8f8", label.size=0, hjust=1.05, vjust=1.5, size= 9 /.pt)+
          facet_wrap(~f_area_label,ncol=2)+
          scale_y_continuous(labels = function(x) scales::percent(x,suffix=""),limits=c(0,1.2),breaks = seq(0, 1, by = 0.25),
                             sec.axis = sec_axis(~./(max(com_sp_area_type$pour)/max(com_sp_area_type$nb_sp)), name="Total taxa (number)"))+
          scale_x_continuous(breaks = seq(min(com_sp_area_type$year),max(com_sp_area_type$year),5))+
          theme(text=element_text(family = 'sans'),
                axis.title.y = element_text(size=9),
                axis.text.y = element_text(size=9),
                axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
                axis.title.y.right = element_text(color = "#c6a67a" , size=9),
                axis.text.y.right = element_text(color = "#c6a67a" , size=9),
                #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                legend.key = element_rect(fill = "white"),
                legend.title = element_blank(),
                legend.text=element_text(size=9),
                legend.background = element_rect(fill="transparent"),
                legend.position = "bottom",
                legend.justification = "left",
                legend.direction = "horizontal",
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_rect(fill="#f8f8f8"),
                panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
                plot.margin = unit(c(0,0,0,0), "cm"))

        fig_name<-paste0(name,".png")
        ggsave(fig_name,fig,"png",path=fig_path,width=6.5,height=6,dpi=900)
        content$fig[name]<-list(file.path(fig_path,fig_name))

        #### Table

        tab_name<-paste0("ref_",name,".csv")

        write.table(com_sp_area_type%>%
                    rename(Pourcentage = pour)
                  ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

        #marine

        name<-paste0("com_sp_nbtax_f_area_marine")

        com_sp_area_type <- data %>%
          filter(Major_Group %in% c("Fishes")) %>%
          filter(f_area_type == "marine") %>%
          select(c("f_area_label","species","scientific_name","family","order","Taxonomic_Code","year")) %>%
          mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
          distinct()%>%
          count(f_area_label,year,level)%>%
          ungroup()%>%
          complete(f_area_label, nesting(year, level),fill=list(n=0))%>%
          group_by(f_area_label,year)%>%
          mutate(sum = sum(n))%>%
          mutate(pour = n/sum)%>%
          group_by(f_area_label,level) %>%
          mutate(pour = zoo::rollapplyr(pour, 3, mean, partial=TRUE),nb_sp=zoo::rollapplyr(sum, 3, mean, partial=TRUE))

        com_sp_area_type$part<-ifelse(com_sp_area_type$f_area_label%in%sort(unique(com_sp_area_type$f_area_label))[1:10],1,2)

        # #### Figure
        for (i in unique(com_sp_area_type$part)){

          com_sp_area_type_part<-com_sp_area_type%>%filter(part==i)

          com_sp_area_type_part$level<-factor(com_sp_area_type_part$level,levels = c("species","family","nei"))

          fig<-ggplot(data=com_sp_area_type_part,aes(x=year,group=level))+
            geom_line(size =0.5,lineend = "round",aes(color=level,y=pour),alpha=0.7)+
            geom_line(aes(y=nb_sp*(max(pour,na.rm=T)/max(nb_sp,na.rm=T))),size =0.5,lineend = "round",color="#c6a67a",linetype="11")+
          #  geom_text(data=com_sp_area_type%>%filter(part==i)%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.7, color="#c6a67a",size=8 /.pt)+
            TaxonomicScale+
            labs(x=NULL,y="Aggregated groups (percentages)",color=NULL)+
            #geom_text(data=com_sp_area_type_part%>%filter(level=='species'),mapping=aes(x = Inf, y = Inf, label = f_area_label), hjust=1.05, vjust=1.5, size= 9 /.pt)+
            geom_label(data=com_sp_area_type_part%>%filter(level=='species'),mapping=aes(x = Inf, y = Inf, label = f_area_label), fill="#f8f8f8", label.size=0, hjust=1.05, vjust=1.5, size= 9 /.pt)+
            facet_wrap(~f_area_label,ncol=2)+
            scale_y_continuous(labels = function(x) scales::percent(x,suffix=""),limits=c(0,1.2),breaks = seq(0, 1, by = 0.25),
                               sec.axis = sec_axis(~./(max(com_sp_area_type_part$pour,na.rm=T)/max(com_sp_area_type_part$nb_sp,na.rm=T)), name="Total taxa (number)"))+
            scale_x_continuous(breaks = seq(min(com_sp_area_type_part$year),max(com_sp_area_type_part$year),5))+
            theme(text=element_text(family = 'sans'),
                  axis.title.y = element_text(size=9),
                  axis.text.y = element_text(size=9),
                  axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
                  axis.title.y.right = element_text(color = "#c6a67a" , size=9),
                  axis.text.y.right = element_text(color = "#c6a67a" , size=9),
                  #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                  #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                  legend.key = element_rect(fill = "white"),
                  legend.title = element_blank(),
                  legend.text=element_text(size=9),
                  legend.background = element_rect(fill="transparent"),
                  legend.position = "bottom",
                  legend.justification = "left",
                  legend.direction = "horizontal",
                  strip.background = element_blank(),
                  strip.text.x = element_blank(),
                  panel.background = element_rect(fill="#f8f8f8"),
                  panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
                  plot.margin = unit(c(0,0,0,0), "cm"))

          fig_name<-paste0(name,"_part",i,".png")
          ggsave(fig_name,fig,"png",path=fig_path,width=6.5,height=9,dpi=900)

        }

        #### Table

        tab_name<-paste0("ref_",name,".csv")

        write.table(com_sp_area_type%>%
                    rename(Pourcentage = pour)
                  ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

        content$tab[name]<-list(file.path(tab_path,tab_name))

        #### Text
        content$text[name]<-list("Text Place Holder")


        #Part 2.B Biodiversity reporting development in FAO Fisheries
        ## By Number of unique record

        ## Summary Formated Table


        #### Data

        com_sp_rec <- data %>%
          filter(Major_Group %in% c("Fishes")) %>%
          select(c("species","scientific_name","family","order","Taxonomic_Code","year")) %>%
          mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
          count(level)%>%
          mutate(sum = sum(n))%>%
          mutate(pour = round(n/sum*100,1))%>%
          mutate(n = round(n/1000,1))%>%
          mutate(level=factor(level,levels = c("species","family","nei"),labels = c("Species level","Family level","Nei")))%>%
          select(level,n,pour)%>%
          arrange(level)%>%
          kable(format = "html",escape = FALSE,booktabs=T,align = "lcc",
                col.names=c("", "(number in thousands)","(percentage)"))%>%
          kable_styling("condensed")%>%
          add_header_above(c("Total records (1950-2019)" = 3),include_empty=F,angle=0,line=T,escape = F,italic = T,align="c",color = "white", background = "#0091a6")%>%
          row_spec(0, bold = T,italic = T, color = "white", background = "#0091a6")%>%
          column_spec(1:3, width = "1in",extra_css = "border-bottom: solid;border-bottom-width: 2px;")%>%
          footnote(general='',general_title = paste0("<b>",source_text,"</b>"),escape=F)%>%
          save_kable(file = file.path(fig_path,"table_taxa_grp_rec_1950_2019.png"), bs_theme = "flatly")


        ## Commonness among Fishes [vertebrate fish] records in FishStatJ — Global
        name<-"com_sp_nbrec_global"

        #### Data

        com_sp_lvl <- data %>%
          filter(Major_Group %in% c("Fishes")) %>%
          select(c("species","scientific_name","family","order","Taxonomic_Code","year")) %>%
          mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
          count(year,level)%>%
          group_by(year)%>%
          mutate(n=n/1000)%>%
          mutate(sum = sum(n))%>%
          mutate(pour = n/sum)%>%
          group_by(level) %>%
          mutate(pour = zoo::rollapplyr(pour, 3, mean, partial=TRUE),nb_sp=zoo::rollapplyr(sum, 3, mean, partial=TRUE))

        com_sp_lvl$level<-factor(com_sp_lvl$level,levels = c("species","family","nei"))

        #### Figure

        fig<-ggplot(data=com_sp_lvl,aes(x=year,group=level))+
          geom_line(aes(y=pour,color=level),size =0.5,lineend = "round",alpha=0.7)+
          geom_line(aes(y=nb_sp*(max(pour)/max(nb_sp))),size =0.5,lineend = "round",color="#c6a67a",linetype="11")+
          #geom_text(data=com_sp_lvl%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.5, color="#c6a67a",size=8 /.pt)+
          TaxonomicScale+
          labs(x=NULL,y="Aggregated groups (percentages)",color=NULL)+
          scale_y_continuous(labels = function(x) scales::percent(x,suffix=""),limits=c(0,1),breaks = seq(0, 1, by = 0.25),
                             sec.axis = sec_axis(~./(max(com_sp_lvl$pour)/max(com_sp_lvl$nb_sp)), name="Total records (number in thousands)"))+
          scale_x_continuous(breaks = seq(min(com_sp_lvl$year),max(com_sp_lvl$year),5))+
          theme(text=element_text(family = 'sans'),
                axis.title.y = element_text(size=9),
                axis.text.y = element_text(size=9),
                axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
                axis.title.y.right = element_text(color = "#c6a67a" , size=9),
                axis.text.y.right = element_text(color = "#c6a67a" , size=9),
                #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                legend.key = element_rect(fill = "white"),
                legend.title = element_blank(),
                legend.text=element_text(size=9),
                legend.background = element_rect(fill="transparent"),
                legend.position = "bottom",
                legend.justification = "left",
                legend.direction = "horizontal",
                panel.background = element_rect(fill="#f8f8f8"),
                panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
                plot.margin = unit(c(0,0,0,0), "cm"))

        fig_name<-paste0(name,".png")

        ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width = 6.5,height=4)

        content$fig[name]<-list(file.path(fig_path,fig_name))

        #### Table

        tab_name<-paste0("ref_",name,".csv")

        write.table(com_sp_lvl%>%
                    rename(Pourcentage = pour)
                  ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")
        content$tab[name]<-list(file.path(tab_path,tab_name))

        #### Text

        content$text[name]<-list("Text Place Holder")

        ###############################################################################################################################
        ## Commonness among fish records in FishStatJ — by Ocean Basins
        ### Output :com_sp_ocean
        name<-"com_sp_nbrec_ocean"
        #### Data

        com_sp_ocean <- data %>%
          filter(f_area_type=="marine") %>%
          filter(Major_Group %in% c("Fishes"))%>%
          select(c("ocean","species","scientific_name","family","order","Taxonomic_Code","year",)) %>%
          mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
          count(ocean,year,level)%>%
          mutate(n=n/1000)%>%
          ungroup()%>%
          complete(ocean, nesting(year, level),fill=list(n=0))%>%
          group_by(ocean,year)%>%
          mutate(sum = sum(n))%>%
          mutate(pour = n/sum)%>%
          group_by(ocean,level) %>%
          mutate(pour = zoo::rollapplyr(pour, 3, mean, partial=TRUE),nb_sp=zoo::rollapplyr(sum, 3, mean, partial=TRUE))%>%
          filter(ocean!="Arctic Sea")

        com_sp_ocean$level<-factor(com_sp_ocean$level,levels = c("species","family","nei"))

        ##### Figure

        fig<-ggplot(data=com_sp_ocean,aes(x=year,group=level))+
          geom_line(aes(y=pour,color=level),size =0.5,lineend = "round",alpha=0.7)+
          geom_line(aes(y=nb_sp*(max(pour)/max(nb_sp))),size =0.5,lineend = "round",color="#c6a67a",linetype="11")+
          #geom_text(data=com_sp_ocean%>%filter(ocean!="Arctic Sea")%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.5, color="#c6a67a",size=8 /.pt)+
          TaxonomicScale+
          labs(x=NULL,y="Aggregated groups (percentages)",color=NULL)+
          geom_label(data=com_sp_ocean%>%filter(level=="species"),mapping=aes(x = Inf, y = Inf, label = ocean), fill="#f8f8f8", label.size=0, hjust=1.05, vjust=1.5, size= 9 /.pt)+
          facet_wrap(~ocean,ncol=2)+
          scale_y_continuous(labels = function(x) scales::percent(x,suffix=""),limits=c(0,1.2),breaks = seq(0, 1, by = 0.25),
                             sec.axis = sec_axis(~./(max(com_sp_ocean$pour)/max(com_sp_ocean$nb_sp)), name="Total records (number in thousands)"))+
          scale_x_continuous(breaks = seq(min(com_sp_ocean$year),max(com_sp_ocean$year),5))+
          theme(text=element_text(family = 'sans'),
                axis.title.y = element_text(size=9),
                axis.text.y = element_text(size=9),
                axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
                axis.title.y.right = element_text(color = "#c6a67a" , size=9),
                axis.text.y.right = element_text(color = "#c6a67a" , size=9),
                #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                legend.key = element_rect(fill = "white"),
                legend.title = element_blank(),
                legend.text=element_text(size=9),
                legend.background = element_rect(fill="transparent"),
                legend.position = "bottom",
                legend.justification = "left",
                legend.direction = "horizontal",
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_rect(fill="#f8f8f8"),
                panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
                plot.margin = unit(c(0,0,0,0), "cm"))

        fig_name<-paste0(name,".png")
        ggsave(fig_name,fig,"png",path=fig_path,width=6.5,height=4,dpi=900)
        content$fig[name]<-list(file.path(fig_path,fig_name))

        #### Table

        tab_name<-paste0("ref_",name,".csv")

        write.table(com_sp_ocean%>%
                    rename(Pourcentage = pour)
                  ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

        content$tab[name]<-list(file.path(tab_path,tab_name))

        #### Text

        content$text[name]<-list("Text Place Holder")

        ###############################################################################################################################
        ## Commonness among fish records in FishStatJ — By FAO Fisheries Areas
        ### Output :com_sp_f_area
        #### Data

        #inland

        name<-paste0("com_sp_nbrec_f_area_inland")

        com_sp_area_type <- data %>%
          filter(Major_Group %in% c("Fishes")) %>%
          filter(f_area_type == "inland") %>%
          filter(f_area!="07")%>%
          select(c("f_area_label","species","scientific_name","family","order","Taxonomic_Code","year")) %>%
          mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
          count(f_area_label,year,level)%>%
          mutate(n=n/1000)%>%
          ungroup()%>%
          complete(f_area_label, nesting(year, level),fill=list(n=0))%>%
          group_by(f_area_label,year)%>%
          mutate(sum = sum(n))%>%
          mutate(pour = n/sum)%>%
          group_by(f_area_label,level) %>%
          mutate(pour = zoo::rollapplyr(pour, 3, mean, partial=TRUE),nb_sp=zoo::rollapplyr(sum, 3, mean, partial=TRUE))

        com_sp_area_type$level<-factor(com_sp_area_type$level,levels = c("species","family","nei"))

        nb_col<-2

        #### Figure

        fig<-ggplot(data=com_sp_area_type,aes(x=year,group=level))+
          geom_line(size =0.5,lineend = "round",aes(color=level,y=pour),alpha=0.7)+
          geom_line(aes(y=nb_sp*(max(pour)/max(nb_sp))),size =0.5,lineend = "round",color="#c6a67a",linetype="11")+
          #geom_text(data=com_sp_area_type%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.5, color="#c6a67a",size=8 /.pt)+
          TaxonomicScale+
          labs(x=NULL,y="Aggregated groups (percentages)",color=NULL)+
          geom_label(data=com_sp_area_type%>%filter(level=='species'),mapping=aes(x = Inf, y = Inf, label = f_area_label), fill="#f8f8f8", label.size=0, hjust=1.05, vjust=1.5, size= 9 /.pt)+
          facet_wrap(~f_area_label,ncol=2)+
          scale_y_continuous(labels = function(x) scales::percent(x,suffix=""),limits=c(0,1.2),breaks = seq(0, 1, by = 0.25),
                             sec.axis = sec_axis(~./(max(com_sp_area_type$pour)/max(com_sp_area_type$nb_sp)), name="Total records (number in thousands)"))+
          scale_x_continuous(breaks = seq(min(com_sp_area_type$year),max(com_sp_area_type$year),5))+
          theme(text=element_text(family = 'sans'),
                axis.title.y = element_text(size=9),
                axis.text.y = element_text(size=9),
                axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
                axis.title.y.right = element_text(color = "#c6a67a" , size=9),
                axis.text.y.right = element_text(color = "#c6a67a" , size=9),
                #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                legend.key = element_rect(fill = "white"),
                legend.title = element_blank(),
                legend.text=element_text(size=9),
                legend.background = element_rect(fill="transparent"),
                legend.position = "bottom",
                legend.justification = "left",
                legend.direction = "horizontal",
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_rect(fill="#f8f8f8"),
                panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
                plot.margin = unit(c(0,0,0,0), "cm"))

        fig_name<-paste0(name,".png")
        ggsave(fig_name,fig,"png",path=fig_path,width=6.5,height=6,dpi=900)
        content$fig[name]<-list(file.path(fig_path,fig_name))

        #### Table

        tab_name<-paste0("ref_",name,".csv")

        write.table(com_sp_area_type%>%
                    rename(Pourcentage = pour)
                  ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

        #marine

        name<-paste0("com_sp_nbrec_f_area_marine")

        com_sp_area_type <- data %>%
          filter(Major_Group %in% c("Fishes")) %>%
          filter(f_area_type == "marine") %>%
          select(c("f_area_label","species","scientific_name","family","order","Taxonomic_Code","year")) %>%
          mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
          count(f_area_label,year,level)%>%
          mutate(n=n/1000)%>%
          ungroup()%>%
          complete(f_area_label, nesting(year, level),fill=list(n=0))%>%
          group_by(f_area_label,year)%>%
          mutate(sum = sum(n))%>%
          mutate(pour = n/sum)%>%
          group_by(f_area_label,level) %>%
          mutate(pour = zoo::rollapplyr(pour, 3, mean, partial=TRUE),nb_sp=zoo::rollapplyr(sum, 3, mean, partial=TRUE))

        com_sp_area_type$part<-ifelse(com_sp_area_type$f_area_label%in%sort(unique(com_sp_area_type$f_area_label))[1:10],1,2)

        # #### Figure
        for (i in unique(com_sp_area_type$part)){

          com_sp_area_type_part<-com_sp_area_type%>%filter(part==i)

          com_sp_area_type_part$level<-factor(com_sp_area_type_part$level,levels = c("species","family","nei"))

          fig<-ggplot(data=com_sp_area_type_part,aes(x=year,group=level))+
            geom_line(size =0.5,lineend = "round",aes(color=level,y=pour),alpha=0.7)+
            geom_line(aes(y=nb_sp*(max(pour,na.rm=T)/max(nb_sp,na.rm=T))),size =0.5,lineend = "round",color="#c6a67a",linetype="11")+
            #  geom_text(data=com_sp_area_type%>%filter(part==i)%>%filter(level=='species'),mapping=aes(y =0,label=paste0("n\n",round(nb_sp,0))),lineheight = .7,vjust=0.7, color="#c6a67a",size=8 /.pt)+
            TaxonomicScale+
            labs(x=NULL,y="Aggregated groups (percentages)",color=NULL)+
            #geom_text(data=com_sp_area_type_part%>%filter(level=='species'),mapping=aes(x = Inf, y = Inf, label = f_area_label), hjust=1.05, vjust=1.5, size= 9 /.pt)+
            geom_label(data=com_sp_area_type_part%>%filter(level=='species'),mapping=aes(x = Inf, y = Inf, label = f_area_label), fill="#f8f8f8", label.size=0, hjust=1.05, vjust=1.5, size= 9 /.pt)+
            facet_wrap(~f_area_label,ncol=2)+
            scale_y_continuous(labels = function(x) scales::percent(x,suffix=""),limits=c(0,1.2),breaks = seq(0, 1, by = 0.25),
                               sec.axis = sec_axis(~./(max(com_sp_area_type_part$pour,na.rm=T)/max(com_sp_area_type_part$nb_sp,na.rm=T)), name="Total records (number in thousands)"))+
            scale_x_continuous(breaks = seq(min(com_sp_area_type_part$year),max(com_sp_area_type_part$year),5))+
            theme(text=element_text(family = 'sans'),
                  axis.title.y = element_text(size=9),
                  axis.text.y = element_text(size=9),
                  axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=0.5),
                  axis.title.y.right = element_text(color = "#c6a67a" , size=9),
                  axis.text.y.right = element_text(color = "#c6a67a" , size=9),
                  #axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                  #axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
                  legend.key = element_rect(fill = "white"),
                  legend.title = element_blank(),
                  legend.text=element_text(size=9),
                  legend.background = element_rect(fill="transparent"),
                  legend.position = "bottom",
                  legend.justification = "left",
                  legend.direction = "horizontal",
                  strip.background = element_blank(),
                  strip.text.x = element_blank(),
                  panel.background = element_rect(fill="#f8f8f8"),
                  panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.caption = element_text(face = "italic",hjust=0,size=9,color="grey20"),
                  plot.margin = unit(c(0,0,0,0), "cm"))

          fig_name<-paste0(name,"_part",i,".png")
          ggsave(fig_name,fig,"png",path=fig_path,width=6.5,height=9,dpi=900)

        }

        #### Table

        tab_name<-paste0("ref_",name,".csv")

        write.table(com_sp_area_type%>%
                    rename(Pourcentage = pour)
                  ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

        content$tab[name]<-list(file.path(tab_path,tab_name))

        #### Text
        content$text[name]<-list("Text Place Holder")

###############################################################################################################################
#Biodiversity indexes
#2
  #a
    #ocean

  name<-paste0("taxa_richess_","ocean")

  ocean_nb_sp<- data %>%
    filter(!is.na(ocean))%>%
    filter(year %in% seq(period_end-9,period_end,1))%>%
    select(c("ocean","Major_Group","species")) %>%
    group_by(ocean,Major_Group)%>%
    summarise(n=length(unique(species)))%>%
    ungroup()%>%
    complete(ocean,Major_Group,fill=list(n=0))%>%
    group_by(ocean)%>%
    mutate(sum=sum(n))%>%
    group_by(ocean,Major_Group)%>%
    mutate(pourc=n/sum*100)%>%
    select(-sum)%>%
  ungroup()

  ocean_nb_sp_table_v<- ocean_nb_sp%>%
    rename(value=n)%>%
    select(-pourc)%>%
    pivot_wider(names_from = Major_Group,values_from = value,values_fill = 0)%>%
    ungroup()

  ocean_nb_sp_table_p<- ocean_nb_sp%>%
    mutate(value=paste0("(",round(pourc,2),"%)"))%>%
    select(-n,-pourc)%>%
    pivot_wider(names_from = Major_Group,values_from = value,values_fill = "(O%)")%>%
    ungroup()

  ocean_nb_sp_table<-rbind(ocean_nb_sp_table_v,ocean_nb_sp_table_p)

  ocean_nb_sp_table<-ocean_nb_sp_table[order(ocean_nb_sp_table$ocean),]

  # ocean_nb_sp_table<-as.data.frame(ocean_nb_sp_table)
  #rownames(ocean_nb_sp_table)<-ocean_nb_sp_table$ocean
  #ocean_nb_sp_table<-ocean_nb_sp_table[,-1]
  names(ocean_nb_sp_table)[1]<-"Ocean/ISSCAAP groups"

  kw0 <- paste0("(number of different taxa",footnote_marker_number(1, double_escape = T),"; percentage of total repartition)")

  ocean_nb_sp_table%>%
    kable(format = "html",escape = FALSE,booktabs=T,align = "lccccc",
    col.names=paste0(substr(names(ocean_nb_sp_table),1,1),tolower(substr(names(ocean_nb_sp_table),2,nchar(names(ocean_nb_sp_table))))))%>%
    kable_styling("condensed")%>%
    add_header_above(c(setNames(6,kw0)),include_empty=F,angle=0,line=T,escape = F,italic = T,align="c",color = "white", background = "#0091a6")%>%
    row_spec(0, bold = T, color = "white", background = "#0091a6")%>%
   column_spec(1,extra_css = "border-bottom: solid;border-bottom-width: 2px;")%>%
    column_spec(2:6, width = "1in",extra_css = "border-bottom: solid;border-bottom-width: 2px;")%>%
    collapse_rows(columns = 1, valign = "m")%>%
    footnote(number = c("Exclude aquatic mammals, amphibia and reptilia"))


  tab_name<-paste0("ref_",name,".csv")
  write.table(ocean_nb_sp_table,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

  #fao major area
  major_area_nb_sp<- data %>%
    filter(year %in% seq(period_end-9,period_end,1))%>%
    select(c("f_area_type","f_area","f_area_label","Major_Group","species")) %>%
    group_by(f_area_type,f_area,f_area_label,Major_Group)%>%
    summarise(n=length(unique(species)))%>%
    ungroup()%>%
    complete(nesting(f_area_type,f_area,f_area_label),Major_Group,fill=list(n=0))%>%
    group_by(f_area_type,f_area,f_area_label)%>%
    mutate(sum=sum(n))%>%
    group_by(f_area_type,f_area,f_area_label,Major_Group)%>%
    mutate(pourc=n/sum*100)%>%
    select(-sum)%>%
    ungroup()

  for(i in unique(major_area_nb_sp$f_area_type)){
    area_type_nb_sp <- major_area_nb_sp %>%
      filter(f_area_type==i)%>%
      mutate(value=paste0(n," [",round(pourc,2),"%]"))%>%
      select(-n,-pourc)%>%
      pivot_wider(names_from = Major_Group,values_from = value,values_fill = "0 [O%]")%>%
      ungroup()
  print(area_type_nb_sp)
  name<-paste0("taxa_richess_",i)
  tab_name<-paste0("ref_",name,".csv")
  write.table(area_type_nb_sp,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")
  }

  #b
  ##ocean

  name<-paste0("taxa_inMoreThan2_","ocean")

  multi_ocean<- data%>%
    filter(!is.na(ocean))%>%
    filter(year %in% seq(period_end-9,period_end,1))%>%
    select(ocean,Major_Group,species,capture)%>%
    mutate(code=paste0(ocean,"-",Major_Group))%>%
    select(-ocean,-Major_Group)%>%
    mutate(capture=1)%>%
    distinct()%>%
    pivot_wider(names_from = code,values_from = capture,values_fill = 0)%>%
    mutate(in_x_ocean=rowSums(across(where(is.numeric))))%>%
    filter(in_x_ocean>=3)%>%
    select(-in_x_ocean)%>%
    pivot_longer(!species, names_to = "code", values_to = "count")%>%
    group_by(code)%>%
    summarise(morethan2=sum(count))%>%
    separate(code, c("ocean", "Major_Group"), sep = "-")%>%
    complete(ocean,Major_Group,fill=list(morethan2=0))%>%
    ungroup()%>%
    inner_join(select(ocean_nb_sp,ocean,Major_Group,n))%>%
    group_by(ocean,Major_Group)%>%
    mutate(pourc=morethan2/n*100)

  multi_ocean_table<-multi_ocean%>%
    group_by(ocean,Major_Group)%>%
    mutate(value=if(!is.nan(pourc)){paste0(morethan2," [",round(pourc,2),"%]")}else{"_"})%>%
    select(ocean,Major_Group,value)%>%
    arrange(match(Major_Group, c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))) %>%
    pivot_wider(names_from = Major_Group,values_from = value,values_fill = "0 [O%]")%>%
    ungroup()

  tab_name<-paste0("ref_",name,".csv")
  write.table(multi_ocean_table,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")


  multi_area<- data%>%
    filter(year %in% seq(period_end-9,period_end,1))%>%
    select(f_area_type,f_area_label,Major_Group,species,capture)%>%
    mutate(code=paste0(f_area_type,"_",f_area_label,"_",Major_Group))%>%
    select(-f_area_type,-f_area_label,-Major_Group)%>%
    mutate(capture=1)%>%
    distinct()%>%
    pivot_wider(names_from = code,values_from = capture,values_fill = 0)%>%
    mutate(in_x_ocean=rowSums(across(where(is.numeric))))%>%
    filter(in_x_ocean>=3)%>%
    select(-in_x_ocean)%>%
    pivot_longer(!species, names_to = "code", values_to = "count")%>%
    group_by(code)%>%
    summarise(morethan2=sum(count))%>%
    separate(code, c("f_area_type","f_area_label", "Major_Group"), sep = "_")%>%
    complete(nesting(f_area_type,f_area_label),Major_Group,fill=list(morethan2=0))%>%
    ungroup()%>%
    inner_join(select(major_area_nb_sp,f_area_type,f_area_label,Major_Group,n))%>%
    group_by(f_area_type,f_area_label,Major_Group)%>%
    mutate(pourc=morethan2/n*100)

  for(i in unique(multi_area$f_area_type)){
    name<-paste0("taxa_inMoreThan2_",i)
    multi_area_type <- multi_area %>%
      filter(f_area_type==i)%>%
      mutate(value=if(!is.nan(pourc)){paste0(morethan2," [",round(pourc,2),"%]")}else{"_"})%>%
      select(f_area_label,Major_Group,value)%>%
      arrange(match(Major_Group, c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))) %>%
      pivot_wider(names_from = Major_Group,values_from = value,values_fill = "0 [O%]")%>%
      ungroup()

    print(multi_area_type)

    tab_name<-paste0("ref_",name,".csv")
    write.table(multi_area_type,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")
  }

  #c
  #ocean

  name<-paste0("taxa_onlyIn1_","ocean")

  unique_ocean<- data%>%
    filter(!is.na(ocean))%>%
    filter(year %in% seq(period_end-9,period_end,1))%>%
    select(ocean,Major_Group,species,capture)%>%
    mutate(code=paste0(ocean,"-",Major_Group))%>%
    select(-ocean,-Major_Group)%>%
    mutate(capture=1)%>%
    distinct()%>%
    pivot_wider(names_from = code,values_from = capture,values_fill = 0)%>%
    mutate(in_x_ocean=rowSums(across(where(is.numeric))))%>%
    filter(in_x_ocean==1)%>%
    select(-in_x_ocean)%>%
    pivot_longer(!species, names_to = "code", values_to = "count")%>%
    group_by(code)%>%
    summarise(unique_nb=sum(count))%>%
    separate(code, c("ocean", "Major_Group"), sep = "-")%>%
    complete(ocean,Major_Group,fill=list(unique_nb=0))%>%
    ungroup()%>%
    inner_join(select(ocean_nb_sp,ocean,Major_Group,n))%>%
    group_by(ocean,Major_Group)%>%
    mutate(pourc=unique_nb/n*100)

  unique_ocean_table<-unique_ocean%>%
    group_by(ocean,Major_Group)%>%
    mutate(value=if(!is.nan(pourc)){paste0(unique_nb," [",round(pourc,2),"%]")}else{"_"})%>%
    select(ocean,Major_Group,value)%>%
    arrange(match(Major_Group, c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))) %>%
    pivot_wider(names_from = Major_Group,values_from = value,values_fill = "0 [O%]")%>%
    ungroup()

  tab_name<-paste0("ref_",name,".csv")
  write.table(unique_ocean_table,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

  #f_area
  unique_area<- data%>%
    filter(year %in% seq(period_end-9,period_end,1))%>%
    select(f_area_type,f_area_label,Major_Group,species,capture)%>%
    mutate(code=paste0(f_area_type,"_",f_area_label,"_",Major_Group))%>%
    select(-f_area_type,-f_area_label,-Major_Group)%>%
    mutate(capture=1)%>%
    distinct()%>%
    pivot_wider(names_from = code,values_from = capture,values_fill = 0)%>%
    mutate(in_x_ocean=rowSums(across(where(is.numeric))))%>%
    filter(in_x_ocean==1)%>%
    select(-in_x_ocean)%>%
    pivot_longer(!species, names_to = "code", values_to = "count")%>%
    group_by(code)%>%
    summarise(unique_nb=sum(count))%>%
    separate(code, c("f_area_type","f_area_label", "Major_Group"), sep = "_")%>%
    complete(nesting(f_area_type,f_area_label),Major_Group,fill=list(unique_nb=0))%>%
    ungroup()%>%
    inner_join(select(major_area_nb_sp,f_area_type,f_area_label,Major_Group,n))%>%
    group_by(f_area_type,f_area_label,Major_Group)%>%
    mutate(pourc=unique_nb/n*100)

  for(i in unique(unique_area$f_area_type)){

    name<-paste0("taxa_onlyIn1_",i)

    unique_area_type <- unique_area %>%
      filter(f_area_type==i)%>%
      mutate(value=if(!is.nan(pourc)){paste0(unique_nb," [",round(pourc,2),"%]")}else{"_"})%>%
      select(f_area_label,Major_Group,value)%>%
      arrange(match(Major_Group, c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))) %>%
      pivot_wider(names_from = Major_Group,values_from = value,values_fill = "0 [O%]")%>%
      ungroup()

    print(unique_area_type)

    tab_name<-paste0("ref_",name,".csv")
    write.table(unique_area_type,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")
  }

  ##
#3
  #a
  #ocean

  name<-paste0("evenness_simpson_","ocean")

   richess_ocean<- data%>%
     filter(!is.na(ocean))%>%
     select(ocean,Major_Group,flag,year,species,capture)%>%
     mutate(code=paste0(ocean,"-",Major_Group))%>%
     select(-ocean,-Major_Group)%>%
     filter(year %in% seq(period_end-9,period_end,1))%>%
     mutate(capture=1)%>%
     distinct()%>%
     group_by(code,species)%>%
     summarise(n=sum(capture))%>%
     pivot_wider(names_from = species,values_from = n,values_fill = 0)%>%
     group_by(code)%>%
     summarise(simpson=diversity(across(where(is.numeric)),index="simpson"))%>%
     separate(code, c("ocean","Major_Group"), sep = "-")%>%
     ungroup()

   richess_ocean_table<-richess_ocean%>%
     group_by(ocean)%>%
     arrange(match(Major_Group, c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))) %>%
     pivot_wider(names_from = Major_Group,values_from = simpson)

   tab_name<-paste0("ref_",name,".csv")
   write.table(richess_ocean_table,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")


   richess_ocean$Major_Group<-factor(richess_ocean$Major_Group,
                                       levels = c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))

   fig<-ggplot(richess_ocean, aes(Major_Group, ocean, fill= simpson)) +
     geom_tile(color = "white",lwd = 1.5,linetype = 1) +
     geom_text(aes(label = scales::comma(simpson,accuracy=0.01)), color = "white", size = 4)+
     scale_fill_gradient2(low="#caf0f8",mid="#48cae4",high="#023e8a",midpoint=0.5,limits=c(0,1))+
     labs(x=NULL,y=NULL,caption="SOURCE : FAO Global Capture Production",fill="Simpson index")+
     scale_y_discrete(limits=rev)+
     scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
     theme(axis.title.y = element_text(size=9),
           axis.text.y = element_text(size=9,vjust = 0.5),
           axis.text.x = element_text(size=7,angle = 0, vjust = 0.5, hjust=0.5),
           strip.background = element_blank(),
           strip.text.x = element_blank(),
           panel.background = element_rect(fill="#f8f8f8"),
           legend.title = element_text(size=8),
           legend.text = element_text(size=8),
           panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(),
           plot.caption = element_text(face = "italic",hjust=0,size=10,color="grey20"),
           plot.margin = unit(c(0,0,0,0), "cm"))

   fig_name<-paste0(name,".png")
   ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width=6.5,height=5)

   #major area

   richess_area<- data%>%
     select(f_area_type,f_area,f_area_label,Major_Group,flag,year,species,capture)%>%
     mutate(code=paste0(f_area_type,"_",f_area,"_",f_area_label,"_",Major_Group))%>%
     select(-f_area_type,-f_area,-f_area_label,-Major_Group)%>%
     filter(year %in% seq(period_end-9,period_end,1))%>%
     mutate(capture=1)%>%
     distinct()%>%
     group_by(code,species)%>%
     summarise(n=sum(capture))%>%
     pivot_wider(names_from = species,values_from = n,values_fill = 0)%>%
     group_by(code)%>%
     summarise(simpson=diversity(across(where(is.numeric)),index="simpson"))%>%
     separate(code, c("f_area_type","f_area","f_area_label","Major_Group"), sep = "_")%>%
     ungroup()

   richess_area$Major_Group<-factor(richess_area$Major_Group,
                                       levels = c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))

   for(i in unique(richess_area$f_area_type)){

     name<-paste0("evenness_simpson_",i)

     richess_area_table<-richess_area%>%
       filter(f_area_type==i)%>%
       group_by(f_area_type,f_area,f_area_label)%>%
       arrange(match(Major_Group, c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))) %>%
       pivot_wider(names_from = Major_Group,values_from = simpson)

      print(richess_area_table)

      tab_name<-paste0("ref_",name,".csv")
      write.table(richess_area_table,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

     richess_area_type <- richess_area %>%
       filter(f_area_type==i)

   fig<-ggplot(richess_area_type%>%filter(f_area!="07"), aes(Major_Group, f_area_label, fill= simpson)) +
     geom_tile(color = "white",lwd = 1.5,linetype = 1) +
     geom_text(aes(label = scales::comma(simpson,accuracy=0.01)), color = "white", size = 4)+
     scale_fill_gradient2(low="#caf0f8",mid="#48cae4",high="#023e8a",midpoint=0.5,limits=c(0,1))+
     labs(x=NULL,y=NULL,caption="SOURCE : FAO Global Capture Production",fill="Simpson index")+
     scale_y_discrete(limits=rev)+
     scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
     theme(axis.title.y = element_text(size=9),
           axis.text.y = element_text(size=9,vjust = 0.5),
           axis.text.x = element_text(size=7,angle = 0, vjust = 0.5, hjust=0.5),
           strip.background = element_blank(),
           strip.text.x = element_blank(),
           panel.background = element_rect(fill="#f8f8f8"),
           legend.title = element_text(size=8),
           legend.text = element_text(size=8),
           panel.grid.major.y = element_line(linetype="dotted",color="grey60"),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(),
           plot.caption = element_text(face = "italic",hjust=0,size=10,color="grey20"),
           plot.margin = unit(c(0,0,0,0), "cm"))

   fig_name<-paste0(name,".png")
   ggsave(fig_name,fig,"png",path=fig_path,dpi=900,width=6.5,height=ifelse(i=="marine",6,5))

   }

#4
  #a
   #ocean

   name<-paste0("turnover_","ocean")

   turn_ocean<- data %>%
     filter(!is.na(ocean))%>%
     select(ocean,flag,Major_Group,year,species) %>%
     filter(year %in% seq(period_end-9,period_end,1))%>%
     distinct()%>%
     arrange(ocean,flag,Major_Group,year)%>%
     group_by(ocean,flag,Major_Group,year)%>%
     summarise(list_sp = paste0(unique(species),collapse = ","))%>%
     group_by(ocean,flag,Major_Group)%>%
     mutate(previous_year = lag(list_sp, order_by = year))%>%
     filter(year!=2010)%>%
     group_by(ocean,flag,Major_Group,year)%>%
     mutate(previous_year = if(!is.na(previous_year)){previous_year}else{"_"})%>%
     mutate(nb_diff={
       x=sort(unlist(strsplit(list_sp,",")))==sort(unlist(strsplit(previous_year,",")))
       length(x[x==FALSE])},nb_egal={
       x=sort(unlist(strsplit(list_sp,",")))==sort(unlist(strsplit(previous_year,",")))
       length(x[x==TRUE])})%>%
     mutate(turnover=(nb_diff/(nb_diff+nb_egal))*100)%>%
     group_by(ocean,flag,Major_Group)%>%
    summarise(meanOfYear=mean(turnover))%>%
     group_by(ocean,Major_Group)%>%
     summarise(meanOfFlag=mean(meanOfYear))%>%
     ungroup()

   turn_ocean_table<-turn_ocean%>%
     arrange(match(Major_Group, c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))) %>%
     pivot_wider(names_from = Major_Group,values_from = meanOfFlag)

   tab_name<-paste0("ref_",name,".csv")
   write.table(turn_ocean_table,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")

   #fao area

   turn_area<- data %>%
     select(f_area_type,f_area_label,flag,Major_Group,year,species) %>%
     filter(year %in% seq(period_end-9,period_end,1))%>%
     distinct()%>%
     arrange(f_area_type,f_area_label,flag,Major_Group,year)%>%
     group_by(f_area_type,f_area_label,flag,Major_Group,year)%>%
     summarise(list_sp = paste0(unique(species),collapse = ","))%>%
     group_by(f_area_type,f_area_label,flag,Major_Group)%>%
     mutate(previous_year = lag(list_sp, order_by = year))%>%
     filter(year!=2010)%>%
     group_by(f_area_type,f_area_label,flag,Major_Group,year)%>%
     mutate(previous_year = if(!is.na(previous_year)){previous_year}else{"_"})%>%
     mutate(nb_diff={
       x=sort(unlist(strsplit(list_sp,",")))==sort(unlist(strsplit(previous_year,",")))
       length(x[x==FALSE])},nb_egal={
         x=sort(unlist(strsplit(list_sp,",")))==sort(unlist(strsplit(previous_year,",")))
         length(x[x==TRUE])})%>%
     mutate(turnover=(nb_diff/(nb_diff+nb_egal))*100)%>%
     group_by(f_area_type,f_area_label,flag,Major_Group)%>%
     summarise(meanOfYear=mean(turnover))%>%
     group_by(f_area_type,f_area_label,Major_Group)%>%
     summarise(meanOfFlag=mean(meanOfYear))%>%
     ungroup()

   for(i in unique(turn_area$f_area_type)){

     name<-paste0("turnover_",i)

     turn_area_table<-turn_area%>%
       filter(f_area_type==i)%>%
       group_by(f_area_type,f_area_label)%>%
     arrange(match(Major_Group, c("Fishes", "Molluscs", "Crustaceans", "Aquatic Invertebrates","Aquatic Plants"))) %>%
     pivot_wider(names_from = Major_Group,values_from = meanOfFlag)

     print(turn_area_table)

     tab_name<-paste0("ref_",name,".csv")
     write.table(turn_area_table,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE,sep="\t")
   }


  return(content)
}
