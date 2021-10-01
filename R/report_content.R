#' Compute content of report
#'
#' This function compute all the output elements to include of report
#'
#' @param file Path to the input file
#' @param out Path to export destination
#' @return A list of element
#' @export

report_content<-function(file,out=NULL){

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
  write.csv(data,file.path(tab_path,"structured_dataset.csv"),row.names = FALSE,quote=FALSE)

  #Graphical rules
  ## ISSCAAP Group colors
  ISSCAAPColors <- as.character(c("PISCES"="#AED6F1",
                                  "MOLLUSCA"="#D7BDE2",
                                  "CRUSTACEA"="#F5B7B1",
                                  "INVERTEBRATA AQUATICA"="#F9E79F",
                                  "PLANTAE AQUATICAE"="#ABEBC6"))
  ISSCAAPScale <- scale_fill_manual(name="Major_Group_En", values=ISSCAAPColors)

  ## Taxonomic line type
  TaxonomicLine <- as.character(c("species"="solid",
                                  "family"="longdash",
                                  "nei"="dotted"))
  TaxonomicScale <- scale_linetype_manual(name="level", values=TaxonomicLine)

  # Introduction

  #content$text$intro_text<-Intro_text(data)
  content$text$intro_text<-"Text Place Holder"

  # Part 1

  ## Commonness of Taxonomic Group records in FishStatJ
  ### Output :com_isscaap_global
  name<-"com_isscaap_global"
  #### Data

  com_global <- data %>%
    select(c("Major_Group_En","capture")) %>%
    count(Major_Group_En) %>%
    mutate(sum = sum(n)) %>%
    mutate(pour = n/sum*100)%>%
    mutate(n=n/1000)

  #### Figure

  fig<-ggplot(data=com_global,aes(x=Major_Group_En,y=n,fill=Major_Group_En))+
    geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
    labs(x=NULL,y="Number of records in FishstatJ\n (in thousands)",caption="SOURCE : FAO Global Capture Production",fill=NULL)+
    geom_text(aes(label=scales::comma(n)), vjust=-0.3, color="grey40", size=3.5)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
    ISSCAAPScale+
    theme(axis.title.y = element_text(size=10),
          axis.text.y = element_text(size=8),
          axis.text.x = element_text(size=8,angle = 0, vjust = 0.5, hjust=0.5),
          panel.background = element_rect(fill="#f0f0f0"),
          panel.grid.major.y = element_line(linetype="dotted",color="grey50"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=8,color="grey20"),
          plot.margin = unit(c(0,0.5,0.5,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,width=7,height=5)
  content$fig[name]<-list(file.path(fig_path,fig_name))


  #### Table

  tab_name<-paste0("ref_",name,".csv")
  write.csv(com_global%>%
            select(Major_Group_En,n,pour)%>%
            rename(ISSCAAP_Group = Major_Group_En)%>%
            rename(Number_thousand = n)%>%
            rename(Pourcentage = pour)
              ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE)

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
  content$text[name]<-list("Text Place Holder")
  #glue("In FAO the word 'fish' is a term used as a collective term, that includes fish, molluscs, crustaceans and any aquatic animal which is harvested. Definition source: FAO Fisheries and Aquaculture Department, FAO, 2014.
  #FishStatJ capture production records (**{ min(as.numeric(data$year))}**-**{max(as.numeric(data$year))}** inclusive) contains **{round(subset(com_global,Major_Group_En=='PISCES')$pour,2)}**% vertebrate fish records more than records of molluscs **{round(subset(com_global,Major_Group_En=='MOLLUSCA')$pour,2)}**% and crustaceans **{round(subset(com_global,Major_Group_En=='CRUSTACEA')$pour,2)}**%. Aquatic plants has the least records in capture production dataset **{round(subset(com_global,Major_Group_En=='PLANTAE AQUATICAE')$pour,2)}**%.")

###############################################################################################################################
  ## Commonness of Marine and anadromous vs Inland records in FishStatJ
  ### Output :com_isscaap_area_type
  name<-"com_isscaap_area_type"
  #### Data

  com_area_type <- data %>%
    select(c("Major_Group_En","f_area_type","capture")) %>%
    count(Major_Group_En,f_area_type)%>%
    group_by(Major_Group_En)%>%
    arrange(Major_Group_En,desc(f_area_type))%>%
    mutate(n=n/1000)%>%
    mutate(lab_ypos = ifelse(f_area_type=="marine",-20, cumsum(n)+20))

  #### Figure

  fig<-ggplot(data=com_area_type,aes(x=Major_Group_En,y=n,fill=f_area_type))+
    geom_bar(stat="identity",position="stack")+
    geom_text(aes(y=lab_ypos,label=scales::comma(n),colour=f_area_type), size=3.5,show.legend = F)+
    scale_fill_manual(values=c("#F7DC6F", "#85C1E9"))+
    scale_color_manual(values=c("#C3A606", "#054478"))+
    labs(x=NULL,y="Number of records in FishstatJ\n (in thousands)",caption="SOURCE : FAO Global Capture Production",fill=NULL)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
    theme(axis.title.y = element_text(size=10),
          axis.text.y = element_text(size=8),
          axis.text.x = element_text(size=8,angle = 0, vjust = 0.5, hjust=0.5),
          legend.background = element_rect(fill="transparent"),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.direction = "horizontal",
          panel.background = element_rect(fill="#f0f0f0"),
          panel.grid.major.y = element_line(linetype="dotted",color="grey50"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=8,color="grey20"),
          plot.margin = unit(c(0,0.5,0.5,0), "cm"))

  fig_blue<-ggplot(data=com_area_type,aes(x=Major_Group_En,y=n,fill=f_area_type))+
    geom_bar(stat="identity",position="stack")+
    geom_text(aes(y=lab_ypos,label=scales::comma(n),colour=f_area_type), size=3.5,show.legend = F)+
    scale_fill_manual(values=c("#80dfff", "#0099cc"))+
    scale_color_manual(values=c("#1ac6ff", "#0086b3"))+
    labs(x=NULL,y="Number of records in FishstatJ\n (in thousands)",caption="SOURCE : FAO Global Capture Production",fill=NULL)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
    theme(axis.title.y = element_text(size=10),
          axis.text.y = element_text(size=8),
          axis.text.x = element_text(size=8,angle = 0, vjust = 0.5, hjust=0.5),
          legend.background = element_rect(fill="transparent"),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.direction = "horizontal",
          panel.background = element_rect(fill="#f0f0f0"),
          panel.grid.major.y = element_line(linetype="dotted",color="grey50"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=8,color="grey20"),
          plot.margin = unit(c(0,0.5,0.5,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,width=7,height=5)
  ggsave(paste0(name,"_blue.png"),fig_blue,"png",path=fig_path,width=7,height=5)
  content$fig[name]<-list(file.path(fig_path,fig_name))

  #### Table

  tab_name<-paste0("ref_",name,".csv")

  write.csv(com_area_type%>%
              select(Major_Group_En,f_area_type,n)%>%
              rename(ISSCAAP_Group = Major_Group_En)%>%
              rename(Number_thousand = n)%>%
              rename(FAO_Area_Type = f_area_type)
            ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE)

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
  content$text[name]<-list("Text Place Holder")

###############################################################################################################################
  ## Commonness of Taxonomic Group records in FishStatJ — by Ocean Basin
  ### Output :com_isscaap_ocean
  name<-"com_isscaap_ocean"
  #### Data
  com_ocean_group <- data %>%
    filter(f_area_type=="marine")%>%
    select(c("ocean","flag","Major_Group_En","capture")) %>%
    count(ocean,flag,Major_Group_En)%>%
    group_by(ocean,Major_Group_En)%>%
    mutate(n=n/1000)%>%
    summarise(median = median(n))%>%
    ungroup()%>%
    complete(ocean, nesting(Major_Group_En),fill=list(median=0))

  nb_col<-2
  nb_height<-ceiling(length(unique(com_ocean_group$ocean))/nb_col)

  #### Figure

  fig<-ggplot(data=com_ocean_group,aes(x=Major_Group_En,y=median,fill=Major_Group_En))+
    geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
    geom_text(aes(label=scales::comma(round(median,2))), vjust=-0.3, color="grey40", size=4)+
    facet_wrap(~ocean,ncol=nb_col)+
    ISSCAAPScale+
    labs(x=NULL,y="Number of records by ocean\n (median in thousands)",caption="SOURCE : FAO Global Capture Production",fill=NULL)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
    coord_cartesian(clip = 'off') +
    theme(axis.title.y = element_text(size=10*nb_col),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12,angle = 0, vjust = 0.5, hjust=0.5),
          strip.background = element_rect(color="black", fill="white", linetype="blank"),
          strip.text = element_text(size = 15),
          panel.background = element_rect(fill="#f0f0f0"),
          panel.grid.major.y = element_line(linetype="dotted",color="grey50"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=7*nb_col,color="grey20"),
          plot.margin = unit(c(0,0.5,0.5,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,width=7*nb_col,height=5*nb_height)
  content$fig[name]<-list(file.path(fig_path,fig_name))


  ####Table

  tab_name<-paste0("ref_",name,".csv")

  write.csv(com_ocean_group%>%
              select(Major_Group_En,ocean,median)%>%
              rename(Ocean = ocean)%>%
              rename(ISSCAAP_Group = Major_Group_En)%>%
              rename(Median_thousand = median)
            ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE)

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
  content$text[name]<-list("Text Place Holder")

###############################################################################################################################
  ## Commonness of Taxonomic Group records in FishStatJ — by FAO Fishery Areas
  ### Output :com_isccap_f_area
  #### Data

  com_area_group <- data %>%
    select(c("f_area_type","f_area","f_area_label","flag","Major_Group_En","capture")) %>%
    count(f_area_type,f_area,f_area_label,flag,Major_Group_En)%>%
    group_by(f_area_type,f_area,f_area_label,Major_Group_En)%>%
    mutate(n=n/1000)%>%
    summarise(median = median(n))%>%
    group_by(f_area_type,f_area,f_area_label)%>%
    complete(Major_Group_En,fill=list(median=0))

  for(i in unique(com_area_group$f_area_type)){

    name<-paste0("com_isccap_f_area_",i)

     com_area_type <- com_area_group %>%
      filter(f_area_type==i)

     nb_col<-3
     nb_height<-ceiling(length(unique(com_area_type$f_area_label))/nb_col)

  #### Figure

     fig<-ggplot(data=com_area_type,aes(x=Major_Group_En,y=median,fill=Major_Group_En))+
        geom_bar(stat="identity",width = 0.8,show.legend = FALSE)+
        geom_text(aes(label=scales::comma(round(median,2))), vjust=-0.3, color="grey40", size=3.5)+
        facet_wrap(~f_area_label,ncol=nb_col)+
        ISSCAAPScale+
        labs(x=NULL,y=paste0("Number of records by FAO ",i," area\n (median in thousands)"),caption="SOURCE : FAO Global Capture Production",fill=NULL)+
        scale_y_continuous(labels = scales::comma)+
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
        theme(axis.title.y = element_text(size=10*nb_col),
              axis.text.y = element_text(size=10),
              axis.text.x = element_text(size=10,angle = 0, vjust = 0.5, hjust=0.5),
              strip.background = element_rect(color="black", fill="white", size=5, linetype="blank"),
              panel.background = element_rect(fill="#f0f0f0"),
              panel.grid.major.y = element_line(linetype="dotted",color="grey50"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              plot.caption = element_text(face = "italic",hjust=0,size=8*nb_col,color="grey20"),
              plot.margin = unit(c(0,0.5,0.5,0), "cm"))

      fig_name<-paste0(name,".png")
      ggsave(fig_name,fig,"png",path=fig_path,width=7*nb_col,height=5*nb_height)
      content$fig[name]<-list(file.path(fig_path,fig_name))

  #### Table

      tab_name<-paste0("ref_",name,".csv")

      write.csv(com_area_type%>%
                  rename(ISSCAAP_Group = Major_Group_En)%>%
                  rename(Median_thousand = median)
                ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE)

      content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
      content$text[name]<-list("Text Place Holder")
  }

###############################################################################################################################
#Part 2 Biodiversity reporting development in FAO Fisheries
  ## Commonness among Pisces [vertebrate fish] records in FishStatJ — Global
  ### Output :com_sp_global
  name<-"com_sp_global"

  #### Data
  com_sp_lvl <- data %>%
    filter(Major_Group_En %in% c("PISCES")) %>%
    select(c("species","scientific_name","family","order","Taxonomic_Code","year","capture")) %>%
    mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
    count(year,level)%>%
    group_by(year)%>%
    mutate(sum = sum(n))%>%
    mutate(pour = n/sum*100)%>%
    group_by(level) %>%
    group_by(level,grp = as.integer(gl(n(), 3, n()))) %>%
    summarise(year = as.numeric(first(year)), pour = mean(pour)) %>%
    select(-grp)

  com_sp_lvl$level<-factor(com_sp_lvl$level,levels = c("species","nei","family"))

  #### Figure

  fig<-ggplot(data=com_sp_lvl,aes(x=year,y=pour,group=level))+
    geom_line(aes(linetype=level))+
    TaxonomicScale+
    labs(x=NULL,y="Pourcentage of records\n (%)",caption="SOURCE : FAO Global Capture Production",linetype=NULL,size=1.5)+
    ylim(0,100)+
    theme(axis.title.y = element_text(size=10),
          axis.text.y = element_text(size=8),
          axis.text.x = element_text(size=8,angle = 0, vjust = 0.5, hjust=0.5),
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.background = element_rect(fill="transparent"),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.direction = "horizontal",
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(linetype="solid",color="grey80"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=8,color="grey20"),
          plot.margin = unit(c(0,0.5,0.5,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,width=7,height=5)
  content$fig[name]<-list(file.path(fig_path,fig_name))

  #### Table

  tab_name<-paste0("ref_",name,".csv")

  write.csv(com_sp_lvl%>%
              rename(Pourcentage = pour)
            ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE)
  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text

  content$text[name]<-list("Text Place Holder")

###############################################################################################################################
  ## Commonness among fish records in FishStatJ — by Ocean Basins
  ### Output :com_sp_ocean
  name<-"com_sp_ocean"
  #### Data

  com_sp_ocean <- data %>%
    filter(f_area_type=="marine") %>%
    filter(Major_Group_En %in% c("PISCES")) %>%
    select(c("flag","ocean","species","scientific_name","family","order","Taxonomic_Code","year","capture")) %>%
    mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
    count(flag,ocean,year,level)%>%
    group_by(ocean,year,level)%>%
    summarise(n=median(n))%>%
    ungroup()%>%
    complete(ocean, nesting(year, level),fill=list(n=0))%>%
    group_by(ocean,year)%>%
    mutate(sum = sum(n))%>%
    mutate(pour = n/sum*100)%>%
    group_by(ocean,level) %>%
    group_by(ocean,level,grp = as.integer(gl(n(), 3, n()))) %>%
    summarise(year = as.numeric(first(year)), pour = mean(pour))%>%
    select(-grp)

  com_sp_ocean$level<-factor(com_sp_ocean$level,levels = c("species","family","nei"))

  ##### Figure

  nb_col<-2
  nb_height<-ceiling(length(unique(com_sp_ocean$ocean))/nb_col)

  fig<-ggplot(data=com_sp_ocean,aes(x=year,y=pour,group=level))+
    geom_line(aes(linetype=level))+
    TaxonomicScale+
    facet_wrap(~ocean,ncol=nb_col)+
    labs(x=NULL,y="Pourcentage of records by ocean\n (%)",caption="SOURCE : FAO Global Capture Production",linetype=NULL)+
    ylim(0,100)+
    theme(axis.title.y = element_text(size=10*nb_col),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12,angle = 0, vjust = 0.5, hjust=0.5),
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.background = element_rect(fill="transparent"),
          legend.position = c(1, 0),
          legend.justification = c(1, 0),
          legend.direction = "horizontal",
          strip.background = element_rect(color="black", fill="white", linetype="blank"),
          strip.text = element_text(size = 15),
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(linetype="solid",color="grey80"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=7*nb_col,color="grey20"),
          plot.margin = unit(c(0,0.5,0.5,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,width=7*nb_col,height=5*nb_height)
  content$fig[name]<-list(file.path(fig_path,fig_name))

#### Table

  tab_name<-paste0("ref_",name,".csv")

  write.csv(com_sp_ocean%>%
              rename(Pourcentage = pour)
            ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE)

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text

  content$text[name]<-list("Text Place Holder")

###############################################################################################################################
  ## Commonness among fish records in FishStatJ — By FAO Fisheries Areas
  ### Output :com_sp_f_area
  #### Data

  for(i in unique(data$f_area_type)){

    name<-paste0("com_sp_f_area_",i)

    com_sp_area_type <- data %>%
      filter(Major_Group_En %in% c("PISCES")) %>%
      filter(f_area_type == i) %>%
      select(c("flag","f_area_label","species","scientific_name","family","order","Taxonomic_Code","year","capture")) %>%
      mutate(level=ifelse(stringr::str_detect(Taxonomic_Code,"X")==FALSE,"species",ifelse(is.na(family),"nei","family")))%>%
      count(flag,f_area_label,year,level)%>%
      group_by(f_area_label,year,level)%>%
      summarise(n=median(n))%>%
      ungroup()%>%
      complete(f_area_label, nesting(year, level),fill=list(n=0))%>%
      group_by(f_area_label,year)%>%
      mutate(sum = sum(n))%>%
      mutate(pour = n/sum*100)%>%
      group_by(f_area_label,level) %>%
      group_by(f_area_label,level,grp = as.integer(gl(n(), 3, n()))) %>%
      summarise(year = as.numeric(first(year)), pour = mean(pour))%>%
      select(-grp)

    com_sp_area_type$level<-factor(com_sp_area_type$level,levels = c("species","family","nei"))

    nb_col<-3
    nb_height<-ceiling(length(unique(com_sp_area_type$f_area_label))/nb_col)

    #### Figure

  fig<-ggplot(data=com_sp_area_type,aes(x=year,y=pour))+
    geom_line(aes(linetype=level))+
    TaxonomicScale+
    facet_wrap(~f_area_label,ncol=nb_col)+
    labs(x=NULL,y=paste0("Pourcentage of records by FAO ",i," area\n (%)"),caption="SOURCE : FAO Global Capture Production",linetype=NULL)+
    ylim(0,100)+
    theme(axis.title.y = element_text(size=10*nb_col),
          axis.text.y = element_text(size=10),
          axis.text.x = element_text(size=10,angle = 0, vjust = 0.5, hjust=0.5),
          legend.key = element_rect(fill = "white"),
          legend.title = element_blank(),
          legend.background = element_rect(fill="transparent"),
          legend.position = c(1, 0),
          legend.justification = c(1, 0),
          legend.direction = "horizontal",
          strip.background = element_rect(color="black", fill="white", size=5, linetype="blank"),
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(linetype="solid",color="grey80"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(face = "italic",hjust=0,size=8*nb_col,color="grey20"),
          plot.margin = unit(c(0,0.5,0.5,0), "cm"))

  fig_name<-paste0(name,".png")
  ggsave(fig_name,fig,"png",path=fig_path,width=7*nb_col,height=5*nb_height)
  content$fig[name]<-list(file.path(fig_path,fig_name))

  #### Table

  tab_name<-paste0("ref_",name,".csv")

  write.csv(com_sp_area_type%>%
              rename(Pourcentage = pour)
            ,file.path(tab_path,tab_name),row.names = FALSE,quote=FALSE)

  content$tab[name]<-list(file.path(tab_path,tab_name))

  #### Text
  content$text[name]<-list("Text Place Holder")
}

  return(content)
}
