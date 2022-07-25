"
***************************************************************************
viz_dana.R
---------------------
Date                 : July 2022
Copyright            : (C) 2022 by Felipe Carranza
Email                : fcarranza@protonmail.com
***************************************************************************
*                                                                         *
*License is next coming
*                                                                         *
*                                                                         *
***************************************************************************
__author__ = 'Felipe Carranza'
__date__ = 'July 2022'
__copyright__ = '(C) 2022, Felipe Carranza'"
#TODO: improve temporal variables, order,  add custom labels, colors and fonts to graphs



##############
library(tidyverse)
library(googlesheets4)
library(naniar)
library(data.table)
library(hgchmagic)
library(lfltmagic)

#folder_clean="data/clean"
#folder_ori="data/original"

#################install.packages("googlesheets4")
# Gráfica 1
url2="https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit?usp=sharing"
data<- read_sheet(url2)
str(data)

# data$`Nombre del indicador`
# data %>% dplyr::select(`Nombre del indicador`)
#first row null
temp= data %>% dplyr::filter(is.na(`Nombre del indicador`)==FALSE)
#Searching NULL values
gg_miss_var(temp)
#TODO:#TODO: Label values, label axis, colors, font, order

opt_list = list(hor_title="Año",
                ver_title="Reservas mineras", text_family="Open Sans", text_size="17",
                background_color="transparent",
                title_color="#0F0F0F",
                tittle_size="18",
                title="Proyección de reservas mineras en 2020",
                title_align="center",
                # palette.colors=c("#22776A", "#0B5D78", "#2A819C" ,"#43A292")
                palette_colors=c("#2A819C", "#575756", "#A7A6A6"),
                text_color="#232323",
                format_sample_num="100,",
                axis_title_size="18")

gr1=hgch_line_CatYeaNum(temp %>% select(`Desagregación del indicador`,Años,Valor),
                        opt_list=opt_list )
gr1
htmlwidgets::saveWidget(gr1, "disponibilidad_reservas.html",background = "transparent")
# saveRDS(temp,"temporal_to_lftl_grf1")
# saveRDS(gr1,"temporal_img_1")
# library(savewidgets)
# install.packages("html")
############
#Grafica #2
url3="https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=2053630180"
data3<- read_sheet(url3,sheet=3)
str(data3)
# data3$`Nombre del indicador`
temp3= data3 %>% dplyr::filter(is.na(`Nombre del indicador`)==FALSE)
gg_miss_var(temp3)
# library(data.table); library(tidyverse)
# install.packages("naniar")
# hgch_line_DatNum(temp, date_intervals = "Year")
#TODO: Label values, label axis, colors, font, order
opt_list$title <- "Índices en base 100 = 2005"
opt_list$hor_title <-"Años"
opt_list$ver_title <- "Índice base 100"
opt_list$label_wrap_legend = 50
temp3 = temp3 %>% filter(`Variable asociada` %like% 'ndice') %>%     select(`Variable asociada`,Años,Valor)

temp3$`Variable asociada` <-  gsub(" Índices en base 100=2005", "", temp3$`Variable asociada`)
# unique(temp3$`Variable asociada`)
gr2 = hgch_line_CatYeaNum(temp3,

                          spline=TRUE,
                          marker_enabled=FALSE,
                          opt_list
)
gr2
htmlwidgets::saveWidget(gr2, "indices_base.html",background = "transparent")

# saveRDS(temp3,"temporal_to_lftl_grf2")
# saveRDS(gr2,"temporal_img_2")

#################################################
#Gráfica #3


url3="https://docs.google.com/spreadsheets/d/1s3RPwgNdPrTVq0kARqZwTSoKzk-8jGC5Ed2_ERXJoIg/edit#gid=2053630180"
data4<- read_sheet(url3,sheet=6)
str(data4)
gg_miss_var(data4)
# data4$Área
# temp3= data3 %>% dplyr::filter(is.na(`Nombre del indicador`)==FALSE)
# library(naniar)
#TODO apply upper case
temp4 = data4 %>% filter(Área %like% "Total" & Variable %like% "Total hogares") %>% as.data.frame()
temp4 = temp4 %>% filter(!Departamento %like% "nacional")
temp4 = temp4 %>% mutate(Departamento = case_when(Departamento == "Valle"  ~ "Valle del Cauca", TRUE ~ Departamento))
temp4 = temp4 %>% mutate(Departamento = case_when(Departamento == "Bogotá DC"  ~ "Bogotá, D.C.", TRUE ~ Departamento))
temp4 = temp4 %>%
  mutate(Departamento = case_when(Departamento == "San Andrés"  ~ "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA", TRUE ~ Departamento))

colnames(temp4) = c("Año","Departamento","Área","Variable","Estimador Total", "Porcentaje")
# hist(temp4$`Estimador Total`)
temp4=temp4 %>% select(Departamento,`Estimador Total`)
# str(temp4)
# temp5 = temp4
# temp5$Departamento = as.factor(temp5$Departamento)
# class(temp4$Departamento)
# class(temp4$`Estimador Total`)
# library(lfltmagic)
opt_list$title <- ""
opt_list$palette_colors  <- c("#EBF6FB", "#6ABFA2","#B4DEE0","#2CA361","#0B7032")
img3 <-lflt_choropleth_GnmNum(data=temp4,map_name = "col_departments", opt_list)
htmlwidgets::saveWidget(img3, "mapa_dpto.html",background = "transparent")
# saveRDS(temp4,"temporal_to_lftl_grf3")
# saveRDS(img3,"temporal_img_3")
