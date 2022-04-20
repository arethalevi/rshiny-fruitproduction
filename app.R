library("shinydashboard")
library("shiny")
library(plotly)
library(ggplot2)
library(tidyverse)
library(sf)
library(dplyr)
library(tidyr)
library(ggthemes)
library(readr)
library(stringr)
library(gridExtra)


theme_update(plot.title = element_text(hjust = 0.5))

data = read.csv("produksi_buah.csv") 
data$Produksi <- as.numeric(as.character(data$Produksi))
data[is.na(data)]<-0
shp <- sf::read_sf("IDN_adm1/IDN_adm1.shp")

dep = data %>% 
  group_by(Tahun,Provinsi)%>%
  summarise(Produksi=sum(Produksi),.groups = 'drop')
dep$Buah <- "Keseluruhan"
dep$X <- c(5460:5669)
dep=dep[,order(ncol(dep):1)]

data=rbind(data,dep)

ui <- dashboardPage(skin='red',
                    dashboardHeader(title = "DASHBOARD"),
                    dashboardSidebar(collapsed = TRUE,
                                     sidebarMenu(
                                       menuItem("Visualization", tabName = "viz"))),
                    dashboardBody(
                      tabItems(tabItem(tabName = "viz",
                                       fluidPage(fluidRow(
                                         valueBoxOutput("value1"),
                                         valueBoxOutput("value2"),
                                         valueBoxOutput("value3")),
                                         fluidRow(
                                           column(width=6,
                                                  box(box(status= "primary",
                                                          width=NULL,
                                                          solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          plotlyOutput("lineplot")),
                                                      selectInput("buah_lp","Jenis Buah:",
                                                                  choices = c("Keseluruhan",
                                                                              "Alpukat","Belimbing","Duku","Durian","Jambu Biji","Jambu Air",
                                                                              "Jeruk Siam","Jeruk Besar","Mangga","Manggis","Nangka","Nenas",
                                                                              "Pepaya","Pisang","Rambutan","Salak","Sawo","Markisa",
                                                                              "Sirsak","Sukun","Melon","Semangka","Blewah","Apel","Anggur","Stroberi"),
                                                                  width="100%"),
                                                      selectInput("daerah_lp","Daerah:",
                                                                  choices = c('INDONESIA',
                                                                              'ACEH', 'SUMATERA UTARA', 'SUMATERA BARAT', 'RIAU', 'JAMBI',
                                                                              'SUMATERA SELATAN', 'BENGKULU', 'LAMPUNG', 'KEP. BANGKA BELITUNG',
                                                                              'KEP. RIAU', 'DKI JAKARTA', 'JAWA BARAT', 'JAWA TENGAH',
                                                                              'DI YOGYAKARTA', 'JAWA TIMUR', 'BANTEN', 'BALI',
                                                                              'NUSA TENGGARA BARAT', 'NUSA TENGGARA TIMUR', 'KALIMANTAN BARAT',
                                                                              'KALIMANTAN TENGAH', 'KALIMANTAN SELATAN', 'KALIMANTAN TIMUR',
                                                                              'KALIMANTAN UTARA', 'SULAWESI UTARA', 'SULAWESI TENGAH',
                                                                              'SULAWESI SELATAN', 'SULAWESI TENGGARA', 'GORONTALO',
                                                                              'SULAWESI BARAT', 'MALUKU', 'MALUKU UTARA', 'PAPUA BARAT', 'PAPUA'),
                                                                  width="100%"),
                                                      width=NULL,
                                                      solidHeader = TRUE)),
                                           column(width=6,
                                                  box(box(status= "primary",
                                                          width=NULL,solidHeader = TRUE,
                                                          collapsible = TRUE,
                                                          plotlyOutput("plotmap")),
                                                      selectInput("tahun_map","Tahun: ",
                                                                  choices = c(2020,2019,2018,2017,2016,2015),width="100%"),
                                                      selectInput("buah_map","Buah: ",
                                                                  choices = c("Keseluruhan",
                                                                              "Alpukat","Belimbing","Duku","Durian","Jambu Biji","Jambu Air",  
                                                                              "Jeruk Siam","Jeruk Besar","Mangga","Manggis","Nangka","Nenas",      
                                                                              "Pepaya","Pisang","Rambutan","Salak","Sawo","Markisa",    
                                                                              "Sirsak","Sukun","Melon","Semangka","Blewah","Apel",       
                                                                              "Anggur","Stroberi" 
                                                                  ),
                                                                  width="100%"),
                                                      width=NULL,
                                                      solidHeader=TRUE)
                                           )),
                                         fluidRow(column(width=12,
                                                         box(
                                                           fluidRow(column(width=6,box(status= "primary",
                                                                                       width=NULL,
                                                                                       solidHeader = TRUE,
                                                                                       collapsible = FALSE,
                                                                                       plotlyOutput("plotbara"),
                                                                                       height=320)),
                                                                    column(width=6,box(status= "primary",
                                                                                       width=NULL,
                                                                                       solidHeader = TRUE,
                                                                                       collapsible = FALSE,
                                                                                       plotlyOutput("plotbarb"),
                                                                                       height=320)
                                                                    )),
                                                           fluidRow(align="center",
                                                                    selectInput("tahun_bar","Tahun: ",
                                                                                choices = c(2020,2019,2018,2017,2016,2015),width="50%"),
                                                                    selectInput("daerah_bar","Daerah:",
                                                                                choices = c(
                                                                                  'INDONESIA',
                                                                                  'ACEH', 'SUMATERA UTARA', 'SUMATERA BARAT', 'RIAU', 'JAMBI',
                                                                                  'SUMATERA SELATAN', 'BENGKULU', 'LAMPUNG', 'KEP. BANGKA BELITUNG',
                                                                                  'KEP. RIAU', 'DKI JAKARTA', 'JAWA BARAT', 'JAWA TENGAH',
                                                                                  'DI YOGYAKARTA', 'JAWA TIMUR', 'BANTEN', 'BALI',
                                                                                  'NUSA TENGGARA BARAT', 'NUSA TENGGARA TIMUR', 'KALIMANTAN BARAT',
                                                                                  'KALIMANTAN TENGAH', 'KALIMANTAN SELATAN', 'KALIMANTAN TIMUR',
                                                                                  'KALIMANTAN UTARA', 'SULAWESI UTARA', 'SULAWESI TENGAH',
                                                                                  'SULAWESI SELATAN', 'SULAWESI TENGGARA', 'GORONTALO',
                                                                                  'SULAWESI BARAT', 'MALUKU', 'MALUKU UTARA', 'PAPUA BARAT', 'PAPUA'),
                                                                                width="50%"
                                                                    )
                                                           ),
                                                           width=NULL
                                                         ))
                                                  
                                         )
                                         
                                       ))
                      )))


server <- function(input, output) {
  z=data[data$Buah=="Keseluruhan"&data$Provinsi=="INDONESIA"&data$Tahun==2020,4]
  r= data[data$Buah=="Keseluruhan"&data$Provinsi=="INDONESIA"&data$Tahun==2019,4]
  x = round(((z-r)/r)*100,2)
  
  output$value2 <- renderValueBox({
    valueBox(
      sprintf("%s ton",formatC(z, format="d", big.mark='.',decimal.mark = ","))
      ,paste('Total Produksi Buah di Indonesia tahun 2020')
      ,color = "green")
  })
  
  output$value1 <- renderValueBox({
    
    valueBox(
      sprintf("%s ton",formatC(r, format="d", big.mark='.',decimal.mark = ","))
      ,'Total Produksi Buah di Indonesia tahun 2019'
      ,color = "purple")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      sprintf("%s %%",formatC(x, big.mark='.',decimal.mark = ","))
      ,paste('% chg 2019 to 2020')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  output$lineplot <- renderPlotly({
    alp = data[data$Provinsi==input$daerah_lp & data$Buah==input$buah_lp,]
    ggplotly(ggplot(alp,aes(x=Tahun,y=Produksi,group=1,
                            text=paste("Tahun: ",Tahun,
                                       "<br>Produksi: ",Produksi,"ton")))+
               geom_line(color="#408FA6")+
               geom_point(color="#408FA6")+
               labs(title=str_wrap(sprintf("Produksi tanaman %s di %s (2015-2020)",input$buah_lp,str_to_title(input$daerah_lp))),
                    subtitle = "(2015-2020)")+
               theme_hc()
             ,tooltip="text")
  })
  
  output$plotmap <- renderPlotly({
    da20alp = data[data$Tahun==input$tahun_map & data$Buah==input$buah_map,]
    da20alp <- da20alp %>% 
      mutate(Provinsi= if_else(Provinsi=="DKI JAKARTA", "JAKARTA RAYA",
                               if_else(Provinsi=="DI YOGYAKARTA", "YOGYAKARTA",
                                       if_else(Provinsi=="KEP. BANGKA BELITUNG", "BANGKA-BELITUNG",
                                               if_else(Provinsi=="PAPUA BARAT", "IRIAN JAYA BARAT",
                                                       if_else(Provinsi=="KEP. RIAU", "KEPULAUAN RIAU",
                                                               Provinsi))))),
             Provinsi = stringr::str_to_title(Provinsi))
    shp<-shp%>%
      left_join(da20alp,by=c("NAME_1"="Provinsi"))
    
    plot_ly(shp, color = ~Produksi, split = ~NAME_1, 
            span = I(1), showlegend = FALSE,
            text = ~paste("<b>", NAME_1, "</b><br>",
                          "<b>Produksi</b>:", Produksi, "ton"),
            hoveron = "fills", hoverinfo = "text") %>% 
      layout(title=sprintf("Peta Persebaran %s tahun %s",input$buah_map, input$tahun_map))
  })
  
  output$plotbara <- renderPlotly({
    da= data[data$Provinsi==input$daerah_bar & data$Tahun==input$tahun_bar,] 
    da = da%>% 
      arrange(desc(Produksi)) %>% 
      head(9)
    da=da[2:9,]
    
    ggplotly(ggplot(da, aes(text=paste("Buah: ",Buah,
                                       "<br>Produksi: ",Produksi,"ton"))) +
               geom_col(mapping=aes(x=reorder(Buah,Produksi),y=Produksi,fill=Buah))+
               labs(x="",y="",
                    title=str_wrap(sprintf("8 Buah dengan Produksi Terbanyak di %s Tahun %s",str_to_title(input$daerah_bar),input$tahun_bar)))+
               coord_flip()+
               theme_hc()+
               theme(legend.position = "none"), height = 300, tooltip="text")
  })
  output$plotbarb <- renderPlotly({
    da= data[data$Provinsi==input$daerah_bar & data$Tahun==input$tahun_bar,] 
    da = da%>% 
      arrange(Produksi) %>% 
      head(8)
    
    ggplotly(ggplot(da, aes(text=paste("Buah: ",Buah,
                                       "<br>Produksi: ",Produksi,"ton"))) +
               geom_col(mapping=aes(x=reorder(Buah,desc(Produksi)),y=Produksi,fill=Buah))+
               labs(x="",y="",
                    title=str_wrap(sprintf("8 Buah dengan Produksi Tersedikit di %s Tahun %s",str_to_title(input$daerah_bar),input$tahun_bar)))+
               coord_flip()+
               theme_hc()+
               theme(legend.position = "none"), height = 300,tooltip="text")
  })
}
shinyApp(ui, server)