##Menggunakan library yang dibutuhkan

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
#================================================================================
##Data Volve
df<-read.csv('D:/Bhakti/9. R Studio/02. Input Data/PROD_EQUINOR.csv') ## Harus disesuaikan dengan folder masing-masing
df$DATEPRD <-lubridate::dmy(as.character(df$DATEPRD)) # Mengganti format menjadi tanggal


#======GGPLOT====================================================================
##Rate Oil, Water, Gas
ggplot() + ggtitle('Oil, Water & Gas Production') + ylab('STB or SCF') + xlab('YEAR') +
  geom_point( data = df, aes(DATEPRD, BORE_OIL_VOL), colour = '#7FFF00')+
  geom_point(data = df, aes(DATEPRD, BORE_WAT_VOL), colour = '#00FFFF')+
  geom_point(data = df, aes(DATEPRD, BORE_GAS_VOL), colour = '#F08080')+
  facet_wrap(~WELL_BORE_CODE, ncol = 2)+theme_bw() + scale_y_continuous(trans='log10')
##Pressure
ggplot() + ggtitle('Pressure') + ylab('Psia') + xlab('YEAR') +
  geom_point( data = df, aes(DATEPRD, AVG_DOWNHOLE_PRESSURE), colour = 'darkblue')+
  facet_wrap(~WELL_BORE_CODE, ncol = 2)+theme_bw() 
##Barplot
df2.agg <- df %>% group_by(WELL_BORE_CODE) %>%summarize(NpOil=sum(BORE_OIL_VOL))

ggplot(data = df2.agg, aes(x=WELL_BORE_CODE, y=NpOil, fill=NpOil)) + 
  geom_bar(stat="identity", width=0.5, fill="#69b3a2")  + ggtitle("CUM OIL ~ VOLVE FIELD") + theme(legend.position="bottom")

ggplot(data = df2.agg, aes(x=WELL_BORE_CODE, y=NpOil) ) +
  geom_segment( aes(x=WELL_BORE_CODE ,xend=WELL_BORE_CODE, y=1, yend=NpOil), color="grey") +
  geom_point(size=5, color="#69b3a2") +
  coord_flip() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") + theme_classic() +ggtitle('CUM OIL by Well')


##=========================Gif==========================================
# libraries:
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(gifski)
library(png)


##Function==============================================================
##membuat fungsi untuk mereplace nama wellbore code dengan nama sumur
WELL<- function(WELL_BORE_CODE){
  WELL_BORE_CODE<- as.character(WELL_BORE_CODE)
  if (WELL_BORE_CODE=="NO 15/9-F-1 C"){
    return("Well#1")
  }else if (WELL_BORE_CODE=="NO 15/9-F-11 H"){
    return("Well#2")
  } else if (WELL_BORE_CODE=="NO 15/9-F-12 H"){
    return("Well#3")
  } else if (WELL_BORE_CODE=="NO 15/9-F-14 H"){
    return("Well#4")
  }else if (WELL_BORE_CODE=="NO 15/9-F-15 D"){
    return("Well#5")
  }else if (WELL_BORE_CODE=="NO 15/9-F-4 AH"){
    return("Well#6")
  }else {
    return("Well#7")
  }
}

#Menggunakan fungsi "simple_sand" dengan "apply" untuk mengaplikasikannya ke kolom sand
df$WELL <- lapply(df$WELL_BORE_CODE, WELL)
df$WELL <- as.character(df$WELL)
df3 <- df %>% select(YEAR, WELL, BORE_OIL_VOL)
df3 <- na.omit(df3)
df3 <- data.frame(df3)


df3 %>%
  ggplot( aes(x=YEAR, y=BORE_OIL_VOL, group=WELL, color=WELL)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Production Oil") +
  theme_ipsum() +
  ylab("STB") +
  transition_reveal(YEAR)

# Save at gif:
anim_save("OilProd_Volve_FIELD.gif")

##test
