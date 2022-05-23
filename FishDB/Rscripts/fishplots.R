library(ggplot2)
library(ggpubr)
#Variable definitions accessing f_trends and f_pop dataframe
f_pop <- FishDat
f_trends <- rivertrends
trendyear <- f_trends$Year
popyear <- f_pop$Year
abundance <- f_trends$Abundance
pH <- f_trends$pH
temp <- f_trends$Temp
riverlevel <- f_trends$`River Level`

#Water Chemistry and Broader Properties ("wcplot" = water chemistry plot (NUM))

wcplot1 <- ggplot(data = f_trends)+geom_line(mapping = aes(x = trendyear ,y = abundance))
wcplot2 <- ggplot(data = f_trends)+geom_line(mapping = aes(x = trendyear, y = pH))
wcplot3 <- ggplot(data = f_trends)+geom_line(mapping = aes(x = trendyear, y = temp))
wcplot4 <- ggplot(data = f_trends)+geom_line(mapping = aes(x = trendyear, y = River.Level))


ggarrange(wcplot1,wcplot2,wcplot3,wcplot4 + rremove("x.text"), 
          ncol = 3, nrow = 2)

#Population-Based Graphs Based on Family ("fpplot" = family pop plot (NUM))

#Paddlefish
ind <- f_pop$Family == "Polydontidae"
Polydontidae <- f_pop$Population[ind]
Tempdata <- data.frame(popyear,ind,Polydontidae)
fpplot1 <- ggplot(data = Tempdata)+geom_line(mapping = aes(x = popyear, y = Polydontidae))

#Redhorses
ind <- f_pop$Family == "Catostomidae"
Catostomidae <- f_pop$Population[ind]
Tempdata <- data.frame(popyear,ind,Catostomidae)
fpplot2 <- ggplot(data = Tempdata)+geom_line(mapping = aes(x = popyear, y = Catostomidae))

#Freshwater Drum
ind <- f_pop$Family == "Sciaenidae"
Sciaenidae <- f_pop$Population[ind]
Tempdata <- data.frame(popyear,ind,Sciaenidae)
fpplot3 <- ggplot(data = Tempdata)+geom_line(mapping = aes(x = popyear, y = Sciaenidae))

#Common Carp
ind <- f_pop$Family == "Cyprinidae"
Cyprinidae <- f_pop$Population[ind]
Tempdata <- data.frame(popyear,ind,Cyprinidae)
fpplot4 <- ggplot(data = Tempdata)+geom_line(mapping = aes(x = popyear, y = Cyprinidae))

#Bass
ind <- f_pop$Family == "Centrachidae"
Centrachidae <- f_pop$Population[ind]
Tempdata <- data.frame(popyear,ind,Centrachidae)
fpplot5 <- ggplot(data = Tempdata)+geom_line(mapping = aes(x = popyear, y = Centrachidae))

#Population Plot Side/Side
ggarrange(fpplot1,fpplot2,fpplot3,fpplot4, fpplot5 + rremove("x.text"), 
          ncol = 1, nrow = 5)

#Plot format for testing water chemistry conditions against population
ggarrange(fpplot1,wcplot4,wcplot1 + rremove("x.text"), 
          ncol = 1, nrow = 3)