img_path<- "images/"
knitr::include_graphics(file.path(img_path,"ggplot.png"))

data("ToothGrowth")
library(dplyr)
library(ggplot2)

ToothGrowth %>% ggplot()

ggplot(data = ToothGrowth)

ToothGrowth %>% ggplot(aes(x = dose, y = len, fill=supp))

ToothGrowth %>% 
  ggplot(aes(x = dose, y = len, color=supp)) +
  geom_point()

ToothGrowth %>% 
  ggplot(aes(x = dose, y = len, color=supp)) +
  geom_line()

ToothGrowth %>%  
  mutate(dose=case_when(dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, color=supp)) +  
  geom_boxplot()

# ToothGrowth %>%
#   mutate(dose=case_when(
#     dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>%
#   ggplot(aes( x = dose, y = len, fill=supp))+
#   geom_bar(stat = "identity", position = "dodge")

ToothGrowth %>%  
  mutate(dose=case_when( dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(aes( x = dose, y = len, fill=supp))+
  geom_bar(stat = "identity", position = "dodge")

# ToothGrowth %>% ggplot(
#   aes(x = dose, y = len, color=supp)) +
#   geom_point()+
#   geom_text(aes(label=rownames(ToothGrowth)))+
#   geom_line()

ToothGrowth %>% ggplot(aes(x = dose, y = len, color=supp)) +
  geom_point()+
  geom_text(aes(label=rownames(ToothGrowth)))+
  geom_line()

img_path<- "images/"
knitr::include_graphics(file.path(img_path,"colors.jpg"))

ToothGrowth %>%  mutate(dose=case_when(
dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, col=supp)) +  
  geom_boxplot()+
  scale_color_manual( values = c("#FF00FF","#00FFFF"))

ToothGrowth %>%  mutate(dose=case_when( dose==0.5~"D0.5",dose==1~"D1",
  dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, fill=supp)) + 
  geom_boxplot()+
  scale_fill_manual(values = c("#FF00FF","#00FFFF"))

library(imager)
library(cowplot)
file1<-load.image("images/rcolor.png")
file2<-load.image("images/viridis.png")
p1 <- ggdraw() + draw_image(file1, scale = 1)
p2 <- ggdraw() + draw_image(file2, scale = 1)
plot_grid(p1,p2)

ToothGrowth %>%  mutate(
  dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, fill=supp)) +  
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "none")

ToothGrowth %>%  mutate(
  dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(
    aes(x = dose, y = len, fill=supp)) +  
  geom_boxplot()+
  scale_fill_viridis_d(option = "C")+
  theme(legend.position = "none")

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(
    aes(x = dose, y = len, fill=supp)) +  
  geom_boxplot()+
  ylab("Longitud diente")+
  xlab("Dosis")+
  ggtitle("Longitud de dientes por Dosis aplicada")

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(
    aes(x = dose, y = len, fill=supp)) +  
  geom_boxplot()+
  ylab("Longitud diente")+  xlab("Dosis")+
  ggtitle("Longitud de dientes por Dosis aplicada") +
  scale_x_discrete(limits = c("D2", "D0.5", "D1"), position ="bottom" )+
  scale_y_continuous(breaks = c(0, 20,40))

par(mfrow= c(2,3))
a1<-ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ theme_classic()+
  ggtitle("theme_classic()")

a2<-ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ theme_linedraw()+
  ggtitle("theme_linedraw()")

a3<-ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ theme_bw()+
  ggtitle("theme_bw()")

a4<-ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ theme_get()+
  ggtitle("theme_get()")

a5<-ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ theme_dark()+
  ggtitle("theme_dark()")

a6<-ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ theme_light()+
  ggtitle("theme_light()")

library(cowplot)
plot_grid(a1,a2,a3,a4,a5,a6, ncol = 3, nrow = 2)


ToothGrowth %>% 
  mutate(dose=case_when(  
    dose==0.5~"D0.5",dose==1~"D1",
  dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, fill=supp)) + 
  geom_boxplot()+ 
  theme(axis.title = element_text(size = 14, colour = "blue"),
  title = element_text(size = 16, colour = "red"), legend.position = "top")

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, fill=supp)) +  
  geom_boxplot()+ 
  facet_grid(supp~.)

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, fill=supp)) +  
  geom_boxplot()+ 
  facet_grid(.~supp)

ToothGrowth %>%  mutate(dose=case_when(dose==0.5~"D0.5",dose==1~"D1", 
dose==2~"D2")) %>% 
  ggplot(aes(x = dose, y = len, fill=supp))+
  geom_boxplot()+ 
  facet_wrap(~supp+dose, ncol = 3, nrow = 2)

# ggsave(filename = "plot.png",
#        plot = a1,
#        dpi = 300,
#        width = 4,
#        height = 3.5)
