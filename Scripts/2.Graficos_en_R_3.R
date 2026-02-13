set.seed(1)
df.heatmap <- expand.grid(Var1 = letters[1:5], Var2 = 1:5)
df.heatmap$score <- runif(nrow(df.heatmap), min = -2, max = 2)

library(ggplot2)
ggplot(df.heatmap, aes(x = Var1, y = Var2, fill = score)) + 
  geom_tile()

ggplot(df.heatmap, aes(x = Var1, y = Var2, fill = score)) + 
  geom_tile() + 
  scale_fill_gradient2()


library(dplyr)
library(tidyr)
library(tibble)
mat = df.heatmap %>%
  pivot_wider(id_cols = Var2, names_from = Var1, values_from = score)%>%
  column_to_rownames(var = "Var2")

head(mat)

# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# 
# BiocManager::install("ComplexHeatmap")

# library(ComplexHeatmap)
# Heatmap(
#   as.matrix(mat),
#   name = "mat",
#   rect_gp = gpar(col = "white", lwd = 2),
#   column_title = "Heatmap",
#   column_km = 3,
#   top_annotation = HeatmapAnnotation(foo1 = 1:5, bar1 = anno_points(runif(5))),
#   right_annotation = rowAnnotation(foo2 = 5:1, bar2 = anno_barplot(runif(5)))
# )

library(ComplexHeatmap)
Heatmap(
  as.matrix(mat),
  name = "mat",
  rect_gp = gpar(col = "white", lwd = 2),
  column_title = "Heatmap",
  column_km = 3,
  top_annotation = HeatmapAnnotation(foo1 = 1:5, bar1 = anno_points(runif(5))),
  right_annotation = rowAnnotation(foo2 = 5:1, bar2 = anno_barplot(runif(5)))
)

# install.packages("ggVennDiagram")

library(ggVennDiagram)
x <- list(A = 1:5, B = 2:7)
ggVennDiagram(x) 

library(ggplot2)
library(ggVennDiagram)
x <- list(Spe1 = 1:5, Spe2 = 2:7)
ggVennDiagram(x) + 
  scale_fill_gradient(low="#00FFFF",high = "#FFB6C1")

# install.packages("networkD3")

library(networkD3)
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
  )
nodes <- data.frame(
  name=c(as.character(links$source), 
  as.character(links$target)) %>% unique()
)
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


# p <- sankeyNetwork(Links = links, Nodes = nodes,
#               Source = "IDsource", Target = "IDtarget",
#               Value = "value", NodeID = "name",
#               sinksRight=FALSE)
# p

# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic1.html"))

data(iris)

df <- iris %>%
  group_by(Species) %>%
  summarise(across(Sepal.Length, mean)) %>% 
  mutate(x=0, y=Sepal.Length)
head(df)

ggplot(df, aes(x = x, y = Species)) +
  geom_segment(aes(xend = y, yend = Species),
               color = "gray", linewidth = 1) +
  geom_point(aes(x = y),
             size = 7.5, shape = 21, fill = "purple") +
  geom_text(aes(x = y, label = round(y, 2)),
            color = "white", size = 2.8) +
  theme_minimal()+
  xlab("Promedio de Sepal Length")

# install.packages('ggordiplots')

# library(ggordiplots)
# ord <- prcomp(iris[, 1:4])
# p <- gg_ordiplot(ord,
#                  iris$Species,
#                  spiders = TRUE,
#                  ellipse = FALSE)
# p

library(ggordiplots)
ord <- prcomp(iris[, 1:4])
p <- gg_ordiplot(ord, 
                 iris$Species, 
                 spiders = TRUE, 
                 ellipse = FALSE)
p

# pcaData <- as.data.frame(ord$x[, 1:2])
# pcaData <- cbind(pcaData, iris$Species)
# colnames(pcaData) <- c("PC1", "PC2", "Species")
# 
# ggplot(pcaData) +
#   aes(PC1, PC2, color = Species, shape = Species) +
#   geom_point(size = 2) +
#   coord_fixed() +
#   xlab("PC1: 73%")+
#   ylab("PC2: 23%") +
#   stat_ellipse(geom="polygon", level=0.95, alpha=0.2)

pcaData <- as.data.frame(ord$x[, 1:2]) 
pcaData <- cbind(pcaData, iris$Species) 
colnames(pcaData) <- c("PC1", "PC2", "Species") 

ggplot(pcaData) +
  aes(PC1, PC2, color = Species, shape = Species) + 
  geom_point(size = 2) +
  coord_fixed() +
  xlab("PC1: 73%")+
  ylab("PC2: 23%") +
  stat_ellipse(geom="polygon", level=0.95, alpha=0.2) 

df <- iris %>% 
  group_by(Species) %>% 
  count() %>% 
  mutate(percent = n/150*100)

head(df)

library(scales)
ggplot(df,aes(x="",y=percent, fill=Species)) +
  geom_bar(stat = "identity",
           color="white")+
    geom_text(aes(label=percent(percent/100)),
              position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange"))+
    theme_void()+
    labs(title="Pie chart")


ggplot(df,aes(x=2,y=percent, fill=Species)) +
  geom_bar(stat = "identity",
           color="white")  +
    geom_text(aes(label=percent(percent/100)),
              position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","orange"))+
    theme_void()+
    labs(title="Donus chart")+
  xlim(0.5,2.5)

data("ToothGrowth")

ToothGrowth %>% ggplot(aes(x = dose, y = len)) +
  stat_summary(geom = "line", fun = mean, aes(group = supp, color = supp)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, aes(group = supp))

ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +    
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# install.packages("ggpubr")
# 

library(ggpubr) 
ggboxplot(ToothGrowth, 
          x = "dose", 
          y = "len", 
          color = "dose", 
          add = "jitter", 
          shape = "dose")

ggbarplot(ToothGrowth, 
          x = "dose", 
          y = "len", 
          fill = "dose", 
          add = "mean_se")

ggboxplot(ToothGrowth,
          x = "dose",
          y = "len", 
          color = "dose",           
          facet.by = "supp")

comparaciones <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") ) 
ggbarplot(ToothGrowth, x = "dose", 
          y = "len", fill = "dose", 
          add = "mean_sd", 
          legend = "right")+
  stat_compare_means(comparisons = comparaciones, label = "p.signif")

# install.packages("tidyplots")

library(tidyplots)

study |> 
  tidyplot(x = treatment, y = score, color = treatment) |> 
  add_mean_bar(alpha = 0.4) |> 
  add_sem_errorbar() |> 
  add_data_points_beeswarm()


study |> 
  tidyplot(x = group, y = score, color = dose) |> 
  add_mean_bar(alpha = 0.4) |> 
  add_mean_dash() |> 
  add_mean_value()


study |> 
  tidyplot(x = treatment, y = score, color = treatment) |> 
  add_boxplot() |> 
  add_test_pvalue(ref.group = 1)


energy |> 
  tidyplot(x = year, y = energy, color = energy_source) |> 
  add_barstack_absolute()


energy |> 
  dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |> 
  tidyplot(y = energy, color = energy_source) |> 
  add_donut() |> 
  adjust_size(width = 25, height = 25) |>
  split_plot(by = year)

# install.packages("plotly")

library(plotly)
fig <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
fig


Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)
fig <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo')
fig <- fig %>% add_trace(y = ~LA_Zoo, name = 'LA Zoo')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
fig


fig <- plot_ly(y = ~rnorm(50), type = "box")
fig <- fig %>% add_trace(y = ~rnorm(50, 1))

fig

# install.packages("remotes")
# remotes::install_github("R-CoderDotCom/ggcats@main")

library(ggcats) 
grid <- expand.grid(1:5, 3:1)  
df <- data.frame(x = grid[, 1], 
                 y = grid[, 2],               
                 image = c("nyancat", "bongo", "colonel", "grumpy",                            "hipster", "lil_bub", "maru", "mouth", "pop", "pop_close", "pusheen", "pusheen_pc", "toast", "venus", "shironeko")) 
ggplot(df) +  
  geom_cat(aes(x, y, cat = image), size = 5) +    
  geom_text(aes(x, y - 0.5, label = image), size = 2.5) +     
  xlim(c(0.25, 5.5)) +      
  ylim(c(0.25, 3.5))

library(ggcats)
ToothGrowth$cats <- factor(ToothGrowth$dose, levels = c(0.5,1,2),                            labels = c("mouth", "grumpy", "pusheen_pc"))  

ToothGrowth %>%  
  ggplot(aes(y = len, x = dose)) +   
  geom_cat(aes(cat = cats), size = 4) +  
  xlim(c(0.25, 2.25))

library(dplyr) 
df<- iris %>% slice(c(1:4)) 
ggtexttable(df, rows = NULL)

# ggtexttable(df, rows = NULL, theme = ttheme("blank"))
# ggtexttable(df, rows = NULL, theme = ttheme("light"))
# ggtexttable(df, rows = NULL, theme = ttheme("classic"))
# ggtexttable(df, rows = NULL, theme = ttheme("minimal"))
# ggtexttable(df, rows = NULL, theme = ttheme("lVioletWhite"))
# ggtexttable(df, rows = NULL, theme = ttheme("mVioletWhite"))

library(cowplot) 
a<-ggtexttable(df, rows = NULL, theme = ttheme("blank")) 
b<-ggtexttable(df, rows = NULL, theme = ttheme("light")) 
c<-ggtexttable(df, rows = NULL, theme = ttheme("classic")) 
d<-ggtexttable(df, rows = NULL, theme = ttheme("minimal")) 
e<-ggtexttable(df, rows = NULL, theme = ttheme("lVioletWhite"))
f<-ggtexttable(df, rows = NULL, theme = ttheme("mVioletWhite"))  
plot_grid(a,b,c,d,e,f, labels=c("blank", "light", "classic",                                  "minimal", "lVioletWhite", "mVioletWhite"), ncol = 2, nrow = 3)

ggtexttable(df, rows = NULL,  theme = ttheme("classic")) %>%    
  table_cell_font(row = 3, column = 2, face = "bold", color = "red")

ggtexttable(df, rows = NULL,  theme = ttheme("classic")) %>%    
  table_cell_bg(row = 2:5, column = 3, fill="yellow")

ggtexttable(df, rows = NULL,  theme = ttheme("classic")) %>%    
  tab_add_title(text = "Data iris",  size = 14, face="bold") %>%
  tab_add_footnote(text = "*Alguna nota", size = 10, face = "italic")
