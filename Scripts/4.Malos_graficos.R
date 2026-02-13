#install.packages("ggbeeswarm")
library(tidyverse)
library(ggbeeswarm) 
library(RColorBrewer) 
library(cowplot) 

set.seed(666)
group1 <- rnorm(n = 100, mean = 1, sd = 1)
group2 <- rlnorm(n = 100, 
                 meanlog = log(1^2/sqrt(1^2 + 1^2)), 
                 sdlog = sqrt(log(1+(1^2/1^2))))

sd(group1)
sd(group2)

mean(group1)
mean(group2)

median(group1)
median(group2)

groups_long <- cbind(
  group1,
  group2
) %>% 
  as.data.frame() %>% 
  gather("group", "response", 1:2)

head(groups_long)

t.test(group1, group2)


wilcox.test(group1, group2)

ks.test(group1, group2)

# bar <- groups_long %>%
#   ggplot(aes(x = group, y = response)) +
#   geom_bar(stat = "summary", fun = mean,
#            width = 0.7, alpha = 0.8,
#            aes(fill = group)) +
#   stat_summary(geom = "errorbar", fun.data = "mean_se",
#                width = 0.1, size = 1) +
#   scale_fill_manual(values = brewer.pal(8, "Accent")[1:2]) +
#   labs(x = "Group",
#        y = "Response",
#        caption = paste0("Es lo mismo!\nP = ",
#                         signif(t.test(group1, group2)$p.value, 2),
#                         " (t test)")) +
#   theme_classic() +
#   theme(
#     text = element_text(size = 12, face = "bold", color = "black"),
#     axis.text = element_text(color = "black"),
#     legend.position = "none",
#     plot.title = element_text(size = 10),
#     plot.caption = element_text(hjust = 0)
#   ) +
#   ggtitle(
#     paste0(
#       "group1: mean = ", signif(mean(group1), 2),
#       "; sd = ", signif(sd(group1), 2), "\n",
#       "group2: mean = ", signif(mean(group2), 2),
#       "; sd = ", signif(sd(group2), 2)
#     ))
# 
# 
# 
# bar

# box <- groups_long %>%
#   ggplot(aes(x = group, y = response)) +
#   geom_boxplot(width = 0.7, alpha = 0.8,
#           aes(fill = group)) +
#   scale_fill_manual(values = brewer.pal(8, "Accent")[1:2]) +
#   labs(x = "Group",
#        y = "Response",
#        caption = paste0("Hmmmmm...\nP = ",
#                         signif(wilcox.test(group1, group2)$p.value, 2),
#                         " (Wilcoxon rank sum test)")) +
#   theme_classic() +
#   theme(
#     text = element_text(size = 12, face = "bold", color = "black"),
#     axis.text = element_text(color = "black"),
#     legend.position = "none",
#     plot.title = element_text(size = 10),
#     plot.caption = element_text(hjust = 0)
#   ) +
#   ggtitle(
#     paste0(
#       "group1: median = ", signif(median(group1), 2),
#       "; IQR = ", signif(IQR(group1), 2), "\n",
#       "group2: median = ", signif(median(group2), 2),
#       "; IQR = ", signif(IQR(group2), 2)
#     )
#   )
# 
# box

# dotplot <- groups_long %>%
#   ggplot(aes(x = group, y = response)) +
#   ggbeeswarm::geom_quasirandom(
#     shape = 21, color = "white",
#     alpha = 0.8, size = 3,
#     aes(fill = group)
#   ) +
#   scale_fill_manual(values = brewer.pal(8, "Accent")[1:2]) +
#   labs(x = "Group",
#        y = "Response",
#         caption = paste0("OH!!!\nP = ",
#                         signif(ks.test(group1, group2)$p.value, 2),
#                         " (Kolmogorov–Smirnov test)")) +
#   theme_classic() +
#   theme(
#     text = element_text(size = 12, face = "bold", color = "black"),
#     axis.text = element_text(color = "black"),
#     legend.position = "none",
#     plot.title = element_text(size = 10),
#     plot.caption = element_text(hjust = 0)
#   ) +
#   ggtitle(
#     paste0(
#       "group1: median = ", signif(median(group1), 2),
#       "; IQR = ", signif(IQR(group1), 2), "\n",
#       "group2: median = ", signif(median(group2), 2),
#       "; IQR = ", signif(IQR(group2), 2)
#     )
#   )
# 
# dotplot

bar <- groups_long %>% 
  ggplot(aes(x = group, y = response)) +
  geom_bar(stat = "summary", fun = mean, 
           width = 0.7, alpha = 0.8,
           aes(fill = group)) + 
  stat_summary(geom = "errorbar", fun.data = "mean_se",
               width = 0.1, size = 1) +
  scale_fill_manual(values = brewer.pal(8, "Accent")[1:2]) +
  labs(x = "Group",
       y = "Response",
       caption = paste0("Son lo mismo!\nP = ", 
                        signif(t.test(group1, group2)$p.value, 2),
                        " (t test)")) +
  theme_classic() +
  theme(
    text = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(size = 10),
    plot.caption = element_text(hjust = 0)
  ) +
  ggtitle(
    paste0(
      "group1: mean = ", signif(mean(group1), 2), 
      "; sd = ", signif(sd(group1), 2), "\n",
      "group2: mean = ", signif(mean(group2), 2), 
      "; sd = ", signif(sd(group2), 2)
    )) 
  
  


box <- groups_long %>% 
  ggplot(aes(x = group, y = response)) +
  geom_boxplot(width = 0.7, alpha = 0.8, 
          aes(fill = group)) +
  scale_fill_manual(values = brewer.pal(8, "Accent")[1:2]) +
  labs(x = "Group",
       y = "Response",
       caption = paste0("Hmmmmm...\nP = ", 
                        signif(wilcox.test(group1, group2)$p.value, 2),
                        " (Wilcoxon rank sum test)")) +
  theme_classic() +
  theme(
    text = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(size = 10),
    plot.caption = element_text(hjust = 0)
  ) +
  ggtitle(
    paste0(
      "group1: median = ", signif(median(group1), 2), 
      "; IQR = ", signif(IQR(group1), 2), "\n",
      "group2: median = ", signif(median(group2), 2), 
      "; IQR = ", signif(IQR(group2), 2)
    )
  )


dotplot <- groups_long %>% 
  ggplot(aes(x = group, y = response)) +
  ggbeeswarm::geom_quasirandom(
    shape = 21, color = "white", 
    alpha = 0.8, size = 3,
    aes(fill = group)
  ) +
  scale_fill_manual(values = brewer.pal(8, "Accent")[1:2]) +
  labs(x = "Group",
       y = "Response",
        caption = paste0("OH!!!\nP = ", 
                        signif(ks.test(group1, group2)$p.value, 2),
                        " (Kolmogorov–Smirnov test)")) +
  theme_classic() +
  theme(
    text = element_text(size = 12, face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(size = 10),
    plot.caption = element_text(hjust = 0)
  ) +
  ggtitle(
    paste0(
      "group1: median = ", signif(median(group1), 2), 
      "; IQR = ", signif(IQR(group1), 2), "\n",
      "group2: median = ", signif(median(group2), 2), 
      "; IQR = ", signif(IQR(group2), 2)
    )
  )


library(cowplot)
cowplot::plot_grid(bar,box,dotplot, ncol = 3)

library(viridis) 
library(patchwork)

Dir1 <- data.frame(
  "FPKM" = c(5, 10, 20, 40, 60, 80, 100, 120),
  "Gene" = 1:8
)  

Dir2 <- data.frame(
  "log2FC" = c(-10, -4, -2, 0, 2, 4, 10),
  "Gene" = 1:7
)

Dir3 <- data.frame(
  "z.score" = c(-2.5, -2, -1, 0, 1, 2, 2.5),
  "Gene" = 1:7
)
  


A <- Dir1 %>% 
 ggplot(aes(x = Gene, y = FPKM)) +
  geom_hline(yintercept = 0, size = 1) +
  geom_segment(aes(x = Gene, xend = Gene), 
               yend = 0, size = 1.2, alpha = 0.8, color = "grey60") +
  geom_point(shape = 21, size = 4, aes(fill = FPKM)) +
  scale_fill_gradientn(colors = viridis(n = 10)) +
  labs(caption = "Está bien.") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "black"), 
    axis.text.y = element_blank(), 
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 12)
  ) +
  coord_flip()+
  ggtitle("Color más oscuro = Min\nColor más clar = Max")

B <- Dir1 %>% 
  ggplot(aes(x = " ", y = Gene)) +
  geom_tile(aes(fill = FPKM)) +
  scale_fill_gradientn(colors = viridis(n = 10)) +
  theme_void() +
  theme(
    legend.position = "none"
  )

Good1 <- wrap_plots(A, B, nrow = 1, widths = c(1, 0.1))

C <- Dir2 %>% 
 ggplot(aes(x = Gene, y = log2FC)) +
  geom_hline(yintercept = 0, size = 1) +
  geom_segment(aes(x = Gene, xend = Gene), 
               yend = 0, size = 1.2, alpha = 0.8, color = "grey60") +
  geom_point(shape = 21, size = 4, aes(fill = log2FC)) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu"))) +
  labs(caption = "Está bien.") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "black"), 
    axis.text.y = element_blank(), 
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 12)
  ) +
  coord_flip()+
  ggtitle("Color más claro = 0\nColor más oscuro = Max absoluto")

D <- Dir2 %>% 
  ggplot(aes(x = " ", y = Gene)) +
  geom_tile(aes(fill = log2FC)) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu"))) +
  theme_void() +
  theme(
    legend.position = "none"
  )

Good2 <- wrap_plots(C, D, nrow = 1, widths = c(1, 0.1))


E <- Dir3 %>% 
 ggplot(aes(x = Gene, y = z.score)) +
  geom_hline(yintercept = 0, size = 1) +
  geom_segment(aes(x = Gene, xend = Gene), 
               yend = 0, size = 1.2, alpha = 0.8, color = "grey60") +
  geom_point(shape = 21, size = 4, aes(fill = z.score)) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu")) +
  labs(caption = "Está bien.",
       y = "z score") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "black"), 
    axis.text.y = element_blank(), 
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 12)
  ) +
  coord_flip()+
  ggtitle("Color más oscuro = Max\nColor más claro = Min")

eF <- Dir3 %>% 
  ggplot(aes(x = " ", y = Gene)) +
  geom_tile(aes(fill = z.score)) +
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu")) +
  theme_void() +
  theme(
    legend.position = "none"
  )


Good3 <- wrap_plots(E, eF, nrow = 1, widths = c(1, 0.1))

G <- Dir1 %>% 
 ggplot(aes(x = Gene, y = FPKM)) +
  geom_hline(yintercept = 0, size = 1) +
  geom_segment(aes(x = Gene, xend = Gene), 
               yend = 0, size = 1.2, alpha = 0.8, color = "grey60") +
  geom_point(shape = 21, size = 4, aes(fill = FPKM)) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu"))) +
  labs(caption = "Un pecado!") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "black"), 
    axis.text.y = element_blank(), 
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 12)
  ) +
  coord_flip()+
  ggtitle("El color más claro no significa nada\n(ni el promedio o mediana).")

H <- Dir1 %>% 
  ggplot(aes(x = " ", y = Gene)) +
  geom_tile(aes(fill = FPKM)) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu"))) +
  theme_void() +
  theme(
    legend.position = "none"
  )

Bad <- wrap_plots(G, H, nrow = 1, widths = c(1, 0.1))



cowplot::plot_grid(
  Good1, Good2, Good3, Bad,
  nrow = 2, ncol = 2
)

heatmap_data <- read_csv("../Data/deidentified_cell_feature_data.csv")

head(heatmap_data)

heatmap_data %>% 
  group_by(cell_type) %>% 
  count()

# sin_ordenar <- heatmap_data %>%
#   mutate(z2 = case_when(
#     z >= 3 ~ 3,
#     T ~ z
#   )) %>%
#   ggplot(aes(x = cell, y = feature)) +
#   geom_tile(aes(fill = z2)) +
#   scale_fill_gradientn(colors = brewer.pal(9, "PuBuGn"),
#                        breaks = c(0, 3),
#                        labels = c("0", ">3")) +
#   scale_x_discrete(labels = NULL) +
#   labs(x = "Cell",
#        y = "Feature",
#        fill = "z score",
#        title = "Sin ordenar") +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14, color = "black"),
#     axis.text = element_text(color = "black"),
#     legend.position = "bottom",
#     axis.ticks.x = element_blank(),
#     plot.caption = element_text(hjust = 0),
#     plot.title = element_text(size = 12, face = "bold")
#   )
# 
# sin_ordenar

sin_ordenar <- heatmap_data %>% 
  mutate(z2 = case_when(
    z >= 3 ~ 3,
    T ~ z
  )) %>% 
  ggplot(aes(x = cell, y = feature)) +
  geom_tile(aes(fill = z2)) +
  scale_fill_gradientn(colors = brewer.pal(9, "PuBuGn"), 
                       breaks = c(0, 3),
                       labels = c("0", ">3")) +
  scale_x_discrete(labels = NULL) +
  labs(x = "Cell",
       y = "Feature",
       fill = "z score",
       title = "Sin ordenar") +
  theme_classic() +
  theme(
    text = element_text(size = 14, color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    axis.ticks.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(size = 12, face = "bold")
  )

sin_ordenar

# ordenando <- heatmap_data %>%
#   mutate(z2 = case_when(
#     z >= 3 ~ 3,
#     T ~ z
#   )) %>%
#   mutate(tag = case_when(
#     str_detect(feature, " 7") ~ 1,
#     str_detect(feature, " 8| 6| 1$") ~ 2,
#     str_detect(feature, " 2| 3| 4| 10") ~ 4,
#     T ~ 3
#   )) %>%
#   mutate(feature = reorder(feature, -tag)) %>%
#   ggplot(aes(x = cell, y = feature)) +
#   facet_grid(. ~ cell_type, scales = "free_x", space = "free_x") +
#   geom_tile(aes(fill = z2)) +
#   scale_fill_gradientn(colors = brewer.pal(9, "PuBuGn"),
#                        breaks = c(0, 3),
#                        labels = c("0", ">3")) +
#   scale_x_discrete(labels = NULL) +
#   labs(x = "Cell",
#        y = "Feature",
#        fill = "z score",
#        title = "Ordenando filas y columnas") +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14, color = "black"),
#     axis.text = element_text(color = "black"),
#     legend.position = "bottom",
#     axis.ticks.x = element_blank(),
#     plot.caption = element_text(hjust = 0),
#     plot.title = element_text(size = 12, face = "bold")
#   )
# 
# ordenando

ordenando <- heatmap_data %>% 
  mutate(z2 = case_when(
    z >= 3 ~ 3,
    T ~ z
  )) %>% 
  mutate(tag = case_when(
    str_detect(feature, " 7") ~ 1,
    str_detect(feature, " 8| 6| 1$") ~ 2,
    str_detect(feature, " 2| 3| 4| 10") ~ 4,
    T ~ 3
  )) %>% 
  mutate(feature = reorder(feature, -tag)) %>% 
  ggplot(aes(x = cell, y = feature)) +
  facet_grid(. ~ cell_type, scales = "free_x", space = "free_x") +
  geom_tile(aes(fill = z2)) +
  scale_fill_gradientn(colors = brewer.pal(9, "PuBuGn"), 
                       breaks = c(0, 3),
                       labels = c("0", ">3")) +
  scale_x_discrete(labels = NULL) +
  labs(x = "Cell",
       y = "Feature",
       fill = "z score",
       title = "Ordenando filas y columnas") +
  theme_classic() +
  theme(
    text = element_text(size = 14, color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    axis.ticks.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(size = 12, face = "bold")
  )

ordenando

set.seed(666)
group1 <- rnorm(mean = 1, sd = 0.2, n = 8)
group2 <- rnorm(mean = 0, sd = 0.2, n = 12)
group3 <- rnorm(mean = 0, sd = 0.2, n = 8)
group4 <- rnorm(mean = 1, sd = 0.2, n = 10)
group5 <- rnorm(mean = 10, sd = 1, n = 2)

toydata <- data.frame(
  observation1 = c(group1, group2),
  observation2 = c(group3, group4, group5)
  ) %>% 
  mutate(feature = 1:20) %>% 
  pivot_longer(cols = !feature, names_to = "observation", values_to = "value") %>% 
  mutate(observation2 = str_remove(observation, "observation"))

p1 <- toydata %>% 
  mutate(observation2 = fct_rev(observation2)) %>% 
  ggplot(aes(x = observation2, y = feature)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colors = viridis(n = 10)) +
  labs(x = "Observations",
       y = "Features",
       caption = "Solos dos valores diferentes!\n",
       title = "Sin checar outliers") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
   # legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  coord_flip()


p2 <- toydata %>% 
  mutate(observation2 = fct_rev(observation2)) %>%
  mutate(rank = rank(value, ties.method = "first")) %>% 
  ggplot(aes(x = value, y = rank)) +
  geom_point(shape = 21, color = "grey20", 
             aes(fill = value), size = 3) +
  scale_fill_gradientn(colors = viridis(n = 10)) +
  labs(caption = "Esperen...\n",
       title = "Checaste outliers?") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "none",
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(size = 12, face = "bold")
  )


p3 <- toydata %>% 
  mutate(observation2 = fct_rev(observation2)) %>%
  mutate(rank = rank(value, ties.method = "first")) %>% 
  mutate(value2 = case_when(
    value >= 2 ~ 2,
    T ~ value
  )) %>% 
  ggplot(aes(x = value, y = rank)) +
  geom_point(shape = 21, color = "grey20", 
             aes(fill = value2), size = 3) +
  scale_fill_gradientn(colors = viridis(n = 10),
                       breaks = c(0, 1, 2),
                       labels = c("0", "1", ">2")
                       ) +
  labs(fill = "value",
       caption = "Mejor...",
       title = "Valores más bajos se visualizan mejor") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "none",
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(size = 12, face = "bold")
  )

p4 <- toydata %>% 
  mutate(observation2 = fct_rev(observation2)) %>% 
  mutate(value2 = case_when(
    value >= 1.5 ~ 2,
    T ~ value
  )) %>% 
  ggplot(aes(x = observation2, y = feature)) +
  geom_tile(aes(fill = value2)) +
  scale_fill_gradientn(colors = viridis(n = 10),
                       breaks = c(0, 1, 2),
                       labels = c("0", "1", ">2")
                       ) +
  labs(x = "Observations",
       y = "Features",
       fill = "value",
       caption = "Dos obervaciones muy diferentes!",
       title = "Colores modificados") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    #legend.position = "bottom",
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  coord_flip()


wrap_plots(p1, p2, p4, p3, 
           nrow = 2, ncol = 2, 
           widths = c(1, 0.7))

group1 <- data.frame(
  "Type" = c("Type I", "Type II", "Type III", "Type IV"),"Percentage" = c(15, 35, 30, 20))

group2 <- data.frame(
  "Type" = c("Type I", "Type II", "Type III", "Type IV"),"Percentage" = c(10, 25, 35, 30))

head(group2)

# g1_pie <- group1 %>%
#   ggplot(aes(x = "", y = Percentage)) +
#   geom_bar(stat = "identity", width = 0.7,
#            color = "white",
#            aes(fill = Type)) +
#   annotate(geom = "text", x = -Inf, y = Inf,
#            label = "Group 1", size = 4, hjust = 0.5, vjust = 0.5) +
#   labs(fill  = NULL,
#        title = "Pie charts:\nArc lengths & area\nrepresent data") +
#   scale_fill_manual(values = brewer.pal(8, "Set2")) +
#   coord_polar("y", start = 0, direction = -1) +
#   theme_void() +
#   theme(
#     text = element_text(size = 14),
#     plot.title = element_text(size = 12),
#     legend.position = "none"
#   )
# 
# g2_pie <- group2 %>%
#   ggplot(aes(x = "", y = Percentage)) +
#   geom_bar(stat = "identity", width = 0.7,
#            color = "white",
#            aes(fill = Type)) +
#   annotate(geom = "text", x = -Inf, y = Inf,
#            label = "Group 2", size = 4, hjust = 0.5, vjust = 0.5) +
#   scale_fill_manual(values = brewer.pal(8, "Set2")) +
#   labs(fill  = NULL,
#        caption = "Hard to compare groups.\nArc length redundant with area.") +
#   coord_polar("y", start = 0, direction = -1) +
#   theme_void() +
#   theme(
#     text = element_text(size = 14),
#     plot.caption = element_text(hjust = 0),
#     legend.position = "none"
#   )
# 
# pies <- wrap_plots(g1_pie, g2_pie, nrow = 2)
# pies

g1_pie <- group1 %>% 
  ggplot(aes(x = "", y = Percentage)) +
  geom_bar(stat = "identity", width = 0.7,
           color = "white",
           aes(fill = Type)) +
  annotate(geom = "text", x = -Inf, y = Inf, 
           label = "Group 1", size = 6, hjust = 0.5, vjust = 0.5) +
  labs(fill  = NULL,
       title = "") +
  scale_fill_manual(values = brewer.pal(8, "Set2")) +
  coord_polar("y", start = 0, direction = -1) +
  theme_void() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 12),
    legend.position = "none"
  )

g2_pie <- group2 %>% 
  ggplot(aes(x = "", y = Percentage)) +
  geom_bar(stat = "identity", width = 0.7, 
           color = "white",
           aes(fill = Type)) +
  annotate(geom = "text", x = -Inf, y = Inf, 
           label = "Group 2", size = 6, hjust = 0.5, vjust = 0.5) +
  scale_fill_manual(values = brewer.pal(8, "Set2")) +
  labs(fill  = NULL,
       caption = "") +
  coord_polar("y", start = 0, direction = -1) +
  theme_void() +
  theme(
    text = element_text(size = 14),
    plot.caption = element_text(hjust = 0),
    legend.position = "none"
  )

pies <- wrap_plots(g1_pie, g2_pie, nrow = 2)
pies

group1$ymax <- cumsum(group1$Percentage)
group1$ymin <- c(0, head(group1$ymax, n = (nrow(group1)-1)))
group2$ymax <- cumsum(group2$Percentage)
group2$ymin <- c(0, head(group2$ymax, n = (nrow(group1)-1)))

# g1_donut <- group1 %>%
#   ggplot(aes(x = 1:4, y = Percentage)) +
#   geom_rect(aes(ymin = ymin, ymax = ymax, fill = Type),
#             xmax = 4, xmin = 3, color = "white") +
#   annotate(geom = "text", x = 0, y = Inf,
#            label = "Group 1", size = 4, hjust = 0.5, vjust = 0.5) +
#   labs(fill  = NULL,
#        title = "") +
#   scale_fill_manual(values = brewer.pal(8, "Set2")) +
#   coord_polar(theta = "y", start = 0, direction = 1) +
#   theme_void() +
#   theme(
#     text = element_text(size = 14),
#     plot.title = element_text(size = 12),
#     legend.position = "none"
#   )
# 
# g2_donut <- group2 %>%
#   ggplot(aes(x = 1:4, y = Percentage)) +
#   geom_rect(aes(ymin = ymin, ymax = ymax, fill = Type),
#             xmax = 4, xmin = 3, color = "white") +
#   annotate(geom = "text", x = 0, y = Inf,
#            label = "Group 2", size = 4, hjust = 0.5, vjust = 0.5) +
#   labs(fill  = NULL,
#        caption = "")  +
#   scale_fill_manual(values = brewer.pal(8, "Set2")) +
#   coord_polar(theta = "y", start = 0, direction = 1) +
#   theme_void() +
#   theme(
#     text = element_text(size = 14),
#     plot.caption = element_text(hjust = 0),
#     legend.position = "none"
#   )
# 
# donuts <- wrap_plots(g1_donut, g2_donut, nrow = 2)
# donuts

g1_donut <- group1 %>% 
  ggplot(aes(x = 1:4, y = Percentage)) +
  geom_rect(aes(ymin = ymin, ymax = ymax, fill = Type), 
            xmax = 4, xmin = 3, color = "white") +
  annotate(geom = "text", x = 0, y = Inf, 
           label = "Group 1", size = 7, hjust = 0.5, vjust = 0.5) +
  labs(fill  = NULL,
       title = "") +
  scale_fill_manual(values = brewer.pal(8, "Set2")) +
  coord_polar(theta = "y", start = 0, direction = 1) +
  theme_void() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 12),
    legend.position = "none"
  )

g2_donut <- group2 %>% 
  ggplot(aes(x = 1:4, y = Percentage)) +
  geom_rect(aes(ymin = ymin, ymax = ymax, fill = Type), 
            xmax = 4, xmin = 3, color = "white") +
  annotate(geom = "text", x = 0, y = Inf, 
           label = "Group 2", size = 7, hjust = 0.5, vjust = 0.5) +
  labs(fill  = NULL,
       caption = "")  +
  scale_fill_manual(values = brewer.pal(8, "Set2")) +
  coord_polar(theta = "y", start = 0, direction = 1) +
  theme_void() +
  theme(
    text = element_text(size = 14),
    plot.caption = element_text(hjust = 0),
    legend.position = "none"
  )

donuts <- wrap_plots(g1_donut, g2_donut, nrow = 2)
donuts

TwoGroups <- rbind(
  group1 %>% 
    mutate(group = "1"),
  group2 %>% 
    mutate(group = "2")
)

# StackedBar <- TwoGroups %>%
#   ggplot(aes(x = group, y = Percentage)) +
#   geom_bar(stat = "identity", aes(fill = Type),
#            width = 0.5, color = "white") +
#   labs(fill  = NULL,
#        x = "Groups",
#        title = "Stacked Bars:\nJust unwrap\nthe polar coordinate!",
#        caption = "Easier to compare groups.\nData represented by bar heights.") +
#   scale_fill_manual(values = brewer.pal(8, "Set2")) +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14),
#     axis.text = element_text(color = "black"),
#     plot.title = element_text(size = 12),
#     plot.caption = element_text(hjust = 0)
#   )
# 
# StackedBar

StackedBar <- TwoGroups %>% 
  ggplot(aes(x = group, y = Percentage)) +
  geom_bar(stat = "identity", aes(fill = Type),
           width = 0.5, color = "white") +
  labs(fill  = NULL,
       x = "Groups",
       title = "",
       caption = "") +
  scale_fill_manual(values = brewer.pal(8, "Set2")) +
  theme_classic() +
  theme(
    text = element_text(size = 20),
    axis.text = element_text(color = "black"),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0)
  )

StackedBar

set.seed(666)

generate_percentages <- function(N){
  random_N <- rlnorm(n = N, sdlog = 3)
  (random_N / sum(random_N)) * 100
} 

my_data <- replicate(100, generate_percentages(8)) %>% 
  as.data.frame() %>%
  cbind(class = letters[1:8]) %>% 
  pivot_longer(cols = !class, names_to = "sample", values_to = "percentage")


no_reorder <- my_data %>% 
  ggplot(aes(x = sample, y = percentage)) +
  geom_bar(stat = "identity", aes(fill = class)) +
  scale_fill_manual(values = brewer.pal(8, "Set2")) +
  theme_classic() +
  theme(axis.text.x = element_blank())


sample_grouping <- my_data %>% 
  group_by(sample) %>% 
  slice_max(order_by = percentage) %>% 
  select(class, sample) %>% 
  rename(peak_class = class)


my_data_reordered <- my_data %>% 
  inner_join(sample_grouping, by = "sample") %>% 
  group_by(peak_class) %>% 
  mutate(rank = rank(percentage)) %>%  
  mutate(sample = reorder(sample, -rank)) %>%  
  ungroup()
  

bars_reordered <- my_data_reordered %>% 
  ggplot(aes(x = sample, y = percentage)) +
  geom_bar(stat = "identity", aes(fill = class)) +
  scale_fill_manual(values = brewer.pal(8, "Set2")) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(0.1, "line")) 

wrap_plots(
  no_reorder +
    labs(title = "Sin reordenar"),
  bars_reordered +
    labs(title = "Barras ordenadas"), 
  guides = "collect",
  nrow = 2
) &
  labs(y = "relative abundance (%)") &
  theme(title = element_text(size = 10))



set.seed(666)


data1 <- data.frame(
  response = rnorm(n = 100, mean = 5, sd = 2)
) %>% 
  mutate(group = "group1")

data2 <- data.frame(
  response = c(
    rnorm(n = 50, mean = 2.5, sd = 1),
    rnorm(n = 50, mean = 7.5, sd = 1)
  )) %>% 
    mutate(group = "group2")

data3 <- data.frame(
  response = c(
    rnorm(n = 33, mean = 2, sd = 0.5),
    rnorm(n = 33, mean = 5, sd = 0.5),
    rnorm(n = 33, mean = 8, sd = 0.5)
  )) %>% 
    mutate(group = "group3")

Box <- rbind(
  data1, 
  data2,
  data3
) %>% 
  ggplot(aes(x = group, y = response)) +
  geom_boxplot(aes(fill = group), alpha = 0.8, width = 0.7) +
  scale_fill_manual(values = brewer.pal(8, "Set2")) +
  labs(title = "Very similar!") + 
  theme_classic()


Dots <- rbind(
  data1, 
  data2,
  data3
) %>% 
  ggplot(aes(x = group, y = response)) +
  geom_quasirandom(aes(color = group), alpha = 0.8) +
  scale_color_manual(values = brewer.pal(8, "Set2")) +
  labs(title = "I guess not!") + 
  theme_classic()


wrap_plots(
  Box, Dots,
  nrow = 1
) &
  theme(legend.position = "none")


set.seed(666)

M3 <- data.frame(
  conc = rnorm(n = 8, mean = 0.03, sd = 0.01)
) %>% 
  mutate(group = "ctrl") %>% 
  rbind(
    data.frame(
      conc = rnorm(n = 6, mean = 0.25, sd = 0.02)
    ) %>% 
      mutate(group = "trt")
  ) %>% 
  mutate(metabolite = "q2")

M2 <- data.frame(
  conc = rnorm(n = 8, mean = 6, sd = 1)
) %>% 
  mutate(group = "ctrl") %>% 
  rbind(
    data.frame(
      conc = rnorm(n = 6, mean = 5.5, sd = 1.1)
    ) %>% 
      mutate(group = "trt")
  ) %>% 
  mutate(metabolite = "q1")

M1 <- data.frame(
  conc = rnorm(n = 8, mean = 20, sd = 0.5)
) %>% 
  mutate(group = "ctrl") %>% 
  rbind(
    data.frame(
      conc = rnorm(n = 6, mean = 19.5, sd = 1.2)
    ) %>% 
      mutate(group = "trt")
  ) %>% 
  mutate(metabolite = "q0")

toydata <- rbind(
  M1, M2, M3
)

head(toydata)

# same_scale <- toydata %>%
#   ggplot(aes(x = group, y = conc)) +
#   facet_grid(. ~ metabolite) +
#   geom_bar(stat = "summary", fill = NA, aes(color = group),
#            alpha = 0.8, fun = mean, width = 0.5, size = 0.8) +
#   ggbeeswarm::geom_quasirandom(
#     shape = 21, size = 2.5, alpha = 0.8, color = "white",
#     aes(fill = group), width = 0.2
#   ) +
#   scale_fill_manual(values = c("grey20", "tomato1")) +
#   scale_color_manual(values = c("grey20", "tomato1")) +
#   labs(y = "# efectivo de especies",
#        x = "Group",
#        title = "Basic layout",
#        caption = "No hay diferencias con el control!\n") +
#   theme_classic() +
#   theme(
#     legend.position = "none",
#     text = element_text(size = 16, color = "black"),
#     axis.text = element_text(color = "black"),
#     panel.spacing = unit(1, "lines"),
#     title = element_text(size = 16, face = "bold"),
#     plot.caption= element_text(size = 16, hjust = 0)
#   )
# 
# same_scale

same_scale <- toydata %>% 
  ggplot(aes(x = group, y = conc)) +
  facet_grid(. ~ metabolite) +
  geom_bar(stat = "summary", fill = NA, aes(color = group), 
           alpha = 0.8, fun = mean, width = 0.5, size = 0.8) +
  ggbeeswarm::geom_quasirandom(
    shape = 21, size = 2.5, alpha = 0.8, color = "white",
    aes(fill = group), width = 0.2
  ) +
  scale_fill_manual(values = c("grey20", "tomato1")) +
  scale_color_manual(values = c("grey20", "tomato1")) +
  labs(y = "# efectivo de especies",
       x = "Group",
       title = "",
       caption = "No hay diferencias con el control!\n") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 20, color = "black"),
    axis.text = element_text(color = "black"),
    panel.spacing = unit(1, "lines"),
    title = element_text(size = 18, face = "bold"),
    plot.caption= element_text(size = 18, hjust = 0)
  )+ggpubr::stat_compare_means(method = "t.test")

same_scale

# free_scale <- toydata %>%
#   ggplot(aes(x = group, y = conc)) +
#   facet_wrap(. ~ metabolite, scales = "free_y") +
#   geom_bar(stat = "summary", fill = NA, aes(color = group),
#            alpha = 0.8, fun = mean, width = 0.5, size = 0.8) +
#   ggbeeswarm::geom_quasirandom(
#     shape = 21, size = 2.5, alpha = 0.8, color = "white",
#     aes(fill = group), width = 0.2
#   ) +
#   scale_fill_manual(values = c("grey20", "tomato1")) +
#   scale_color_manual(values = c("grey20", "tomato1")) +
#   labs(y = "# efectivo de especies",
#        x = "Group",
#        title = "",
#        caption = "OH!!!") +
#   theme_classic() +
#   theme(
#     legend.position = "none",
#     text = element_text(size = 20, color = "black"),
#     axis.text = element_text(color = "black"),
#     panel.spacing = unit(1, "lines"),
#     title = element_text(size = 20, face = "bold"),
#     plot.caption= element_text(size = 20, hjust = 0)
#   ) +ggpubr::stat_compare_means(method = "t.test")
# 
# free_scale

free_scale <- toydata %>% 
  ggplot(aes(x = group, y = conc)) +
  facet_wrap(. ~ metabolite, scales = "free_y") +
  geom_bar(stat = "summary", fill = NA, aes(color = group), 
           alpha = 0.8, fun = mean, width = 0.5, size = 0.8) +
  ggbeeswarm::geom_quasirandom(
    shape = 21, size = 2.5, alpha = 0.8, color = "white",
    aes(fill = group), width = 0.2
  ) +
  scale_fill_manual(values = c("grey20", "tomato1")) +
  scale_color_manual(values = c("grey20", "tomato1")) +
  labs(y = "# efectivo de especies",
       x = "Group",
       title = "",
       caption = "OH!!!") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 20, color = "black"),
    axis.text = element_text(color = "black"),
    panel.spacing = unit(1, "lines"),
    title = element_text(size = 20, face = "bold"),
    plot.caption= element_text(size = 20, hjust = 0)
  ) +ggpubr::stat_compare_means(method = "t.test")

free_scale

set.seed(666)
Example_data1 <- rbind(
  rnorm(3, mean = 5, sd = 0.5) %>%
    as.data.frame() %>% 
    mutate(time = 1),
  rnorm(3, mean = 8, sd = 0.5) %>% 
    as.data.frame() %>% 
    mutate(time = 2),
  rnorm(3, mean = 10, sd = 0.5) %>%
    as.data.frame() %>% 
    mutate(time = 3)
) %>% 
  rename(response = ".")

Dotline <- Example_data1 %>% 
  ggplot(aes(x = time, y = response)) +
  stat_summary(geom = "line", fun.data = mean_se, 
               size = 1.1, alpha = 0.8, color = "grey60") +
  geom_point(aes(fill = as.factor(time)), shape = 21, color = "grey20",
             alpha = 0.8, size = 3, position = position_jitter(0.1, seed = 666)) +
  stat_summary(geom = "errorbar", fun.data = mean_se,
               width = 0.05, alpha = 0.8) +
  scale_fill_manual(values = brewer.pal(8, "Accent")) +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  labs(x = "Time point",
       y = "Response",
       title = "Los Dot/line plots se\n basan en posición",
       caption = "\nLos valores de posición están entre\n el eje x & y") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 14, color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0)
  )



GoodBar <- Example_data1 %>% 
  ggplot(aes(x = time, y = response)) +
  stat_summary(geom = "bar", fun.data = mean_se, aes(color = as.factor(time)),
               size = 1.1, alpha = 0.8, fill = NA, width = 0.5) +
  geom_point(aes(fill = as.factor(time)), shape = 21, color = "grey20",
             alpha = 0.8, size = 3, position = position_jitter(0.1, seed = 666)) +
  stat_summary(geom = "errorbar", fun.data = mean_se,
               width = 0.05, alpha = 0.8) +
  scale_fill_manual(values = brewer.pal(8, "Accent")) +
  scale_color_manual(values = brewer.pal(8, "Accent")) +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  labs(x = "Time point",
       y = "\nResponse",
       title = "Los Barplots están\nbasadas en longitud",
       caption = "\nLos valores se representan con\nla distancia del eje x") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 14, color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0)
  )


Example_data1_s <- Example_data1 %>% 
  group_by(time) %>% 
  summarise(response = mean(response))



BadBar <- Example_data1 %>% 
  ggplot(aes(x = time, y = response)) +
  geom_rect(data = Example_data1_s,
            aes(ymax = response, xmin = time-0.25, xmax = time+0.25,
                color = as.factor(time)), 
            ymin = 4.5, fill = NA, alpha = 0.8, size = 1.1) +
  geom_point(aes(fill = as.factor(time)), shape = 21, color = "grey20",
             alpha = 0.8, size = 3, position = position_jitter(0.1, seed = 666)) +
  stat_summary(geom = "errorbar", fun.data = mean_se,
               width = 0.05, alpha = 0.8) +
  scale_fill_manual(values = brewer.pal(8, "Accent")) +
  scale_color_manual(values = brewer.pal(8, "Accent")) +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  ylim(c(4.25, 11)) + 
  labs(x = "Time point",
       y = "\nResponse",
       title = "NUNCA HAGAS ESTO!",
       caption = "\nLa longitud de las barras\nse pueden malinterpretar") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 14, color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0)
  )


plot_grid(Dotline, GoodBar, BadBar, 
          align = "h", axis = "lrtb", nrow = 1)
