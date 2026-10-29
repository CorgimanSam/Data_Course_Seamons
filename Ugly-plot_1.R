library(ggplot2)
#Bringing in the data
fig_four_c <- read.csv("13.Fig 4C_pgh2_Lung.csv")
fig_four_d <- read.csv("14.Fig 4D_PGE2_Lung.csv")
#Organizing the data
Four_d_data <- fig_four_d$PGE2..pg.10.mg.tissue.
Four_c_data <- fig_four_c$PGH2..pg.10.mg.tissue.
plot_data <- cbind(Four_c_data,Four_d_data)
# Creating the Ugly-plot
ggplot(data = plot_data, aes(x=Four_c_data, y=Four_d_data)) +
geom_point(colour = "lightgreen",size = 7)+ 
  theme(plot.background = element_rect("navy")) + 
  theme(panel.background = element_rect(fill = "tomato"))+
  labs(title = "Uglyplot", x="Four_C_Data",y="Four_D_Data") + geom_line()