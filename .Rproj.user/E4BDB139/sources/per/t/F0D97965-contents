pacman::p_load(showtext,ggplot2,readxl,grid,tidyverse,gtable,ggrepel,ggpubr,tiff)# for setting the font family globally
data <- read_excel(here::here("data", "19-03-2024 data.xlsx"))


# Set the font family to Times New Roman
font_add(family = "Times New Roman", regular = "Times New Roman.ttf")
showtext_auto(enable = TRUE)

library(ggplot2)
library(ggrepel) # repel geom_label
pacman::p_load(ggpubr)

wild_data <- filter(data, cell_line == "wild-type")
cell_type <- filter(data, cell_line == "cell-type 101")
cell_type$name[cell_type$conc != 10] <- ""
wild_data$name[wild_data$conc != 10] <- ""


theme_set(theme_bw(base_family = "Times New Roman"))


wild_plot <- ggplot(wild_data,aes(x = conc,y = gene_expression,fill = treatment, label = name)) +
  geom_point(shape = 21, stroke = 0.8, size = 3, color = "black", alpha = 0.7) + # black border
  geom_label_repel(nudge_x = 0.6) + # add series label with line
  labs(title = "Wild-type", tag = "A", x = "ug/ml",y = "Gene Expresion") + #labels
  scale_fill_manual(values = c("steelblue3","tan"))  +  # colour for fill scale
  scale_x_continuous(breaks = 0:10) +
  scale_discrete_identity(aesthetics = "label") + # remove label from legend
  theme_bw() + # this is the base theme they used
  theme(legend.position = "none",text = element_text(family = "serif"),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 10),
        plot.tag = element_text(size = 14))

cell_plot <- ggplot(cell_type,aes(x = conc,
                                  y = gene_expression,
                                  fill = treatment, label = name)) +
  geom_point(shape = 21, stroke = 0.8, size = 3, color = "black", alpha = 0.7) + # black border
  geom_label_repel(nudge_x = 0.6) + # add series label with line
  labs(title = "Cell-type 101", tag = "B", x = "ug/ml",y = "Gene Expresion") + #labels
  scale_fill_manual(values = c("steelblue3","tan"))  +  # colour for fill scale
  scale_x_continuous(breaks = 0:10) +
  scale_discrete_identity(aesthetics = "label") + # remove label from legend
  theme_bw() + # this is the base theme they used
  theme(legend.position = "none",text = element_text(family = "serif"),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 10),
        plot.tag = element_text(size = 14))


# had to do an extra plot to extract the legend it seems that ggrepel fucks up with it somehow

fix <- ggplot(data = data,aes(x = conc,y = gene_expression, fill = treatment)) +
  geom_point(shape = 21, stroke = 0.8, size = 3, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("steelblue3","tan"),
                    labels = c("Activating factor 42", "Placebo")) + # factor labels for legend
  scale_x_continuous(breaks = 0:10) +
  theme_bw() +
  theme(text = element_text(family = "serif"), legend.position = "bottom") +
  guides(fill = guide_legend(title = "Treatment")) # legend title




combined_plot <- ggarrange(wild_plot,cell_plot, legend = "bottom", legend.grob = get_legend(fix))
combined_plot

tiff(filename = here::here("figs", "gene_plot.tiff"),
     width = 9, height = 6, units = "in", res = 500)
grid.draw(combined_plot)
dev.off()
