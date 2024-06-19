pacman::p_load(officer,readxl,ggplot2,dplyr,tidyr,tidyverse,gt)

# Load the data
data <- read_excel(here::here("data", "19-03-2024 data.xlsx"))


# Plotting gene expression by concentration for each treatment
ggplot(data, aes(x = conc, y = gene_expression, color = treatment)) +
  geom_point() +
  geom_line(aes(group = interaction(treatment, conc))) +
  labs(title = "Gene Expression by Concentration and Treatment", x = "Concentration", y = "Gene Expression")



# ANOVA to test for differences in gene expression across treatments and concentrations
fit <- aov(gene_expression ~ treatment * conc, data = data)
anova_summary <- summary(fit)
anova_summary

anova_df <- as.data.frame(anova_summary[[1]])  # Adjust this indexing based on your data

# Create GT table
gt_table <- gt(anova_df)
file_path <- here::here("table", "anova_table.pdf")
gtsave(gt_table, filename = file_path)

agg_data <- data %>%
  group_by(conc, treatment) %>%
  summarise(mean_gene_expression = mean(gene_expression), .groups = 'drop')

# Plotting
ggplot(agg_data, aes(x = conc, y = mean_gene_expression, color = treatment)) +
  geom_line() +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Mean Gene Expression by Concentration and Treatment",
       x = "Concentration",
       y = "Mean Gene Expression")


ggplot(data, aes(x = conc, y = gene_expression, color = treatment)) +
  geom_point() +
  geom_line(aes(group = treatment)) +
  facet_wrap(~cell_line) +
  labs(x = "Concentration of Growth Factor",
       y = "Gene Expression",
       color = "Treatment") +
  theme_minimal()



com_data <- data %>%
  group_by(conc) %>%
  summarise(mean_gene_expression = mean(gene_expression), .groups = 'drop')

ggplot(com_data, aes(x=conc,y = mean_gene_expression))+
  geom_line()+
  geom_point()+
  labs(x = "Concentration", y = "Mean Gene Expression")


