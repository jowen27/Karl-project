pacman::p_load(readxl,dplyr,openxlsx)

data <- read_excel(here::here("raw-data","WIF-tis4d.xlsx"))
data$treatment <- tolower(data$treatment)
data$cell_line <- tolower(data$cell_line)


data$name <- gsub("^GL-", "", data$name)
data$name <- gsub("^Gl-", "", data$name)
data$name <- gsub("^Cwn", "cwN", data$name)
data$name <- gsub("^Xib", "XIb", data$name)
data$name <- gsub("^Rjs", "rjS", data$name)
data$name <- gsub("^Zhw", "ZHw", data$name)
data$treatment <- gsub("^activating", "Activating", data$treatment)
data$treatment <- gsub("^placebo", "Placebo", data$treatment)


# Convert factor variables if necessary
data$cell_line <- as.factor(data$cell_line)
data$treatment <- as.factor(data$treatment)


path <- "data/19/03/2024"
# File name
file_name <- "2024 data.xlsx"

# Full path to the file
full_path <- file.path(path, file_name)

write.xlsx(data, file = "19-03-2024 data.xlsx")
