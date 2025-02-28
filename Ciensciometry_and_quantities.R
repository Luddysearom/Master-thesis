packages <- c("openxlsx", "tidyverse", "egg", "patchwork", "scales")

install_and_load_packages <- function(packages) {
  for(package in packages) {
    if(!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

install_and_load_packages(packages)

data <- read.xlsx("All_data.xlsx")

# Quantities ----------------------------------------------------------------------------------------------------------
lapply(data[c(5:7, 12:14, 17:19, 23:24, 29:34, 36:39, 41:42)], \(x) table(x)) # number (n) of observations in each category

length(table(data$ID))      # n of articles

table(data$source)
length(table(data$source))  # n of sources

length(table(data$hsp))     # n of host species
length(table(data$psp))     # n of parasite species

data |> 
  count(psp) |> 
  filter(n == 1) |>  
  summarise(total = sum(n)) # n of parasite species occurring only once

data |> 
  count(hsp) |> 
  filter(n == 1) |>  
  summarise(total = sum(n)) # n of host species occurring only once

nrow(data)                  # n of registers

table(data$meltdown)

View(table(data$horder))

length(table(data$pfamily)) # n of parasite families
length(table(data$porder))  # n of parasite orders
length(table(data$pphy))    # n of parasite phyla

length(table(data$horder))  # n of host orders
length(table(data$hfamily)) # n of host families

table(data$lab)             # n of studies conducted in laboratory

# Invasions per year --------------------------------------------------------------------------------------------------
# Registers per year
data_registers <- data |> 
  group_by(ID, Year) |> 
  summarise(n = n(), .groups = "drop") |> 
  fill(Year, .direction = "downup") |> 
  group_by(Year) |> 
  summarise(registers = sum(n), .groups = "drop")

ggplot(data_registers, aes(x = Year, y = registers)) +
  geom_col(position = "stack", color = "black") +
  labs(
    title = "Number of registers per year",
    x = NULL,
    y = "Frequency",
    fill = NULL
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 71)) +
  theme_bw(base_size = 18)

# Articles published per year
data_articles <- data$Year |>
  table() |> 
  as.data.frame()

data_articles$Var1 <- as.numeric(as.character(data_articles$Var1))

ggplot(data_articles, aes(x = Var1, y = Freq)) +
  geom_col(position = "stack", color = "black") +
  labs(
    title = "Number of articles published per year",
    x = NULL,
    y = "Frequency",
    fill = NULL
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2), expand = c(0,0), limits = c(0, 20)) +
  scale_x_continuous(breaks = seq(from = 1975, to = 2020, by =5)) +
  theme_bw(base_size = 18)
