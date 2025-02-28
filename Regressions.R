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

# number of articles per year
data_articles <- data$Year |>
  table() |> 
  as.data.frame()

# Regression for number of articles published by year
# Non-controlled regressions
ggplot(data_articles, aes(x = as.numeric(levels(Var1))[Var1], y = Freq)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", colour = "forestgreen", fill = "lightgreen", formula = "y ~ x") +
  geom_smooth(method = lm, colour="darkblue", fill="lightblue") +
  scale_x_continuous(breaks = seq(1975, 2020, 5), labels = seq(1975, 2020, 5)) +
  theme_article(base_size = 18) +
  labs (title = "Non-controlled regression", x= NULL, y= "Articles published")

# Number of articles related to biological sciences indexed in Web of Science, in each year (from 1974 to 2021, **including only the years where articles elected for this review were published**)
correctionWOS <- c(1313, 2428, 4640, 5479, 5427, 5583, 5760, 6067, 7731, 21240, 21723, 23767, 25595, 27104, 29623, 33014, 31921, 31906, 35001, 35242, 37878, 38332, 41176, 47025, 52926, 56692, 62755, 63681, 70639, 71357, 76191, 79482, 85068, 93098, 99055, 121026, 140964, 163793, 178630)

# Number of artibles related to biological sciences indexed in Scopus, in each year (from 1974 to 2021, **including only the years where articles elected for this review were published**)
correctionSCO <- c(18795, 23260, 29303, 35192, 35086, 36097, 38307, 41139, 44738, 46279, 48478, 52297, 55100, 74972, 78886, 79767, 80854, 83581, 85594, 90711, 94879, 99441, 110686, 122382, 134657, 144867, 153702, 162468, 179915, 189544, 201072, 205702, 214796, 214526, 222004, 231622, 239857, 263837, 284272)

correction.full <- correctionSCO + correctionWOS # There are probably some duplicates, but the number of articles indexed in Scopus is so large that I could just subtract the number of articles encountered in Web of Science, it would not change the regression plot.

# data used to regression of articles published along the years
reg_data_articles <- data_articles  |>  
  mutate(N = Freq/correction.full)

# function used to turn all values into scientific notation
scientific_10 <- function(x) {
  parse(text = gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# Linear regression
summary(lm(reg_data_articles$N ~ as.numeric(as.character(reg_data_articles$Var1))))

li.reg <- ggplot(reg_data_articles, aes(x= as.numeric(levels(Var1))[Var1], y = N)) +
  geom_point(size= 4) +
  geom_smooth(method = lm, colour="skyblue", fill="lightblue") +
  scale_y_continuous(labels = scientific_10, breaks = c(0, 2*10^-5, 3*10^-5, 4*10^-5, 5*10^-5, 6*10^-5, 7*10^-5)) +
  scale_x_continuous(breaks = seq(1975, 2020, 5), labels = seq(1975, 2020, 5)) +
  theme_classic(base_size = 15) +
  labs (title = "a)", 
        x= NULL, 
        y= "Number of articles controlled by\nthe total number of articles in WoS and Scopus\n") +
  theme(text = element_text(color = "black"), legend.title = element_blank()) +
  annotate("text", x = 1977, y = 6.5e-05, label = "    R² < 0.001\np = 0.89", size = 6)

# Loess regression
lo.reg <- ggplot(reg_data_articles, aes(x = as.numeric(levels(Var1))[Var1], y = N)) +
  geom_point(size= 4) +
  geom_smooth(method = "loess", colour = "forestgreen", fill = "lightgreen", formula = "y ~ x") +
  scale_x_continuous(breaks = seq(1975, 2020, 5), labels = seq(1975, 2020, 5)) +
  theme_classic(base_size = 15) +
  labs (title = "b)", 
        x= NULL, 
        y= NULL) +
  theme(text = element_text(color = "black"), legend.title = element_blank(), axis.line.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

li.reg + lo.reg + plot_annotation(
  title = "Publication trend over time:", 
  theme = theme(plot.title = element_text(size = 15))
)
