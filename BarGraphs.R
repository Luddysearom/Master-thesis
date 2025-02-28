packages <- c("openxlsx", "tidyverse", "egg", "patchwork", "scales", "ggsci")

install_and_load_packages <- function(packages) {
  for(package in packages) {
    if(!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

install_and_load_packages(packages)

data <- read.xlsx("All_Data.xlsx")

# Factor inducing invasion by spillmode ---------------------------------------------------------------------------
data |> 
  group_by(Spillmode, factor.ind) |> 
  summarise(n = n()) |> 
  mutate(percent = n / sum(n)*100)

fct.ind_records <- data |>
  ggplot(aes(x = fct_infreq(factor.ind), fill = Spillmode)) +
  geom_bar(position = "dodge", width = 0.8, color = "black") +
  scale_fill_jco(guide = guide_legend(nrow = 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 320), breaks = seq(0, 300, 50)) +
  scale_x_discrete(labels = c("Pisciculture", "Not mentioned\n or unknown", "Aquarism", "Biological\n control", "Ballast water", "Fisherman", "Special case", "Experiments")) +
  labs(title = NULL,
       x = "",
       y = "") +
  theme_bw(base_size = 15) +
  theme(
    legend.title = element_blank(), 
    legend.position = "inside",
    legend.position.inside = c(0.50, 0.96),
    legend.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  ) +
  annotate(geom = "text", label = "A)", x = 8.3, y = 310)
fct.ind_records

fct.ind_species <- data |> 
  group_by(factor.ind, Spillmode, psp) |> 
  summarise(
    n = n(),
    .groups = "drop"
  ) |> 
  mutate(
    factor.ind = fct_relevel(factor.ind, "Pisciculture", "Not mentioned or unknown", "Aquarism", "Biological control", "Ballast water", "Fisherman", "Special case", "Experiment")
  ) |> 
  group_by(factor.ind, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |>
  ggplot(aes(x = factor.ind, y = n, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, color = "black") +
  scale_fill_jco() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks = seq(0, 80, 10)) +
  scale_x_discrete(labels = c("Pisciculture", "Not mentioned\n or unknown", "Aquarism", "Biological\n control", "Ballast water", "Fisherman", "Special case", "Experiments")) +
  labs(title = NULL,
       x = "",
       y = "") +
  theme_bw(base_size = 15) +
  theme(
    legend.title = element_blank(), 
    legend.position = "none", 
    legend.background = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.6),
  ) +
  annotate(geom = "text", label = "B)", x = 8.3, y = 77)
fct.ind_species

fct.ind_records / fct.ind_species + plot_annotation(title = "Factor inducing parasite invasions for amount of records (A) and number of species (B)")

# Factor inducing invasion by continent ---------------------------------------------------------------------------
fct.ind_continent_records <- data |> 
  filter(
    continent != "Not applicable", # all "factor.ind == 'Experiment'" are removed here
    factor.ind != "Special case"   # removed because there are only 11 reports, 10 from Europe
  ) |>
  ggplot(aes(x = fct_infreq(continent), fill = fct_infreq(factor.ind))) +
  geom_bar(position = "dodge", color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 225), breaks = seq(0, 250, 50)) +
  scale_fill_frontiers(guide = guide_legend(nrow = 2)) +
  theme_bw(base_size = 15) +
  labs(title = NULL, x = "", y = "") +
  theme(
    legend.title = element_blank(), 
    legend.position = "inside", 
    legend.position.inside = c(0.50, 0.90),
    legend.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  ) +
  annotate(geom = "text", label = "A)", x = 6.3, y = 207)
fct.ind_continent_records

fct.ind_continent_species <- data |> 
  filter(
    continent != "Not applicable",
    factor.ind != "Special case"
  ) |> 
  group_by(factor.ind, continent, psp) |> 
  summarise(
    n = n(),
    .groups = "drop"
  ) |> 
  mutate(
    continent = fct_relevel(continent, "Europe", "North America", "South America", "Oceania", "Asia", "Africa"),
    factor.ind = fct_relevel(factor.ind, "Pisciculture", "Not mentioned or unknown", "Aquarism", "Biological control", "Ballast water", "Fisherman")
  ) |> 
  group_by(factor.ind, continent) |> 
  summarise(n = n(), .groups = "drop") |>
  ggplot(aes(x = continent, y = n, fill = factor.ind)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks = seq(0, 80, 10)) +
  scale_fill_frontiers() +
  theme_bw(base_size = 15) +
  labs(title = NULL, x = "", y = "") +
  theme(
    legend.title = element_blank(), 
    legend.position = "none", 
    legend.background = element_blank()
  ) +
  annotate(geom = "text", label = "B)", x = 6.3, y = 77)
fct.ind_continent_species

fct.ind_continent_records / fct.ind_continent_species + plot_annotation(title = "Factor inducing parasite invasions in each continent, for amount of records (A) and number of species (B)")

# Spillmode by Continent ------------------------------------------------------------------------------------------
spill.continent_records <- data |> 
  filter(
    continent != "Not applicable", # all "factor.ind == 'Experiment'" are removed here
  ) |>
  ggplot(aes(x = fct_infreq(continent), fill = Spillmode)) +
  geom_bar(position = "dodge", color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 260), breaks = seq(0, 250, 50)) +
  scale_fill_jco(guide = guide_legend(nrow = 1)) +
  theme_bw(base_size = 15) +
  labs(title = NULL, x = "", y = "") +
  theme(
    legend.title = element_blank(), 
    legend.position = "inside",
    legend.position.inside = c(0.50, 0.93),
    legend.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  ) +
  annotate(geom = "text", label = "A)", x = 6.3, y = 243)
spill.continent_records

spill.continent_species <- data |> 
  filter(
    continent != "Not applicable"
  ) |>
  group_by(Spillmode, continent, psp) |> 
  summarise(
    n = n(),
    .groups = "drop"
  ) |> 
  mutate(
    continent = fct_relevel(continent, "Europe", "North America", "South America", "Oceania", "Asia", "Africa"),
    ) |> 
  group_by(Spillmode, continent) |> 
  summarise(n = n(), .groups = "drop")  |>
  ggplot(aes(x = fct_infreq(continent), y = n, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 80), breaks = seq(0, 80, 10)) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  labs(title = NULL, x = "", y = "") +
  theme(
    legend.title = element_blank(), 
    legend.position = "none", 
    legend.background = element_blank()
  ) +
  annotate(geom = "text", label = "B)", x = 6.3, y = 77)
spill.continent_species

spill.continent_records / spill.continent_species + plot_annotation(title = "Spillback and spillover by continent, for amount of records (A) and number of species (B)")

# Parasite specialist or generalist nature ----------------------------------------------------------------------
# With these graphs we were willing to discuss the functional trait of "specialist" or "generalist" found in the review, against the data collected in literature. 
data_specia <- data |> 
  mutate(
    NumberFamilies = case_when(
      N.host.families == 1 ~ factor("One"),
      N.host.families > 1 & N.host.families <= 5 ~ factor("More than one"),
      N.host.families > 5 ~ factor("More than five")
    ),
    NumberOrders = case_when(
      N.host.orders == 1 ~ factor("One"),
      N.host.orders > 1 & N.host.orders <= 5 ~ factor("More than one"),
      N.host.orders > 5 ~ factor("More than five")
    )
  )

data |> 
  count(psp) |> 
  filter(n == 1) |> 
  summarise(sum(n))         # Number of parasites occurring only once = 88

pstyle_plot <- data |> 
  mutate(
    NumberFamilies = case_when(
      N.host.families == 1 ~ factor("One"),
      N.host.families > 1 & N.host.families <= 5 ~ factor("More than one"),
      N.host.families > 5 ~ factor("More than five")
    ),
    NumberOrders = case_when(
      N.host.orders == 1 ~ factor("One"),
      N.host.orders > 1 & N.host.orders <= 5 ~ factor("More than one"),
      N.host.orders > 5 ~ factor("More than five")
    )
  ) |> 
  group_by(psp, pstyle, ambiguous, N.host.families, NumberFamilies, N.host.orders, NumberOrders) |> 
  count() |> 
  filter(n > 1) |> 
  ungroup()

## **Without** parasites registered only once - for amount of families infected

nfamilies <- pstyle_plot |> 
  ggplot(aes(x = pstyle, fill = NumberFamilies)) +
  geom_bar(stat = "count", position = "dodge", color= "black") +
  theme_bw(base_size = 15) +
  scale_fill_tron() +
  scale_y_continuous(
    expand = c(0, 0), 
    limits = c(0, 42),
    breaks = seq(0, 40, 10)
  ) +
  labs(
    y = NULL, 
    x = NULL, 
    title = "a) Number of host families",
    #subtitle = "*for parasites registered more than once",
    fill = NULL
  ) +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

## **Without** parasites registered only once - for amount of orders infected
norders <- pstyle_plot |> 
  ggplot(aes(x = pstyle, fill = NumberOrders)) +
  geom_bar(stat = "count", position = "dodge", color= "black") +
  theme_bw(base_size = 15) +
  scale_fill_tron() +
  scale_y_continuous(
    expand = c(0, 0), 
    limits = c(0, 42),
    breaks = seq(0, 40, 10)
  ) +
  labs(
    y = NULL, 
    x = NULL, 
    title = "b) Number of host orders",
    #subtitle = "parasites registered only once were removed (98 species remained)",
    fill = NULL
  ) +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "inside",
    legend.background = element_blank(),
    legend.position.inside = c(0.78,0.88)
  )

nfamilies + norders + plot_annotation(title = "Number of hosts infected by generalist and specialist parasites", subtitle = "for parasites registered more than once")

pstyle_plot |> 
  group_by(NumberOrders) |> 
  summarise(n = n())
# Fish order and parasite phyla proportion ------------------------------------------------------------------------
## parasite phyla
pphyla.registers <- data |> 
  group_by(pphy) |> 
  summarise(
    n = n(),
    .groups = "drop"
  ) |> 
  inner_join(
    data |> 
      group_by(Spillmode, pphy) |> 
      summarise(
        n = n(),
        .groups = "drop"
      ), 
    by = "pphy"
  ) |>
  mutate(
    proportion = n.x / sum(n.x)*100,
    rel_proportion = n.y / sum(n.y)*100
  ) |> 
  ggplot(
    aes(
      x = reorder(pphy, proportion), 
      y = rel_proportion, 
      fill = Spillmode,
    )
  ) +
  geom_bar(stat = "identity", position = "dodge", color= "black") +
  coord_flip() +
  theme_bw(base_size = 12) +
  scale_fill_jco() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 35.5), labels = label_percent(scale = 1), breaks = seq(0, 35, 5)) +
  labs(y = NULL, x = NULL, fill = NULL, title = "b) Amount of parasites recorded") +
  theme(legend.position = "none", 
        legend.position.inside = c(0.75, 0.15), 
        legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0.3), "cm"),
        axis.text.x = element_blank(),    
        axis.ticks.x = element_blank())

pphyla.species <- data |> 
  group_by(pphy, psp) |> 
  summarise(
    n = n(),
    .groups = "drop"
  ) |> 
  group_by(pphy) |> 
  summarise(n = n()) |> 
  inner_join(
    data |> 
      group_by(Spillmode, pphy, psp) |> 
      summarise(
        n = n(),
        .groups = "drop"
      ) |> 
      group_by(Spillmode, pphy) |> 
      summarise(n = n(), .groups = "drop"), 
    by = "pphy" 
  ) |> # Em acantho, tenho 19 espécies no total, sendo 17 em spillback e 3 espécies em spillover (a soma da 20, porque Pomphorhynchus laevis ocorre em spillover E em spillback)
  mutate(
    proportion = n.x / sum(n.x)*100,
    rel_proportion = n.y / sum(n.y)*100,
    pphy = fct_relevel(pphy, "Euglenozoa", "Negarnaviricota", "Metamonada", "Microsporidia", "Nucleocytoviricota", "Annelida", "Choanozoa", "Cnidaria", "Oomycota", "Ciliophora", "Acanthocephala", "Arthropoda", "Nematoda", "Platyhelminthes")
  ) |> 
  ggplot(
    aes(
      x = pphy, proportion, 
      y = rel_proportion, 
      fill = Spillmode,
    )
  ) +
  geom_bar(stat = "identity", position = "dodge", color= "black") +
  coord_flip() +
  theme_bw(base_size = 12) +
  scale_fill_jco() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 35.5), labels = label_percent(scale = 1), breaks = seq(0, 35, 5)) +
  labs(y = NULL, x = NULL, fill = NULL, title = "d) Number of parasite species") +
  theme(legend.position = "inside", legend.position.inside = c(0.75, 0.15), legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0.3), "cm"))

## fish order
forder.registers <- data |> 
  group_by(horder) |> 
  summarise(
    n = n(),
    .groups = "drop"
  ) |> 
  inner_join(
    data |> 
      group_by(Spillmode, horder) |> 
      summarise(
        n = n(),
        .groups = "drop"
      ), 
    by = "horder"
  ) |>
  mutate(
    proportion = n.x / sum(n.x)*100,
    rel_proportion = n.y / sum(n.y)*100
  ) |> 
  ggplot(
    aes(
      x = reorder(horder, proportion), 
      y = rel_proportion, 
      fill = Spillmode,
    )
  ) +
  geom_bar(stat = "identity", position = "dodge", color= "black") +
  coord_flip() +
  theme_bw(base_size = 12) +
  scale_fill_jco() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22.5), labels = label_percent(scale = 1), breaks = seq(0, 20, 5)) +
  labs(y = NULL, x = NULL, fill = NULL, title = "a) Amount of hosts recorded") +
  theme(legend.position = "none", 
        legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_blank(),    
        axis.ticks.x = element_blank())

forder.species <- data |> 
  group_by(horder, hsp) |> 
  summarise(
    n = n(),
    .groups = "drop"
  ) |> 
  group_by(horder) |> 
  summarise(n = n()) |> 
  inner_join(
    data |> 
      group_by(Spillmode, horder, hsp) |> 
      summarise(
        n = n(),
        .groups = "drop"
      ) |> 
      group_by(Spillmode, horder) |> 
      summarise(n = n(), .groups = "drop"), 
    by = "horder" 
  ) |>
  mutate(
    proportion = n.x / sum(n.x)*100,
    rel_proportion = n.y / sum(n.y)*100,
    horder = fct_relevel(horder, "Blenniiformes", "Esociformes", "Gadiformes", "Kurtiformes", "Scorpaeniformes", "Synbranchiformes", "Osmeriformes", "Pleuronectiformes", "Anabantiformes", "Mugiliformes", "Clupeiformes", "Galaxiiformes", "Characiformes", "Atheriniformes", "Siluriformes", "Perciformes", "Cichliformes", "Cyprinodontiformes", "Salmoniformes", "Centrarchiformes", "Anguilliformes", "Cypriniformes", "Gobiiformes")
  ) |> 
  ggplot(
    aes(
      x = horder, 
      y = rel_proportion, 
      fill = Spillmode,
    )
  ) +
  geom_bar(stat = "identity", position = "dodge", color= "black") +
  coord_flip() +
  theme_bw(base_size = 12) +
  scale_fill_jco() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22.5), labels = label_percent(scale = 1), breaks = seq(0, 20, 5)) +
  labs(y = NULL, x = NULL, fill = NULL, title = "c) Number of host species") +
  theme(legend.position = "none", 
        legend.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
View(forder.registers$data)

(forder.registers + pphyla.registers) / (forder.species + pphyla.species) + plot_annotation(title = "Hosts and parasites spillback and spillover proportions, for amount registers and number of species")

# Parasite functional trait proportions (table and barchart)--------------------------------
spill_lc.records <- data |> 
  group_by(Life.cycle, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |> 
  mutate(percent = n / sum(n)*100) |>
  rename(trait = Life.cycle)

slc.records <- ggplot(spill_lc.records, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 82),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"),
    breaks = seq(0, 80, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0.05, 0), "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
  ) + 
  annotate(geom = "text", label = "A)", x = 0.5, y = 77)
slc.records

spill_lc.species <- data |> 
  group_by(Life.cycle, Spillmode, psp) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(Life.cycle, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |>
  mutate(percent = n / sum(n)*100) |>
  rename(trait = Life.cycle)

slc.species <- ggplot(spill_lc.species, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 82),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"),
    breaks = seq(0, 80, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) + 
  annotate(geom = "text", label = "B)", x = 0.5, y = 77)

spill_loc.records <- data |> 
  group_by(Ecto.or.Endo, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |> 
  mutate(percent = n / sum(n)*100) |> 
  rename(trait = Ecto.or.Endo)

sl.records <- ggplot(spill_loc.records, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 82),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"),
    breaks = seq(0, 80, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

spill_loc.species <- data |> 
  group_by(Ecto.or.Endo, Spillmode, psp) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(Ecto.or.Endo, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |>
  mutate(percent = n / sum(n)*100) |>
  rename(trait = Ecto.or.Endo)

sl.species <- ggplot(spill_loc.species, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 82),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"),
    breaks = seq(0, 80, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

spill_style.records <- data |> 
  group_by(pstyle, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |> 
  mutate(
    percent = n / sum(n)*100,
    pstyle = factor(pstyle, levels = c("Generalist", "Specialist", "Not identified"))
  ) |> 
  rename(trait = pstyle)

ss.records <- ggplot(spill_style.records, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 82),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"),
    breaks = seq(0, 80, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.99, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

spill_style.species <- data |> 
  group_by(pstyle, Spillmode, psp) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(pstyle, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |>
  mutate(percent = n / sum(n)*100) |>
  rename(trait = pstyle)

ss.species <- ggplot(spill_style.species, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 82),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"),
    breaks = seq(0, 80, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

data |> 
  group_by(Life.cycle, psp) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(Life.cycle) |> 
  summarise(n = n(), .groups = "drop")

data |> 
  group_by(Life.cycle) |> 
  summarise(n = n(), .groups = "drop")

data |> 
  group_by(Ecto.or.Endo, psp) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(Ecto.or.Endo) |> 
  summarise(n = n(), .groups = "drop")

data |> 
  group_by(Ecto.or.Endo) |> 
  summarise(n = n(), .groups = "drop")

data |> 
  group_by(pstyle, psp) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(pstyle) |> 
  summarise(n = n(), .groups = "drop")

data |> 
  group_by(pstyle) |> 
  summarise(n = n(), .groups = "drop")

(slc.records + sl.records + ss.records) / (slc.species + sl.species + ss.species) + plot_annotation(title = "Parasite's functional trait proportions by spillmode, for amount of records (A) and number of species (B)")


spill_plot <- rbind(spill_lc.records, spill_style.records, spill_loc.records) |> 
  mutate(
    trait = factor(trait, levels = c("Monoxenous", "Heteroxenous", "Not identified", "Specialist", "Generalist", "Endoparasite", "Ectoparasite")),
    Spillmode = factor(Spillmode, levels = c("spillback", "spillover")) 
  )

ggplot(spill_plot, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  coord_flip() +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 80),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"),
    breaks = seq(0, 80, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

# Host functional traits proportions (table and barchart) ---------------------------------------------------------
hfeed.spill <- data |> 
  group_by(host.feeding, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |> 
  mutate(percent = n / sum(n)*100) |>
  rename(trait = host.feeding)

hf <- ggplot(hfeed.spill, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 72),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%"),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

hmigr.spill <- data |> 
      group_by(Migrant, Spillmode) |> 
      summarise(n = n(), .groups = "drop") |> 
      mutate(percent = n / sum(n)*100) |> 
      rename(trait = Migrant)

hm <- ggplot(hmigr.spill, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 72),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%"),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

hscho.spill <- data |> 
  group_by(shchooling, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |> 
  mutate(percent = n / sum(n)*100) |> 
  rename(trait = shchooling)

hsc <- ggplot(hscho.spill, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 72),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%"),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

hstatus.spill <- data |> 
  group_by(status, Spillmode) |> 
  summarise(n = n(), .groups = "drop") |> 
  mutate(percent = n / sum(n)*100) |> 
  rename(trait = status)

hst <- ggplot(hstatus.spill, aes(x = trait, y = percent, fill = Spillmode)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_jco() +
  theme_bw(base_size = 15) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 72),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%"),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    y = NULL, x = NULL, fill = NULL
  ) +
  theme(
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

library(patchwork)
hf + hm + hsc + hst # Talvez seja melhor apresentar isso em uma tabela.

data |> 
  count(hsp) |> 
  arrange(n) |> 
  View()

hfeed <- data |> 
  group_by(host.feeding) |> 
  summarise(unique_sp = n_distinct(hsp), .groups = "drop") |> 
  mutate(percent = round(unique_sp / sum(unique_sp)*100, 3)) |>
  rename(trait = host.feeding)

hmigr <- data |> 
  group_by(Migrant) |> 
  summarise(unique_sp = n_distinct(hsp), .groups = "drop") |> 
  mutate(percent = round(unique_sp / sum(unique_sp)*100, 3)) |> 
  rename(trait = Migrant)

hscho <- data |> 
  group_by(shchooling) |> 
  summarise(unique_sp = n_distinct(hsp), .groups = "drop") |> 
  mutate(percent = round(unique_sp / sum(unique_sp)*100, 3)) |> 
  rename(trait = shchooling)

hstatus <- data |> 
  group_by(status) |> 
  summarise(unique_sp = n_distinct(hsp), .groups = "drop") |> 
  mutate(percent = round(unique_sp / sum(unique_sp)*100, 3)) |> 
  rename(trait = status)

general.host.table <- rbind(hfeed, hmigr, hscho, hstatus)
  