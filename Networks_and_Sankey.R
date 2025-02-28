packages <- c("openxlsx", "tidyverse", "bipartite", "qgraph", "networkD3", "igraph", "paletteer")

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

# General network structure  --------------------------------------------------------------------------------------
# Sankey diagram for host orders and parasite phyla
# For the sankey diagram, its needed two data frames: the first is the count of links between hosts and parasites; the second is a list of host-parasite connections (which will be the node names).

# The first data frame:
weblinks <- data |> 
  count(horder, pphy) |> 
  as.data.frame()

# The second data frame:
webnodes <- data |> 
  count(horder, pphy) |> 
  pivot_longer(cols = horder:pphy, names_to = "species", values_to = "nodes") |> 
  distinct(nodes) |> 
  as.data.frame()

# Now we need to numerically "name" (starting from 0) each node from our links data frame.
weblinks <- weblinks |> 
  mutate(
    IDsource = match(weblinks$horder, webnodes$nodes)-1,
    IDtarget = match(weblinks$pphy, webnodes$nodes)-1
  )

# Then, we can plot it:
sankeyNetwork(Links = weblinks, Nodes = webnodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n", NodeID = "nodes", 
              sinksRight=T)

### Network structure with 'bipartite'
# For host-parasite species
network <- data |> 
  select(horder, pphy, hsp, psp) |> 
  mutate(nothing = "")  # creating an empty column because frame2webs function requires to specify the network community even if I'm working with just one community, and I don't know how to handle this otherwise.
  
network_sp <- frame2webs(network, varnames = c("hsp", "psp", "nothing"), type.out = "array") |> 
  as.data.frame()

# reordering is needed to see the nestedness pattern in the network.
network_sp <- network_sp[order(rowSums(network_sp), decreasing = T), order(colSums(network_sp), decreasing = T)]

# ploting network
plotweb(network_sp, method = "normal", y.width.low = 0.05, y.width.high = 0.05, y.lim = c(0, 1.92), arrow = "no", col.low = "black", bor.col.low = "black", col.high = "black", bor.col.high = "black", col.interaction = "grey", bor.col.interaction = "black", high.lablength = 0, low.lablength = 0)

# For host-parasite highest levels (order for fish and phyla for parasites)
network_hl <- network |> 
  frame2webs(varnames = c("horder", "pphy", "nothing"), type.out = "array") |> 
  as.data.frame()

length(colnames(network_hl))

network_hl <- network_hl[order(rowSums(network_hl), decreasing = T), order(colSums(network_hl), decreasing = T)]

plotweb(network_hl, method = "normal", text.rot = 90, y.width.low = 0.05, y.width.high = 0.05, y.lim = c(0, 1.92), labsize = 1.2, arrow = "no", col.low = "black", bor.col.low = "black", col.high = "black", bor.col.high = "black", col.interaction = "grey", bor.col.interaction = "black", high.lablength = NULL)

# Origin and invaded area for spillover parasites -----------------------------------------------------------------
# Since this is a Sankey diagram, it will follow the same steps of the first one
spillover <- data |> 
  mutate(
    original.host.continent = str_replace_all(original.host.continent, "Amazon basin", "South America"), ## Talvez seja possivel mudar isso depois nos dados no Excel diretamente, mas como especifiquei abaixo, tenho receio de mexer muito nos dados originais
    original.host.continent = str_replace_all(original.host.continent, "Nile River basin", "Africa"),    ##
    original.host.continent = str_replace_all(original.host.continent, "South Africa", "Africa"),        ##
  ) |> 
  filter(
    lab %in% c("No"),
    meltdown %in% c("No"), ## Estou tirando os eventos de invasional meltdown, porque neste caso o parasita e o hospedeiro são nativos de regiões que não são a mesma região onde o registro de invasão está ocorrendo. Pode ser interessante incoroporar nesse gráfico, já que estou tentando verificar de onde os parasitas saem e para onde vão, mas eu fiquei com receio de modificar a coluna 'porigin' que contém informação de onde eu "soube" que aquele parasita era nativo, seja pelo estudo analisado ou por algum outro estudo que comentou que o parasito é proveniente daquela região. Tem a coluna 'Ambiguous', que informa se a distribuição do parasita é ambuigua (Yes) ou não (No) ou se ele é um registro único da revisão (Unique). As distribuições ambuiguas (n = 77), são aquelas onde eu notei pelo menos dois artigos diferentes dando respostas distintas sobre a distribuição nativa daquela espécie de parasita.
    Spillmode == "spillover",
    original.host.continent %in% c("Africa", "Asia", "Europe", "North America", "Oceania", "South America") ## Existem alguns parasitas que eu não soube de onde era seu hospedeiro original, nem a área original do parasita, ou então tinham descrições muito vagas como "Atlantic ocean". Por isso, estou filtrando somente por aqueles onde a área está com o nome de um dos continentes. Poderíamos discutir juntos para saber em qual continente colocar alguns destes parasitos.
  ) |> 
  select(
    original.host.continent,
    continent ## Isso aqui é onde o parasito foi encontrado no estudo, onde o registro de invasão foi feito. É diferente da coluna 'porigin' porque lá é de onde o hospedeiro nativo (e eu deduzi que por extensão o parasito) é nativo, mas pode ser que ambos tenham distribuições diferentes... é difícil saber a distribuição dos parasitas, principalmente aqueles que são pouco estudados
  ) |> 
  as_tibble()

spillover_links <- spillover |> 
  count(original.host.continent, continent) |> 
  mutate(continent = str_c(continent, " "))

spillover_nodes <- data.frame(
  nodes = c(
    as.character(spillover_links$original.host.continent), 
    spillover_links$continent
  ) |> 
    unique()
)

spillover_links <- spillover_links |> 
  mutate(
    IDsource = match(spillover_links$original.host.continent, spillover_nodes$nodes)-1,
    IDtarget = match(spillover_links$continent, spillover_nodes$nodes)-1
  )

sankeyNetwork(Links = spillover_links, Nodes = spillover_nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n", NodeID = "nodes", 
              sinksRight = T, nodeWidth = 40, fontSize = 13)


spillback <- data |> 
  mutate(
    invaded.host.origin = str_replace_all(invaded.host.origin, "Eurasia: along the coasts of Black, Azov, Caspian and Aegean seas west to Aliakmon drainage (Greece).", "Europe"), 
    invaded.host.origin = str_replace_all(invaded.host.origin, "Eurasia", "Europe")       
  ) |> 
  filter(
    lab %in% c("No"),   
    meltdown %in% c("No"),
    Spillmode == "spillback",
    invaded.host.origin %in% c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
  ) |> 
  select(
    invaded.host.origin,
    continent
  ) |> 
  as_tibble()

spillback_links <- spillback |> 
  count(invaded.host.origin, continent) |> 
  mutate(continent = str_c(continent, " "))

spillback_nodes <- data.frame(
  nodes = c(
    spillback_links$invaded.host.origin, 
    spillback_links$continent
  ) |> 
    unique()
)

spillback_links <- spillback_links |> 
  mutate(
    IDsource = match(spillback_links$invaded.host.origin, spillback_nodes$nodes)-1,
    IDtarget = match(spillback_links$continent, spillback_nodes$nodes)-1
  )

sankeyNetwork(Links = spillback_links, Nodes = spillback_nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n", NodeID = "nodes", 
              sinksRight = T, nodeWidth = 40, fontSize = 13)

# Parasite functional traits networks -----------------------------------------------------------------------------
# Those networks are split into spillback and spillover.
# Creating a spillback only data frame:
spillback <- data |> 
  select(spillmode, psp, hsp, pstyle, Ecto.or.Endo, life.cycle) |> 
  filter(spillmode == "spillback")

# Creating the network and modifying the nodes
traits_spillback <- spillback |> 
  select(psp, hsp) |> 
  graph_from_data_frame(directed = F) %>%
  set_vertex_attr(name = "shape", # Every host will be  shaped as squares, while parasites will be circles
                  value = ifelse(V(.)$name %in% unique(spillback$hsp), "square", "circle")) %>% 
  set_vertex_attr(name = "color", # Every host will be colored as green nodes, while parasites will be blue nodes
                  value = ifelse(V(.)$name %in% unique(spillback$hsp), "lightgreen", "skyblue"))

# Setting the layout
layout_spillback <- qgraph.layout.fruchtermanreingold(
  get.edgelist(traits_spillback, names = F),
  vcount = vcount(traits_spillback),
  area = 8*(vcount(traits_spillback)^2),
  repulse.rad=(vcount(traits_spillback)^3.1)
)

# Spillback networks
#   by infection location (ecto or endoparasite)
plot(
  traits_spillback, layout = layout_spillback, vertex.label = NA, vertex.size = 4,
  edge.color = ifelse(
    spillback$Ecto.or.Endo == "Ectoparasite", 
    yes = "#532B29", 
    no = ifelse(
      spillback$Ecto.or.Endo == "Endoparasite", 
      yes = "#EBCFB2", 
      no = "black"
    )
  )
)

#   by specialization (specialist or generalist)
plot(
  traits_spillback, layout = layout_spillback, vertex.label = NA, vertex.size = 4, 
  edge.color = ifelse(
    spillback$pstyle == "Specialist", 
    yes = "#532B29", 
    no = ifelse(
      spillback$pstyle == "Generalist", 
      yes = "#EBCFB2", 
      no = "#17BEBB"
    )
  )
)

#   by life cycle (monoxenous or heteroxenous)
plot(
  traits_spillback, layout = layout_spillback, vertex.label = NA, vertex.size = 4, 
  edge.color = ifelse(
    spillback$life.cycle == "Monoxenous", 
    yes = "#532B29", 
    no = ifelse(
      spillback$life.cycle == "Heteroxenous", 
      yes = "#EBCFB2", 
      no = "black"
    )
  )
)


# Creating a spillover only data frame:
spillover <- data |> 
  select(spillmode, psp, hsp, pstyle, Ecto.or.Endo, life.cycle) |> 
  filter(spillmode == "spillover")

# Creating the network and modifying the nodes
traits_spillover <- spillover |> 
  select(psp, hsp) |> 
  graph_from_data_frame(directed = F) %>%
  set_vertex_attr(name = "shape", # Every host will be  shaped as squares, while parasites will be circles
                  value = ifelse(V(.)$name %in% unique(spillover$hsp), "square", "circle")) %>% 
  set_vertex_attr(name = "color", # Every host will be colored as green nodes, while parasites will be blue nodes
                  value = ifelse(V(.)$name %in% unique(spillover$hsp), "lightgreen", "skyblue"))

# Setting the layout
layout_spillover <- qgraph.layout.fruchtermanreingold(
  get.edgelist(traits_spillover, names = F),
  vcount = vcount(traits_spillover),
  area = 8*(vcount(traits_spillover)^2),
  repulse.rad=(vcount(traits_spillover)^3.1)
)

# Spillback networks
#   for infection location (ecto or endoparasite)
plot(
  traits_spillover, layout = layout_spillover, vertex.label = NA, vertex.size = 4,
  edge.color = ifelse(
    spillback$Ecto.or.Endo == "Ectoparasite", 
    yes = "#532B29", 
    no = ifelse(
      spillback$Ecto.or.Endo == "Endoparasite", 
      yes = "#EBCFB2", 
      no = "black"
    )
  )
)

#   for specialization (specialist or generalist)
plot(
  traits_spillover, layout = layout_spillover, vertex.label = NA, vertex.size = 4, 
  edge.color = ifelse(
    spillback$pstyle == "Specialist", 
    yes = "#532B29", 
    no = ifelse(
      spillback$pstyle == "Generalist", 
      yes = "#EBCFB2", 
      no = "#17BEBB"
    )
  )
)

#   for life cycle (monoxenous or heteroxenous)
plot(
  traits_spillover, layout = layout_spillover, vertex.label = NA, vertex.size = 4, 
  edge.color = ifelse(
    spillback$life.cycle == "Monoxenous", 
    yes = "#532B29", 
    no = ifelse(
      spillback$life.cycle == "Heteroxenous", 
      yes = "#EBCFB2", 
      no = "black"
    )
  )
)

# Host functional traits networks ---------------------------------------------------------------------------------
#Alltoguether
traits_all <- data |> 
  select(psp, hsp) |> 
  graph_from_data_frame(directed = F) %>%
  set_vertex_attr(name = "shape", # Every host will be  shaped as squares, while parasites will be circles
                  value = ifelse(V(.)$name %in% unique(data$hsp), "square", "circle")) %>% 
  set_vertex_attr(name = "color", # Every host will be colored as green nodes, while parasites will be blue nodes
                  value = ifelse(V(.)$name %in% unique(data$hsp), "lightgreen", "skyblue"))

# Setting the layout
layout_all <- qgraph.layout.fruchtermanreingold(
  get.edgelist(traits_all, names = F),
  vcount = vcount(traits_all),
  area = 8*(vcount(traits_all)^2),
  repulse.rad=(vcount(traits_all)^3.1)
)

plot(
  traits_all, layout = layout_all, vertex.label = NA, vertex.size = 4,
  edge.color = ifelse(
    data$host.feeding == "Carnivorous", 
    yes = "darkred", 
    no = ifelse(
      data$host.feeding == "Omnivorous", 
      yes = "forestgreen", 
      no = "skyblue"
    )
  )
)

# Those networks are split into spillback and spillover.
# Creating a spillback only data frame:
spillback <- data |> 
  select(spillmode, psp, hsp, host.feeding, status, migrant, Shchooling) |> 
  filter(spillmode == "spillback")

# Creating the network and modifying the nodes
traits_spillback <- spillback |> 
  select(psp, hsp) |> 
  graph_from_data_frame(directed = F) %>%
  set_vertex_attr(name = "shape", # Every host will be  shaped as squares, while parasites will be circles
                  value = ifelse(V(.)$name %in% unique(spillback$hsp), "square", "circle")) %>% 
  set_vertex_attr(name = "color", # Every host will be colored as green nodes, while parasites will be blue nodes
                  value = ifelse(V(.)$name %in% unique(spillback$hsp), "lightgreen", "skyblue"))

# Setting the layout
layout_spillback <- qgraph.layout.fruchtermanreingold(
  get.edgelist(traits_spillback, names = F),
  vcount = vcount(traits_spillback),
  area = 8*(vcount(traits_spillback)^2),
  repulse.rad=(vcount(traits_spillback)^3.1)
)

# Spillback networks
#   by feeding behavior
plot(
  traits_spillback, layout = layout_spillback, vertex.label = NA, vertex.size = 4,
  edge.color = ifelse(
    spillback$host.feeding == "Carnivorous", 
    yes = "darkred", 
    no = ifelse(
      spillback$host.feeding == "Omnivorous", 
      yes = "forestgreen", 
      no = "skyblue"
    )
  )
)

#   by IUCN status
plot(
  traits_spillback, layout = layout_spillback, vertex.label = NA, vertex.size = 4,
  edge.color = ifelse(
    spillback$status == "Least concern", 
    yes = "forestgreen", 
    no = ifelse(
      spillback$status == "Not evaluated", 
      yes = "grey", 
      no = ifelse(
        spillback$status == "Data deficient",
        yes = "grey",
        no = "darkred"
      )
    )
  )
)

#   by migration
plot(
  traits_spillback, layout = layout_spillback, vertex.label = NA, vertex.size = 4,
  edge.color = ifelse(
    spillback$migrant == "Yes", 
    yes = "forestgreen", 
    no = ifelse(
      spillback$migrant == "No", 
      yes = "darkred", 
      no = "skyblue"
    )
  )
)

#   by schooling or shoaling behavior
plot(
  traits_spillback, layout = layout_spillback, vertex.label = NA, vertex.size = 4,
  edge.color = ifelse(
    spillback$Shchooling == "Yes", 
    yes = "forestgreen", 
    no = ifelse(
      spillback$Shchooling == "No", 
      yes = "darkred", 
      no = "skyblue"
    )
  )
)

# One mode projection network ------------------------------------------------------------------------------
# Setting our colors:
# Create a list of fish order names
fish_orders <- data.frame(
  horder = c(
  "Polypteriformes",
  "Acipenseriformes",
  "Lepisosteiformes",
  "Amiiformes",
  "Elopiformes",
  "Anguilliformes",
  "Notacanthiformes",
  "Albuliformes",
  "Osteoglossiformes",
  "Hiodontiformes",
  "Alepocephaliformes",
  "Clupeiformes",
  "Gonorynchiformes",
  "Cypriniformes",
  "Gymnotiformes",
  "Siluriformes",
  "Characiformes",
  "Lepidogalaxiiformes",
  "Argentiniformes",
  "Salmoniformes",
  "Esociformes",
  "Stomiatiformes",
  "Osmeriformes",
  "Galaxiiformes",
  "Ateleopodiformes",
  "Aulopiformes",
  "Myctophiformes",
  "Polymixiiforme",
  "Percopsiformes",
  "Zeiformes",
  "Gadiformes",
  "Stylephoriformes",
  "Lampridiformes",
  "Beryciformes",
  "Holocentriformes",
  "Ophidiiformes",
  "Batrachoidiformes",
  "Labriformes",
  "Tetraodontiformes",
  "Lophiiformes",
  "Acanthuriformes",
  "Ephippiformes",
  "Spariformes",
  "Incertae sedis in Eupercaria",
  "Uranoscopiformes",
  "Perciformes",
  "Scorpaeniformes",
  "Centrarchiformes",
  "Pempheriformes",
  "Gobiiformes",
  "Kurtiformes",
  "Syngnathiformes",
  "Scombriformes",
  "Anabantiformes",
  "Synbranchiformes",
  "Pleuronectiformes",
  "Incertae sedis in Carangaria",
  "Carangiformes",
  "Istiophoriformes",
  "Mugiliformes",
  "Blenniiformes",
  "Incertae sedis in Ovalentaria",
  "Cichliformes",
  "Pholidichthyiformes",
  "Atheriniformes",
  "Cyprinodontiformes",
  "Beloniiformes"
  ),
  colors = paletteer_c("grDevices::rainbow", 67)
)

fish_orders2 <- fish_orders |> 
  filter(horder %in% c(unique(data$horder))) |> 
  cbind(data.frame(group = c(1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7),
                   true_colors = c("#3975B9", "#D15022", "#DF5E1F", "#E8681D", "#F28227", "#AD9024", "#B99C33", "#CAAD4B", "#D6BB66", "#645A9F", "#56BD96", "#4BBE9F", "#3DBEAC", "#004616", "#026329", "#137F3B", "#1E9B4A", "#51B16B", "#FA4E5A", "#FF6972", "#FF8288", "#FF989D", "#FFB7BA")))

# Joining the host order colors to our main data.
data_colors2 <- data |> 
  left_join(fish_orders2, join_by(horder))

node_colors <- paletteer_d("ggsci::default_igv")[0:23] # It's a vector of 23 color codes, one for each host order.

# Creating a data frame with the name of each host order and its colors.
order_colors <- data.frame(
  horder = unique(data$horder), 
  color = node_colors
)

# Joining the host order colors to our main data.
data_colors <- data |> 
  left_join(order_colors, join_by(horder))

# One mode projection for spillback registers
  # Creating the spillback network.
spillback_1mode <- data |> 
  filter(Spillmode == "spillback") |> 
  select(hsp, psp) |> 
  graph_from_data_frame(directed = F)

# It was needed to set this network as a bipartite network
V(spillback_1mode)$type <- bipartite_mapping(spillback_1mode)$type

# Then, it was possible to use this function to made a one mode projection. $proj1 is the one mode network for hosts, while $proj2 is for parasites. 
proj_spillback <- bipartite_projection(spillback_1mode)$proj1

# Imputing the color for each node
V(proj_spillback)$color <- data_colors2$true_colors[match(V(proj_spillback)$name, data_colors2$hsp)]

# Setting the layout
lb <- qgraph.layout.fruchtermanreingold(as_edgelist(proj_spillback, names = FALSE), vcount = vcount(proj_spillback), area = 8*(vcount(proj_spillback)^2), repulse.rad = (vcount(proj_spillback)^3.1))

plot(proj_spillback, layout = lb, vertex.label.color = "black", vertex.size = 3, vertex.label = NA) 


# One mode projection for spillover registers
# Creating the spillover network.
spillover_1mode <- data |> 
  filter(Spillmode == "spillover") |> 
  select(hsp, psp) |> 
  graph_from_data_frame(directed = F)

# Exactly the same things done for spillback will be done in here for spillover
V(spillover_1mode)$type <- bipartite_mapping(spillover_1mode)$type

proj_spillover <- bipartite_projection(spillover_1mode)$proj1

V(proj_spillover)$color <- data_colors2$true_colors[match(V(proj_spillover)$name, data_colors2$hsp)]

lo <- qgraph.layout.fruchtermanreingold(as_edgelist(proj_spillover, names = FALSE), vcount = vcount(proj_spillover), area = 8*(vcount(proj_spillover)^2), repulse.rad = (vcount(proj_spillover)^3.1))

plot(proj_spillover, layout = lo, vertex.label.color = "black", vertex.size = 3, vertex.label = NA) 

# Network metrics -------------------------------------------------------------------------------------------------
# This was the code used to find the network indexes described for the full network (with spillback and spillover events), for host orders and parasite phyla:
data |> 
  select(horder, pphy) |> 
  mutate(nothing = "") |> 
  frame2webs(varnames = c("horder", "pphy", "nothing"), type.out = "array") |> 
  as.data.frame() |> 
  networklevel(index = "ALL")

# This is the same, but for host and parasite species:
data |> 
  select(hsp, psp) |> 
  mutate(nothing = "") |> 
  frame2webs(varnames = c("hsp", "psp", "nothing"), type.out = "array") |> 
  as.data.frame() |> 
  networklevel(index = "ALL")

# This is all indexes for host and parasite species registered for spillback
data |> 
  select(hsp, psp, Spillmode) |> 
  mutate(nothing = "") |> 
  filter(Spillmode == "spillback") |> 
  frame2webs(varnames = c("hsp", "psp", "nothing"), type.out = "array") |> 
  as.data.frame() |> 
  networklevel(index = "ALL")

# This is all indexes for host and parasite species registered for spillover
data |> 
  select(hsp, psp, Spillmode) |> 
  mutate(nothing = "") |> 
  filter(Spillmode == "spillover") |> 
  frame2webs(varnames = c("hsp", "psp", "nothing"), type.out = "array") |> 
  as.data.frame() |> 
  networklevel(index = "ALL")