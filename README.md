# Master-thesis
Here are the data and R scripts used to perform all the analyses and generate the figures for the publication on Biological Invasions.

# Metadata description
The metadata used are a .xlsx file provenient from a systematic search for the overview. The methods and publication can be found at DOI:10.1007/s10530-025-03555-y.

I will explain the meaning of each colunm in the dataset. The R code used for data manipulation are in this repositoire.

**ID:** The ID number of that article. Each scientific article elected for the review have one unique ID number. Each row from the dataset are a single observation of parasite invasion.

**DOI:** That scientific article's DOI, when available.
**WoS categories:** The scientific field of research, classified from Web of Science. The articles not present in the Web of Science database are in blank for this column.
**Research Areas:** The same as *WoS categories* but for the Scopus classification.
**Type:** If the article is a scientific article (J), a communication, short communication, note, etc.
**Authors:** The authors list of that particular article.
**Source title:** The journal in which that article was published in.
**Year:** Year of publication.
**Title:** Title of that particular article.
**Abstract:** Abstract of that particular article.
**Native host:** The common native host for that parasite recorded.
**Parasite phylum:** The parasite phylum for each parasite recorded.
**Parasite class:** The parasite class for each parasite recorded.
**Parasite order:** The parasite order for each parasite recorded.
**Parasite family:** The parasite family for each parasite recorded.
**Parasite genus:** The parasite genus for each parasite recorded.
**Parasite species:** The parasite species for each parasite recorded.
**Parasite origin:** From where is that parasite native from.
**Life cycle:** If the parasite life cycle was either heteroxenous or monoxenous.
**Reproduction:** Form of reproduction, classified as: “sexual”, “asexual”, “both”, “replication” or “not identified”.
**Parasite specialization:** Whether the parasite is a specialist or generalist. If the parasite is reported in literature occurring in 2 or more orders, it was considered generalist.
**N host orders:** The number of host orders in which the particular parasite species occurred in this review records.
**N host families:** The number of host families in which the particular parasite species occurred in this review records.
**Ambiguous native range:** Since accessing the native occurrence area of parasites requires significant effort, it was checked whether, among the review records, the native occurrence area was concordant or discordant. Here, the answers were "Yes" when the distribution was discordant from one record to another on a continental scale, and "No" when the distribution was concordant. In cases where only one study recorded the species, the answer was "Unique”, indicating that the record was registered in only in one document in this review.
**Ecto or Endo:** Where the parasite is allocated. It was classified as: “endoparasite” or “ectoparasite”.
**Micro or macro:** If the particular parasite species is a microparasite or a macroparasite.
**Euc proc or virus:** The parasite nuclear envelope. Classified as “eukaryotic”, “prokaryote”, or “virus”.
**Identification methodology:** Identification methodology used for parasite species, classified as: DNA or Morphometry/Morphology.
**Life stage:** If the parasite is in its definitive or intermediate host.
**Infection site:** Where the parasite was registered in that article.
**Invaded host phylum:** The host phylum for each parasite recorded.
**Invaded host class:** The host class for each parasite recorded.
**Invaded host order:** The host host for each parasite recorded.
**Invaded host family:** The host family for each parasite recorded.
**Invaded host genus:** The host genus for each parasite recorded.
**Invaded host species:** The host species for each parasite recorded.
**Invaded host indigenous to:** The native region of the infected host in the study.
**Original host Indigenous to Continent:** The original host’s native region of the parasite recorded in the study.
**Invaded host Feeding behaviour:** Feeding habit, classified as “piscivorous”, "carnivorous," "herbivorous," "detritivorous," or "omnivorous".
**IUCN Red List Status:** The host IUCN Red List Status, classified as “critically endangered”, “endangered”, “vulnerable”, “near threatened”, “not evaluated”, “data deficient”, and “least concern”.
**Host specialization:** If the host is classified as generalist or specialist.
**Trophic level:** Host trophic level, according to FishBase website. This trait was not available for all host species.
**Migrant:** Whether the host is migratory or not.
**Shoaling or Schooling:** Whether schoaling or schooling behavior is present or absent.
**Size cm:** The maximum size of that particular host, from FishBase.
**Study environment:** In which freshwater environment the parasite record occurred.
**Experiment:** Whether experiments were conducted in the study. Categorized as: “yes”, “no” or “yes, exclusively”.
**Factor inducing invasion:** The factor that led to the invasion recorded in the study. Categorized as: “aquarism”, “ballast water”, “biological control”, “pisciculture”, “fisherman”, “experiments”, “special case”a, or “unknown or not mentioned”.
**Spillmode:** Mode of invasion, classified as “S/DE” or “spillover”.
**Invaded continent:** Continent where the recorded invasion occurred.
**Invaded country:** Country where the recorded invasion occurred.
**Invaded area:** Biogeographic realm in which the recorded invasion occurred, classified according to Wallace (2011).
**Invasion meltdown:** Whether the study records the invasion of a parasite into a host species that is not native to the parasite's native region and is also invasive to the region where the record is made.
