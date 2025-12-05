### 1. Summarizing incidents involving AI and death.
###
### The  data was downloaded from AIM of OECD as xlsx files. 
### Since only up to 100 records can be seen or downloaded, records were downloaded three times with different "sort by" options: 
### Date of first reporting (incidence-2.xlsx), Number of articles (incidence-3.xlsx), and Relevance (incidence-4.xlsx). 

library(tidyverse)
library(ggraph)
library(readxl)

### Setting a file path of the files to be imported.
file_path = "OECD/xls/"

file_lists <- paste0(file_path, list.files(path = file_path, pattern = "\\.xlsx$"))

### Reading the all three xlsx files and combining them in a data frame.
all_incidence <- do.call(rbind, lapply(file_lists, function(x) read_excel(x))) %>% 
  distinct()
### All 157 records were successfully collected. 

### Determining the number of occurrence of each concept terms. 
concepts_separated <- all_incidence %>% 
  ### Assigning a unique ID number to each record. 
  mutate(ID = 1:nrow(.)) %>% 
  ### Counting the number of "concepts" (which are essentially keywords related to the record) in each record.
  mutate(n_concepts_in_record = str_count(.$concepts, ",") + 1) %>% 
  ### Separating each concept in different columns.
  separate_wider_delim(., col = concepts, delim = ", ", 
                       names = paste("concept", seq(1:max(.$n_concepts_in_record)), sep = "_"), 
                       too_few = "align_start", cols_remove = FALSE) %>% 
  ### Changing to a longer format. 
  pivot_longer(cols = starts_with("concept_"), names_to = "name", values_to = "concept", values_drop_na = TRUE) %>% 
  group_by(concept) %>% 
  ### Counting the number of occurrence of each concept across the record corpus. 
  mutate(occurrence_concept = n()) %>% 
  ungroup()

### Showing the number of occurrence of concept terms
concepts_number_occurrence <- concepts_separated %>% 
  select(concept, occurrence_concept) %>% 
  distinct() %>% 
  arrange(desc(occurrence_concept))

### Based on the above, the following four frequently occurring concept terms will be ignored, 
### as they are not specific enough to be informative. 
excluded_top_concepts <- c("Artificial intelligence", "Inc.", "Algorithm", "Technology", "Software")

### Selecting concept terms to be included in the analysis. 
selected_concepts <- concepts_separated %>% 
  ### Concept terms that occur more than three times are included in the analysis. 
  filter(occurrence_concept > 3, 
         ### Excluding the excluded terms. 
         !concept %in% excluded_top_concepts) %>% 
  select(ID, concept) %>% 
  distinct() 

### Assigning unique ID numbers to each concept term. 
concepts_id <- selected_concepts %>% 
  select(concept) %>% 
  distinct() %>% 
  mutate(concept_ID = 1:nrow(.))

### Counting the number of co-occurrence of pairs of concept terms. 
concepts_co_occurrence_count <- selected_concepts %>% 
  rename(from_concept = concept) %>% 
  ### Performing left_join by the record ID. 
  ### By doing this, all pairwise combinations of concept terms within a record are made as a combination of two columns
  left_join(., selected_concepts, by = "ID", relationship = "many-to-many") %>% 
  rename(to_concept = concept) %>% 
  group_by(from_concept, to_concept) %>% 
  ### Counting the number of co-occurrence of pairwise combinations of concept terms.
  mutate(co_occurrence_count = n()) %>% 
  ungroup() %>% 
  select(!ID) %>% 
  distinct()

### The above data frame still contains duplicate combinations, such as "Tesla - cars" and "cars - Tesla"
### These duplicates are removed here. 
selected_concepts_co_occurrence <- concepts_co_occurrence_count %>% 
  ### Adding concept term IDs
  left_join(., concepts_id, by = c("from_concept" = "concept")) %>% 
  rename(concept_ID_from = concept_ID) %>% 
  left_join(., concepts_id, by = c("to_concept" = "concept")) %>% 
  rename(concept_ID_to = concept_ID) %>% 
  ### Keeping rows where concept ID of "from" is smaller than concept ID of "to". 
  filter(concept_ID_from < concept_ID_to) %>% 
  ### renaming columns so that they can be readily recognized by ggraph functions. 
  rename(from = from_concept, 
         to = to_concept, 
         n = co_occurrence_count) %>% 
  select(from, to, n) %>% 
  ### Only plotting co-occurrence bigger than 2. 
  filter(n > 2)

### Making a preliminary graph to see how it looks. 
graph_object_test <- selected_concepts_co_occurrence %>% 
  igraph::graph_from_data_frame()

set.seed(1)

ggraph(graph_object_test, layout = "fr") +
  geom_edge_link(aes(edge_width = n), alpha = 0.2) +
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1.5)
### It seems there are four clusters of concept terms, 
### with central terms being "Tesla", "Israel", "Suicide", and "Unmanned aerial vehicle".

### Below, concept terms are assigned to one of these four groups, or the "other" group. 
### If a concept term co-occurs with one of these four central terms, the concept term is assigned to the corresponding group.
### If not, or if a concept term co-occurs with more than one of these four terms, it is assigned to the "other" group. 
### Making a custom function to identify concept terms that co-occur with the central terms. 
fn_concept_grouping <- function(central_concept) { 
  concepts_group <- selected_concepts_co_occurrence %>% 
    filter(from == central_concept | to == central_concept) %>% 
    pivot_longer(cols = c("from", "to"), names_to = "name", values_to = "concept") %>% 
    select(concept) %>% 
    distinct() %>% 
    mutate(concept_group = central_concept)
}

### Making a data frame of concept terms and corresponding groups. 
concepts_grouped <- rbind(fn_concept_grouping("Tesla"), 
                          fn_concept_grouping("Israel"), 
                          fn_concept_grouping("Suicide"), 
                          fn_concept_grouping("Unmanned aerial vehicle")) %>% 
  group_by(concept) %>% 
  mutate(occurrence = n()) %>% 
  ungroup() %>% 
  ### If a concept term is associated with more than one group, it is assigned to the "other" group. 
  mutate(concept_group = ifelse(occurrence > 1, "other", concept_group)) %>% 
  distinct()

### Making a data frame for figure. 
concepts_modified <- selected_concepts %>% 
  group_by(concept) %>% 
  mutate(total_occurrence = n()) %>% 
  ungroup() %>% 
  select(!ID) %>% 
  distinct() %>% 
  filter(concept %in% c(selected_concepts_co_occurrence$from, selected_concepts_co_occurrence$to)) %>% 
  ### Adding the concept term groups. 
  left_join(., concepts_grouped[, c(1, 2)], by = "concept") %>% 
  mutate(concept_group = ifelse(!is.na(concept_group), concept_group, "other")) %>% 
  ### Assigning the concept terms into one of three groups based on the importance / the number of occurrence, so that 
  ### text of concept terms in the figure has different size according to the occurrence. 
  mutate(tier1_terms = ifelse(concept %in% c("Unmanned aerial vehicle", "Suicide", "Israel", "Tesla"), concept, "")) %>% 
  mutate(tier2_terms = ifelse(total_occurrence > 10 & tier1_terms == "", concept, ""), 
         tier3_terms = ifelse(total_occurrence < 11, concept, ""))

### Making a graph object of igraph. 
graph_object2 <- selected_concepts_co_occurrence %>% 
  igraph::graph_from_data_frame(., vertices = concepts_modified)

set.seed(29)

### Plotting
AI_death_concept <- ggraph(graph_object2, layout = "fr") +
  geom_edge_link(aes(edge_width = n), show.legend = FALSE, alpha = 0.04) +
  geom_node_point(size = 1, alpha = 0.05) + 
  geom_node_text(aes(label = tier1_terms, color = concept_group), size = 3, vjust = 1.2, fontface = 2) + 
  geom_node_text(aes(label = tier2_terms, color = concept_group), size = 2.7, vjust = 1.5) + 
  geom_node_text(aes(label = tier3_terms, color = concept_group), size = 2.3, vjust = 1.5) + 
  scale_x_continuous(expand = expansion(mult = 0.15)) + 
  scale_y_continuous(expand = expansion(mult = 0.05)) + 
  scale_color_manual(values = c("blue", "grey40", "violetred1", "springgreen4", "orange3")) + 
  guides(color = "none", size = "none") + 
  theme(panel.background = element_rect(fill = "white"))

ggsave(AI_death_concept, 
       filename = "figures/fig_AI_death_concept.png", 
       width = 150, height = 110, units = "mm", dpi = 300)


### For graphical abstract, a simplified image is made. 
###
### concepts_modified data frame is saved as a csv file. 
### A new "vjust" column is added manually, which shows vertical justification of each text to show in the figure. 
### The resulting csv file is loaded.

#write.csv(concepts_modified, file = "csv/concepts_modified.csv", row.names = FALSE)
concepts_vjust <- read.csv("csv/concepts_modified.csv")

graph_object2 <- selected_concepts_co_occurrence %>% 
  igraph::graph_from_data_frame(., vertices = concepts_vjust)

set.seed(29)

AI_death_GA <- ggraph(graph_object2, layout = "fr") +
  geom_edge_link(aes(edge_width = n), show.legend = FALSE, alpha = 0.04) +
  geom_node_point(size = 1, alpha = 0.05) + 
  geom_node_text(aes(label = tier1_terms, color = concept_group), size = 3, vjust = 1.2, fontface = 2) + 
  geom_node_text(aes(label = tier2_terms, color = concept_group), size = 2.2, vjust = concepts_vjust$vjust, fontface = 2) + 
  scale_x_continuous(expand = expansion(mult = 0.15)) + 
  scale_y_continuous(expand = expansion(mult = 0.05)) + 
  scale_color_manual(values = c("blue", "grey40", "violetred1", "springgreen4", "orange3")) + 
  guides(color = "none", size = "none") + 
  theme(panel.background = element_rect(fill = "white"))

ggsave(AI_death_GA, 
       filename = "figures/AI_death_GA.png", 
       width = 60, height = 60, units = "mm", dpi = 300)
