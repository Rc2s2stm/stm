##########################################
# Search num topic
##########################################


# Load necessary libraries
library(stm)
library(tidyverse)
library(openxlsx)
library(stringr)
library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(writexl)
library(gridExtra)


#######
# Preprocess
#######

# Set working directory 
setwd("C:/Users/goszt/Desktop/Inkubator/0_STM_inkubator")

# Load the dataset
DF <- read_csv("InputData/Inkubator_20100101_20231230_STM.csv", col_types = cols(.default = "c"))

# Convert Date column to numeric time variable
DF <- DF %>%
  rename(Date = `Created`)

DF <- DF %>%
  mutate(Date = as.Date(Date),  # Ensure Date is in Date format
         day_number = as.integer(Date - min(Date)) + 1)

# Convert Domain column to Newspaper
DF <- DF %>%
  rename(Newspaper = Domain)

# Create id column
DF <- DF %>%
  mutate(id = row_number()) %>%
  select(id, everything())  # Moves 'id' to the first column

colnames(DF)

# Select relevant columns for STM
DF <- DF %>%
  select(id, title, text, tokens, Date, Newspaper, Newspaper_Category, filter, Periods_Migrations, Periods_Ukraine, day_number)

# Convert categorical variables
DF$NewsCategory <- as.factor(DF$Newspaper_Category)

# create year variable for analysis
DF <- DF %>%
  mutate(year = format(as.Date(Date), "%Y"))

DF %>%
  count(year)


# Alternatively, using dplyr for a more structured output
DF %>%
  count(Newspaper, sort = TRUE)


DF %>%
  group_by(Newspaper, NewsCategory) %>%
  summarise(count = n()) %>%
  arrange(Newspaper, desc(count))

# # # # # # # # # # # # # # 
# searchK K = 10-40
# # # # # # # # # # # # # # 

storage <- searchK(out$documents,
                   out$vocab,
                   K = 10:40,
                   prevalence = ~ NewsCategory + s(day_number),
                   content = ~ NewsCategory,
                   data = out$meta,
                   init.type = "Spectral")

plot(storage)

# Mentés PDF-be
pdf("Models/NewsCategory_model/Results/SearchK/stm_searchK_10_40_results.pdf", width = 9, height = 6)
plot(storage)
dev.off()

saveRDS(storage, "Models/NewsCategory_model/Results/SearchK/searchK_10_40_storage.rds")

# Semantic coherence 
K_values <- 10:40
results <- data.frame(K = integer(), MeanSemanticCoherence = numeric())

for (k in K_values) {
  cat("Futtatás K =", k, "...\n")
  
  model <- stm(documents = out$documents,
               vocab = out$vocab,
               K = k,
               prevalence = ~ NewsCategory + s(day_number),
               content = ~ NewsCategory,
               data = out$meta,
               init.type = "Spectral")
  
  coh <- semanticCoherence(model = model, documents = out$documents)
  avg_coh <- mean(coh)
  
  results <- rbind(results, data.frame(K = k, MeanSemanticCoherence = avg_coh))
}

results


# Semantic coherence ggplot
sc_plot <- ggplot(results, aes(x = K, y = MeanSemanticCoherence)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(shape = 21, color = "black", fill = "white", size = 1.5, stroke = 0.8) +
  labs(
    title = "Semantic Coherence",
    x = "Number of Topics (K)",
    y = "Mean Semantic Coherence"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black"),
    panel.background = element_blank()
  )


sc_plot

# PDF mentés: előbb searchK ábrák, majd ggplot
pdf("Models/NewsCategory_model/Results/SearchK/stm_Inkubator_searchK_full_12_13.pdf")

# 1–4 oldal: STM diagnosztikai plotok
plot(storage)

# 5. oldal: saját semantic coherence ábra
gridExtra::grid.arrange(sc_plot)

dev.off()

########################################################
# II. Model: News Category Model
########################################################

#######
# Fit STM model with optimal K = 25
#######

# Cseréljük az ő -> o és ű -> u karaktereket az összes releváns oszlopban
DF$tokens <- str_replace_all(DF$tokens, c("ő" = "o", "ű" = "u"))
DF$NewsCategory <- str_replace_all(DF$NewsCategory, c("ő" = "o", "ű" = "u"))
DF$Newspaper <- str_replace_all(DF$Newspaper, c("ő" = "o", "ű" = "u"))


# Prepare for STM - lower.thresh = 50

metadata <- DF %>%
  select(c(id, title, text, NewsCategory, Date, day_number, Newspaper,year))

DF_processed <- textProcessor(DF$tokens, 
                              metadata = metadata,
                              removepunctuation  = FALSE,
                              lowercase = FALSE,
                              removenumbers  = FALSE,
                              removestopwords = FALSE,
                              stem = FALSE,
                              wordLengths = c(1, Inf))

out <- prepDocuments(DF_processed$documents, DF_processed$vocab, 
                     DF_processed$meta, lower.thresh = 50)



# NewsCategory K=25
DF_Inkubator_25 <- stm(documents = out$documents,
                       vocab = out$vocab,
                       K = 25,
                       prevalence = ~ NewsCategory + s(day_number),
                       content = ~ NewsCategory,
                       data = out$meta,
                       init.type = "Spectral")

plot.STM(DF_Inkubator_25, type = "summary",
         main="Topic prevalence",
         xlim=c(0,.4))


#### Save STM model
saveRDS(DF_Inkubator_25, "Models/NewsCategory_model/Data/stm_model_Inkubator_k25_NewCats.rds")

#### Save topic prevalence summary plot
pdf("Models/NewsCategory_model/Results/Topic_prevalence_SUM/stm_Inkubator_t25_prevalence_summary_NewsCat.pdf")
plot.STM(DF_Inkubator_25, type = "summary",
         main="Topic prevalence",
         xlim=c(0,.4))
dev.off()


##################################
# Analysis
##################################


#######
# Topic perspectives analysis !!!! tablouid!!! ő ű 
#######

# Just check
str(out$meta)
unique(out$meta$NewsCategory)
par(family = "ArialMT") 
Sys.setlocale("LC_CTYPE", "hu_HU.UTF-8")


out$meta$NewsCategory <- factor(out$meta$NewsCategory)
covars <- levels(out$meta$NewsCategory)
print(covars)


for (top in 1:25) {
  for (persp1i in 1:(length(covars)-1)) {
    for (persp2i in (persp1i+1):length(covars)) {
      
      # Debugging print statement
      print(paste("Plotting:", covars[persp1i], "vs", covars[persp2i], "for topic", top))
      
      # Safe plotting with try() to avoid errors breaking the loop
      try({
        pdf(paste0("Models/NewsCategory_model/Results/Topic_perspectives_analysis/t25/stm_Inkubator_t25_topic_persp_topic", 
                   top, "_", covars[persp1i], '_', covars[persp2i], ".pdf"))
        plot(DF_Inkubator_25, type = "perspectives", topics = top, 
             covarlevels = c(covars[persp1i], covars[persp2i]))
        dev.off()
      }, silent = TRUE)
      
      # Display plot in R instead of saving
      try({
        plot(DF_Inkubator_25, type = "perspectives", topics = top, 
             covarlevels = c(covars[persp1i], covars[persp2i]))
      }, silent = TRUE)
    }
  }
}




############
# Extract top words for each topic
############
# Define function to extract top words
extract_top_words_by_covariate <- function(stm_model, topic, top_n = 15) {
  top_words_list <- list()
  covariate_levels <- stm_model$settings$covariates$yvarlevels
  
  for (i in 1:length(stm_model$beta$logbeta)) {
    beta_level <- exp(stm_model$beta$logbeta[[i]])
    sorted_indices <- order(beta_level[topic, ], decreasing = TRUE)
    top_words <- stm_model$vocab[sorted_indices[1:top_n]]
    top_probs <- beta_level[topic, sorted_indices[1:top_n]]
    top_words_df <- data.frame(Word = top_words, Probability = top_probs)
    colnames(top_words_df) <- paste0(colnames(top_words_df), "_", covariate_levels[i])
    top_words_list[[i]] <- top_words_df
  }
  result_df <- do.call(cbind, top_words_list)
  return(result_df)
}

# Extract top words for each topic
for (top in 1:25) {
  cat(paste0("-----------------------\n--------  ", top, "  ----------\n"))
  df <- extract_top_words_by_covariate(DF_Inkubator_25, top, top_n=15)
  print(df)
  write.xlsx(df, paste("Models/NewsCategory_model/Results/Top_words/NewCat/t25/stm_Inkubator_t25_topic_words_topic_news", top, ".xlsx", sep=""))
}

###########
# Identify and display cluster-defining words
##########
toplabs <- labelTopics(DF_Inkubator_25, c(1:25), n=50)
toplabs

# each topic and save to CSV
df_all_topics <- as.data.frame(t(toplabs$topics))  
colnames(df_all_topics) <- sprintf("Topic_%02d", 1:25)
# CSV 
write.csv(
  df_all_topics,
  file = "Models/NewsCategory_model/Results/Top_words/Topics/t25/stm_Inkubator_t25_topic_topwords.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

#Excel
df_all_topics <- as.data.frame(t(toplabs$topics))  
colnames(df_all_topics) <- sprintf("Topic_%02d", 1:25)

# Save as Excel
write_xlsx(
  df_all_topics,
  path = "Models/NewsCategory_model/Results/Top_words/Topics/t25/stm_Inkubator_t25_topic_topwords.xlsx"
)

########
# Identify characteristic articles
########

# Select the 4 most representative articles for each topic
found_articles <- findThoughts(DF_Inkubator_25, texts = metadata$text, n = 20, topics = 1:24)


# Loop through each topic and print the selected articles
for (top_n in seq_along(found_articles$docs)){
  topic_docs <- found_articles$docs[[top_n]]  # Extract topic-specific texts
  
  if (!is.null(topic_docs)) {
    cat("\n----------------------------\n")
    cat("Topic", top_n, "\n")
    cat("----------------------------\n")
    
    # Print each selected article text
    for (i in seq_along(topic_docs)) {
      cat("\n[Article", i, "]\n")
      cat(topic_docs[i], "\n")
    }
  }
}


# Export
output_file <- "Models/NewsCategory_model/Results/Top_Articles/t25/stm_Inkubator_t25_characteristic_articles_20.txt"

con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(con)

for (top_n in seq_along(found_articles$docs)) {
  topic_docs <- found_articles$docs[[top_n]]
  
  if (!is.null(topic_docs)) {
    cat("\n----------------------------\n")
    cat("Topic", top_n, "\n")
    cat("----------------------------\n")
    
    for (i in seq_along(topic_docs)) {
      cat("\n[Article", i, "]\n")
      cat(topic_docs[i], "\n")
    }
  }
}

sink()

#########
# Compute topic correlation
########

mod.out.corr_25 <- topicCorr(DF_Inkubator_25)
plot(mod.out.corr_25)

# Round and print correlation matrix
round(mod.out.corr_25$cor, 2)

#### Save
pdf("Models/NewsCategory_model/Results/Topic_corr/t25/stm_Inkubator_t25_topic_corr.pdf")
plot(mod.out.corr_25)
dev.off()

# Nicer I.
plot(mod.out.corr_25, topic.labels = topic_labels)

# Topics
topic_labels <- c(
  "Magyar kormányzati válaszreakciók háborús rendkívüli helyzetekre",
  "Járványkezelés és egészségpolitika a COVID alatt",
  "Háborús emlékezet és személyes narratívák",
  "Közlekedési és határellenőrzési hírek",
  "Ukrán menekültek magyarországi befogadása és helyzete",
  "Nemzetközi diplomácia és béketárgyalások",
  "Kárpátaljai magyar kisebbség helyzete",
  "Gazdasági szankciók és következmények",
  "Civil és karitatív segítségnyújtás Ukrajnának",
  "Minszki megállapodás és tűzszüneti tárgyalások",
  "Energiaárak és inflációs hatások Magyarországon",
  "Orosz-ellenes tiltakozások és szolidaritási mozgalmak",
  "Oroszország háborús narratívái és dezinformáció",
  "Politikai nyilatkozatok a háborúról Magyarországon",
  "Európai uniós egyeztetések és intézkedések",
  "NATO és EU szerepvállalás Ukrajna támogatásában",
  "Gázfüggőség és energiapolitika az orosz–ukrán háború tükrében",
  "Migrációellenes határpolitika és határvédelem",
  "Magyar kormány narratívái Ukrajnáról és Kárpátaljáról",
  "Légi közlekedés, Orosz légi konfliktus és kisebb incidensek",
  "Amerikai politikai reakciók az orosz invázióra",
  "Ukrán frontvonalak és hadműveletek",
  "Nyugati fegyverszállítás Ukrajna felé",
  "Orosz támadások, ukrán ellenállás, belpolitikai hatások",
  "Nukleáris létesítmények és háborús fenyegetések"
)


# Graph
g <- graph_from_adjacency_matrix(mod.out.corr_25$posadj, mode = "undirected", diag = FALSE)

# Rename Nodes
V(g)$name <- topic_labels

png("Models/NewsCategory_model/Results/Topic_corr/t25/stm_Inkubator_t25_topic_graph_nicer.png", width = 800, height = 600)
plot(g,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.size = 30,
     vertex.color = "lightgreen",
     edge.lty = "dashed")
dev.off()

# Nicer II.

topic_labels <- c(
  "Magyar válaszreakciók",
  "COVID-19 egészségpolitika",
  "Háborús emlékezet",
  "Határ- és közlekedési hírek",
  "Menekültek befogadása",
  "Diplomácia és béketárgyalás",
  "Kárpátaljai magyarok",
  "Szankciók és gazdaság",
  "Civil segítségnyújtás",
  "Tűzszüneti egyeztetések",
  "Energiaárak és infláció",
  "Tiltakozások és szolidaritás",
  "Orosz narratíva és infoháború",
  "Magyar politikai nyilatkozatok",
  "EU-s intézkedések",
  "NATO és EU támogatás",
  "Energiastratégia és gázfüggőség",
  "Migrációellenes politika",
  "Kormányzati Ukrajna-narratíva",
  "Légi incidensek",
  "Amerikai reakciók",
  "Ukrán fronthelyzet",
  "Nyugati fegyverszállítás",
  "Orosz támadások, ellenállás",
  "Nukleáris fenyegetések"
)


# Graph
g <- graph_from_adjacency_matrix(mod.out.corr_25$posadj, mode = "undirected", diag = FALSE)
V(g)$name <- topic_labels

# Louvain-clusters
V(g)$community <- cluster_louvain(g)$membership

# tg
tg <- as_tbl_graph(g)

# Plot
ggraph(tg, layout = "fr") +  # fr = Fruchterman-Reingold layout
  geom_edge_link(aes(), edge_colour = "gray70", linetype = "dashed") +
  geom_node_point(aes(color = as.factor(community)), size = 8, alpha = 0.8) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4, color = "black") +
  theme_graph(base_family = "Arial") +
  theme(legend.position = "none") +
  ggtitle("Topic Correlation Graph (STM, k = 25)") +
  labs(caption = "Dashed lines indicate positive topic correlations")



# Save Plot
ggsave(
  filename = "Models/NewsCategory_model/Results/Topic_corr/t25/stm_Inkubator_t25_topic_graph_nicer_clusters.png",
  plot = ggraph(tg, layout = "fr") + 
    geom_edge_link(edge_colour = "gray70", linetype = "dashed") +
    geom_node_point(aes(color = as.factor(community)), size = 8, alpha = 0.8) +
    geom_node_text(aes(label = name), repel = TRUE, size = 4, color = "black") +
    theme_graph(base_family = "Arial") +
    theme(legend.position = "none") +
    ggtitle("Topic Correlation Graph (STM, k = 25)") +
    labs(caption = "Dashed lines indicate positive topic correlations"),
  width = 10, height = 8, dpi = 300
)

#################
# Estimate effects of metadata on topic prevalence
#################


table(out$meta$NewsCategory)

### gov az alap
out$meta$NewsCategory <- relevel(as.factor(out$meta$NewsCategory), ref = "gov")

table(out$meta$NewsCategory)

prep <- estimateEffect(1:25 ~ NewsCategory + s(day_number),
                       DF_Inkubator_25,
                       meta = out$meta,
                       uncertainty = "Global")

for (i in 1:25) {
  cat(paste0("\n----- Topic ", i, " -----\n"))
  print(summary(prep, topics = i))
}



#### Save estimated effects
saveRDS(prep, file = "Models/NewsCategory_model/Data/t25/stm_Inkubator_t25_estimated_effects_newsCat.rds")
# prep <- readRDS("Data/Estimated_effects/News/stm_Webaratas_t11_estimated_effects_newsCat.rds")


#######
# Evolution of Topics Over Time
#######

for (topic_num in prep$topics) {
  jpeg(paste("Models/NewsCategory_model/Results/Evolution_of_Topics_Over_Time/t25/stm_Inkubator_t25_time_vs_prev_topic", topic_num, ".jpeg", sep=""))
  plot(prep, "day_number", method = "continuous", topics = c(topic_num),
       printlegend = FALSE, xaxt = "n")
  month_dates <- seq(from = min(out$meta$Date), to = max(out$meta$Date), by = "month")
  month_labs <- format(month_dates, "%Y-%m")
  posi = as.integer((month_dates - min(month_dates)) / (60*60*24)) + 1
  axis(1, at = posi, labels = month_labs)
  abline(v = as.integer(as.Date("2010-01-01") - as.Date(min(out$meta$Date))), col = "blue", lty = 2)
  title(paste("Topic", topic_num))
  dev.off()
  
  # Display plot in R
  plot(prep, "day_number", method = "continuous", topics = c(topic_num),
       printlegend = FALSE, xaxt = "n")
}

jpeg("Models/NewsCategory_model/Results/Evolution_of_Topics_Over_Time/t25/stm_Inkubator_t25_timegrid.jpeg", 
     width = 2500, height = 2500, res = 200)

par(mfrow = c(5, 5), mar = c(3, 3, 2, 1)) 

for (topic_num in 1:25) {
  plot(prep, "day_number", method = "continuous", topics = topic_num,
       printlegend = FALSE, xaxt = "n", xlab = "", ylab = "", main = paste("Topic", topic_num))
  abline(v = as.integer(as.Date("2010-01-01") - as.Date(min(out$meta$Date))), col = "blue", lty = 2)
}

dev.off()

#######
# Topic prevalence by NewsCat
#######

for (topic_num in 1:25) {
  pdf(paste("Models/NewsCategory_model/Results/Topic_prevalence/t25/stm_Inkubator_t25_NewsCat", topic_num, ".pdf", sep=""))
  plot(prep, covariate = "NewsCategory", 
       topics = topic_num, 
       method = "pointestimate", 
       main = paste("Topic", topic_num, "Prevalence by Outlet Type"),
       xlab = "Prevalence",
       ylab = "Outlet Type",
       ci.level = 0.95)  # Add confidence intervals
  dev.off()
  
  # Display plot in R
  plot(prep, covariate = "NewsCategory", 
       topics = topic_num, 
       method = "pointestimate", 
       main = paste("Topic", topic_num, "Prevalence by Outlet Type"),
       xlab = "Prevalence",
       ylab = "Outlet Type",
       ci.level = 0.95)
}


##########################################
####### Get the Dominant Topic per Document Topik-dominancia meghatarozása dokumentumonkent
##########################################
library(writexl)

# Get the most dominant topic (i.e., the one with highest probability) for each document
dominant_topic <- apply(DF_Inkubator_25$theta, 1, which.max)

# Add to metadata
out$meta$dominant_topic <- dominant_topic

# Count how many documents are most strongly associated with each topic
table(out$meta$dominant_topic)


# Excel
dominant_table <- as.data.frame(table(out$meta$dominant_topic))
colnames(dominant_table) <- c("Topic", "Document_Count")
write_xlsx(dominant_table, "Models/NewsCategory_model/Results/Dominant Topic per Document and per NewsCategory/t25/stm_Inkubator_t25_Dominant_Per_Document.xlsx")

############
# how many articles per NewsCategory are strongly associated (e.g., θ > 0.3) with each topic
############
library(purrr)
library(dplyr)

# Set a threshold to define "strong association"
threshold <- 0.3

# Create empty list to store per-topic tables
topic_by_category_list <- list()

# Loop through each topic (1 to 25)
for (k in 1:25) {
  # Logical vector: TRUE if topic k is strong in the document
  relevant_docs <- DF_Inkubator_25$theta[, k] > threshold
  
  # Subset NewsCategory of these documents
  cats <- out$meta$NewsCategory[relevant_docs]
  
  # Create frequency table for NewsCategory
  topic_table <- table(cats)
  
  # Save as a data.frame
  df <- as.data.frame(topic_table)
  colnames(df) <- c("NewsCategory", paste0("Topic_", k))
  
  # Save to list
  topic_by_category_list[[k]] <- df
}

# Merge all into one wide table by NewsCategory
# Start with the first topic's data frame
topic_by_category_df <- reduce(topic_by_category_list, full_join, by = "NewsCategory")

# Replace NAs with 0
topic_by_category_df[is.na(topic_by_category_df)] <- 0

# View result
print(topic_by_category_df)

write_xlsx(topic_by_category_df,
           "Models/NewsCategory_model/Results/Dominant Topic per Document and per NewsCategory/t25/stm_Inkubator_t25_Topic_by_NewsCategory.xlsx")

