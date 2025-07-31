################################
# # Rainette ## développé par Julien Barnier
# https://cran.r-project.org/web/packages/rainette/vignettes/introduction_usage.html
# https://juba.r-universe.dev/builds
################################


# install.packages(c("rainette", "quanteda", "wordcloud", "RColorBrewer", "dplyr", "shiny"))
# install.packages("htmltools")

###############################################################################
#                             Script CHD                                      #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
#                     version : 01-08-2025                                    #
#                                                                             #
#      1.Aide au paramétrage dans R de la CHD sur le corpus                   #
#      2.Extrait chi2, rainette_explor                                        #
#      3.Génère nuages de mots par classe                                     #
#      4.Exporte les segments de texte par classe au format text              #
#      5.Creation d'un concordancier au format html                           #
#      6.Reconstruction du concordancier avec me corpus même avec la lemma    #
#      6.Affichage de la CHD avec rainette_explor (navigateur)                #
#                                                                             #
###############################################################################


# Chargement des bibliothèques nécessaires
library(rainette)
library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(htmltools)
library(udpipe)
library(quanteda.textstats)


# Forcer le répertoire de travail pour assurer les bons chemins d’export
setwd("/Users/stephanemeurisse/Documents/Cours ISTHIA/Cours ISTHIA 2026/Cours_Analyse_textuelle/surtourismepsychiatrie_corpus/")
cat("Répertoire de travail défini :", getwd(), "\n")

# Chemin du modèle UDPipe français
modele_udpipe <- "french-gsd-ud-2.5-191206.udpipe"
if (!file.exists(modele_udpipe)) {
  cat("Téléchargement du modèle UDPipe français...\n")
  udpipe_download_model(language = "french", model_dir = ".", overwrite = FALSE)
}
ud_model <- udpipe_load_model(modele_udpipe)

#########################################################
# PARAMÈTRES UTILISATEUR (modifiable par l'utilisateur)
#########################################################

# Choix du mode de découpage
mode_decoupage <- "segment_size"    # "segment_size" ou "ponctuation"

# Taille des segments avant analyse (nombre de mots par segment)
segment_size <- 40

# Nom du fichier texte brut (dans base_dir)
texte_fichier <- "youtubeprovencetourisme.txt"

# Répertoire principal contenant le fichier texte
base_dir <- "/Users/stephanemeurisse/Documents/Cours ISTHIA/Cours ISTHIA 2026/Cours_Analyse_textuelle/surtourismepsychiatrie_corpus/"

# Nombre de classes (clusters) pour Rainette
k <- 6

# Nombre minimal de segments de texte par classe pour continuer à diviser
# (évite de créer des classes trop petites)
min_split_segments <- 12

# Seuil minimal de fréquence documentaire dans le DFM
# Exemple : si min_docfreq <- 2 seuls les mots qui apparaissent dans au moins 2 segments 
# (c’est-à-dire présents dans 2 segments différents du corpus) sont conservés dans la matrice DFM.
# Les mots présents dans seulement 1 segment sont retirés
min_docfreq <- 1

# Nombre maximal de mots affichés par classe dans les nuages de mots
top_n <- 20

# Répertoire où exporter les résultats
export_dir <- file.path(base_dir, "exports")
dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)

# lemmatisation
lemmatisation <- TRUE   # TRUE pour activer la lemmatisation, FALSE sinon


# Liste des formes (UPOS) à conserver lors de la lemmatisation
# Exemple de valeurs : "NOUN" = nom, "ADJ" = adjectif, "VERB" = verbe, "PROPN" = nom propre, "ADV" = adverbe, etc.
# Liste complète UDPipe (UPOS) : "ADJ" (adjectif), "ADP" (adposition), "ADV" (adverbe), "AUX" (auxiliaire),
# "CCONJ" (conjonction de coordination), "DET" (déterminant), "INTJ" (interjection), "NOUN" (nom), 
# "NUM" (numéral), "PART" (particule), "PRON" (pronom), "PROPN" (nom propre), "PUNCT" (ponctuation), 
# "SCONJ" (conjonction de subordination), "SYM" (symbole), "VERB" (verbe), "X" (autre, inconnu)

upos_a_conserver <- c("NOUN", "ADJ")  # Modifier ici selon vos besoins

#########################################################
# Chargement du corpus
#########################################################
chemin_fichier <- file.path(base_dir, texte_fichier)
corpus_brut <- import_corpus_iramuteq(chemin_fichier)
cat("Nombre de documents importés :", ndoc(corpus_brut), "\n")

#########################################################
# Découpage du corpus en segments
#########################################################
if (mode_decoupage == "segment_size") {
  cat("Découpage par taille fixe de segments (", segment_size, " mots)...\n")
  corpus <- split_segments(corpus_brut, segment_size = segment_size)
  cat("Segments créés (mode segment_size) :\n")
  print(data.frame(segment = docnames(corpus), text = as.character(corpus)))
} else if (mode_decoupage == "ponctuation") {
  cat("Découpage par ponctuation (phrases)...\n")
  textes <- as.character(corpus_brut)
  noms_docs <- docnames(corpus_brut)
  phrases_list <- lapply(seq_along(textes), function(i) {
    doc_id <- noms_docs[i]
    phrases <- tokenize_sentences(textes[i])[[1]]
    phrases <- phrases[nchar(phrases) > 0]
    names(phrases) <- paste0(doc_id, "_phrase", seq_along(phrases))
    return(phrases)
  })
  phrases_all <- unlist(phrases_list)
  corpus <- corpus(phrases_all)
  cat("Segments créés (mode ponctuation) :\n")
  print(data.frame(segment = docnames(corpus), text = as.character(corpus)))
} else {
  stop("Mode de découpage invalide, choisir 'segment_size' ou 'ponctuation'.")
}
cat("Nombre de segments créés :", ndoc(corpus), "\n")

#########################################################
# Téléchargement et chargement du modèle UDPipe
#########################################################
modele_udpipe <- "french-gsd-ud-2.5-191206.udpipe"
if (!file.exists(modele_udpipe)) {
  cat("Téléchargement du modèle UDPipe français...\n")
  udpipe_download_model(language = "french", model_dir = ".", overwrite = FALSE)
}
ud_model <- udpipe_load_model(modele_udpipe)

#########################################################
# Lemmatisation (optionnelle) avec UDPipe
#########################################################
if (lemmatisation) {
  cat("Lemmatisation en cours...\n")
  annotation_list <- lapply(seq_along(corpus), function(i) {
    cat("  Segment", i, "/", ndoc(corpus), "\n")
    res <- udpipe_annotate(ud_model, x = as.character(corpus[i]))
    df <- as.data.frame(res)
    df$doc_id <- docnames(corpus)[i]
    return(df)
  })
  annotation_df <- do.call(rbind, annotation_list)
  # Filtrer selon les formes choisies par l'utilisateur
  annotation_df <- annotation_df %>%
    filter(upos %in% upos_a_conserver, !is.na(lemma), lemma != "", lemma != " ")
  cat("Lemmatisation - lemmes retenus :", nrow(annotation_df), "\n")
  # Reconstruction du texte lemmatisé par segment
  textes_lemmat <- annotation_df %>%
    group_by(doc_id) %>%
    summarise(text = paste(lemma, collapse = " ")) %>%
    ungroup()
  cat("Segments lemmatisés :", nrow(textes_lemmat), "\n")
  cat("Exemples de segments lemmatisés :\n")
  print(head(textes_lemmat, 10))
  # Création du corpus lemmatisé
  corpus_lemmat <- corpus(textes_lemmat$text, docnames = textes_lemmat$doc_id)
} else {
  cat("Lemmatisation désactivée, travail sur texte brut.\n")
  textes_lemmat <- data.frame(doc_id = docnames(corpus), text = as.character(corpus))
  corpus_lemmat <- corpus(as.character(corpus), docnames = docnames(corpus))
}

docvars(corpus_lemmat, "doc_id") <- docnames(corpus_lemmat)
docvars(corpus_lemmat, "orig_doc_id") <- gsub("_.*", "", docnames(corpus_lemmat))


#########################################################
# Prétraitement du texte et création du DFM
#########################################################
cat("Préparation de la DFM...\n")
tok <- tokens(corpus_lemmat, remove_punct = TRUE, remove_numbers = TRUE)
tok <- tokens_remove(tok, stopwords("fr"))
tok <- tokens_split(tok, "'") # supprime le '
tok <- tokens_remove(tok, pattern = c("\\b[a-zA-Z]\\b", "^[^a-zA-Z]+$"), valuetype = "regex")
tok <- tokens_tolower(tok)
dfm <- dfm(tok)
dfm <- dfm_trim(dfm, min_docfreq = min_docfreq)

cat("Dimensions DFM après trim :", dim(dfm), "\n")
# Statistiques sur la longueur des segments (informations seulement)
longueurs <- ntoken(dfm)
cat("Analyse des longueurs des segments après nettoyage :\n")
print(summary(longueurs))
cat("Nombre de segments vides :", sum(longueurs == 0), "\n")
cat("Nombre de segments < 5 mots :", sum(longueurs < 5), "\n")

# On ne garde que les segments non vides pour la suite
included_segments <- docnames(dfm)

### Test corpus reconstitué
#Corpus original correspondant aux segments conservés (sans lemmatisation ni suppression de stopwords)
corpus_affichage <- corpus[included_segments]
### fin

filtered_corpus <- corpus_lemmat[included_segments]
docvars(filtered_corpus, "orig_doc_id") <- gsub("_.*", "", docnames(filtered_corpus))


#########################################################
# Classification hiérarchique descendante (CHD)
#########################################################
cat("Clustering...\n")
# Rainette n’accepte pas de valeur de min_split_members inférieure à 3.
# On force donc la valeur minimale à 3 pour éviter les erreurs.
res <- rainette(dfm, k = k, min_segment_size = 0, min_split_members = max(3, min_split_segments))
docvars(filtered_corpus)$Classes <- res$group
cat("Répartition classes :\n")
print(table(docvars(filtered_corpus)$Classes))


#########################################################
# Extraction et export des segments par classe
#########################################################
# segments_by_class <- split(as.character(filtered_corpus), docvars(filtered_corpus)$Classes)
# Ne garder que les segments contenant au moins un terme discriminant
segments_by_class <- lapply(split(as.character(corpus_affichage), docvars(filtered_corpus)$Classes), function(segments) {
  segments[sapply(segments, function(segment) {
    any(sapply(highlight_terms, function(term) grepl(paste0("\\b", term, "\\b"), segment, ignore.case = TRUE)))
  })]
})

segments_file <- file.path(export_dir, "segments_par_classe.txt")
writeLines(
  unlist(lapply(names(segments_by_class), function(cl) {
    c(paste0("Classe ", cl, ":"), segments_by_class[[cl]], "")
  })), 
  segments_file
)
cat("Segments par classe exportés dans :", segments_file, "\n")

#########################################################
# Récupération des termes discriminants (chi²)
#########################################################
res_stats_list <- rainette_stats(
  dtm = dfm,
  groups = docvars(filtered_corpus)$Classes,
  measure = c("chi2"),
  n_terms = 9999,
  show_negative = TRUE,
  max_p = 0.05
)
res_stats_df <- bind_rows(res_stats_list, .id = "Classe")
highlight_terms <- res_stats_df %>%
  filter(p <= 0.05, nchar(feature) >= 3) %>%
  group_by(Classe) %>%
  arrange(desc(chi2)) %>%
  slice_head(n = top_n) %>%
  pull(feature) %>%
  unique()

#########################################################
# Export HTML avec surlignage
#########################################################
html_file <- file.path(export_dir, "segments_par_classe.html")
highlight_text_html <- function(text, terms, start_tag, end_tag) {
  for (term in terms) {
    escaped_term <- gsub("([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|])", "\\\\\\1", term)
    text <- gsub(paste0("\\b", escaped_term, "\\b"), paste0(start_tag, term, end_tag), text, ignore.case = TRUE)
  }
  return(text)
}
if (file.exists(html_file)) file.remove(html_file)
cat("<html><head><style>body { font-family: Arial; } span.highlight { background-color: yellow; }</style></head><body>\n",
    file = html_file, append = TRUE)
cat("<h1>Segments par Classe (avec surlignage des termes discriminants)</h1>\n", file = html_file, append = TRUE)
for (cl in names(segments_by_class)) {
  cat(paste0("<h2>Classe ", cl, "</h2>\n"), file = html_file, append = TRUE)
  for (segment in segments_by_class[[cl]]) {
    highlighted_segment <- tryCatch(
      highlight_text_html(segment, highlight_terms, "<span class='highlight'>", "</span>"),
      error = function(e) segment
    )
    cat(paste0("<p>", highlighted_segment, "</p>\n"), file = html_file, append = TRUE)
  }
}
cat("</body></html>\n", file = html_file, append = TRUE)
cat("Fichier HTML exporté dans :", html_file, "\n")

#########################################################
# Visualisations : Nuages de mots (chi2 et fréquence)
#########################################################
wordcloud_dir <- file.path(export_dir, "wordclouds")
dir.create(wordcloud_dir, showWarnings = FALSE, recursive = TRUE)

clusters <- sort(unique(docvars(filtered_corpus)$Classes))
for (cl in clusters) {
  # Nuage de mots basé sur le chi2 (déjà présent)
  subset_stats <- subset(res_stats_df, Classe == cl & p <= 0.05)
  subset_stats <- subset_stats[order(-subset_stats$chi2), ]
  if (nrow(subset_stats) > 0 && max(subset_stats$chi2, na.rm = TRUE) > 0) {
    subset_stats <- head(subset_stats, top_n)
    png_filename <- file.path(wordcloud_dir, paste0("cluster_", cl, "_wordcloud_chi2.png"))
    png(png_filename, width = 800, height = 600)
    wordcloud(
      words = subset_stats$feature,
      freq  = subset_stats$chi2,
      scale = c(10, 0.5),
      max.words = top_n,
      colors = brewer.pal(8, "Dark2")
    )
    dev.off()
    cat("Nuage de mots (chi2) pour la classe", cl, "enregistré dans :", png_filename, "\n")
  } else {
    cat("Classe", cl, ": aucun terme significatif pour le nuage de mots (chi2).\n")
  }
  
  # Nuage de mots basé sur la fréquence brute
  segments_cl <- which(docvars(filtered_corpus)$Classes == cl)
  if (length(segments_cl) > 0) {
    dfm_cl <- dfm[segments_cl, ]
    freq_terms <- colSums(dfm_cl)
    freq_terms <- sort(freq_terms, decreasing = TRUE)
    freq_terms <- freq_terms[1:min(top_n, length(freq_terms))]
    png_filename2 <- file.path(wordcloud_dir, paste0("cluster_", cl, "_wordcloud_frequence.png"))
    png(png_filename2, width = 800, height = 600)
    wordcloud(
      words = names(freq_terms),
      freq  = as.numeric(freq_terms),
      scale = c(10, 0.5),
      max.words = top_n,
      colors = brewer.pal(8, "Set2")
    )
    dev.off()
    cat("Nuage de mots (fréquence) pour la classe", cl, "enregistré dans :", png_filename2, "\n")
  }
}


#########################################################
# Export des mots chi² avec les segments correspondants
#########################################################
# Création d'une table mot -> segments (par classe)
mot_segment_liste <- list()

for (cl in clusters) {
  mots_cl <- res_stats_df %>%
    filter(Classe == cl, p <= 0.05, nchar(feature) >= 3) %>%
    arrange(desc(chi2)) %>%
    slice_head(n = top_n) %>%
    pull(feature)
  
  segments_cl <- as.character(segments_by_class[[as.character(cl)]])
  noms_segments_cl <- names(segments_by_class[[as.character(cl)]])
  
  for (mot in mots_cl) {
    segments_contenant_mot <- segments_cl[grepl(paste0("\\b", mot, "\\b"), segments_cl, ignore.case = TRUE)]
    noms_segments <- noms_segments_cl[grepl(paste0("\\b", mot, "\\b"), segments_cl, ignore.case = TRUE)]
    
    if (length(segments_contenant_mot) > 0) {
      mot_segment_liste[[length(mot_segment_liste) + 1]] <- data.frame(
        Mot = mot,
        Classe = cl,
        Segment = segments_contenant_mot,
        Nom_Segment = noms_segments,
        stringsAsFactors = FALSE
      )
    }
  }
}

df_mot_segments <- do.call(rbind, mot_segment_liste)

# Export CSV
csv_mot_segments_path <- file.path(export_dir, "mots_chi2_segments.csv")
write.csv(df_mot_segments, file = csv_mot_segments_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("✅ Export CSV des mots chi² avec les segments associés :", csv_mot_segments_path, "\n")


#########################################################
# Fusion des données chi²/fréquence avec les segments contenant les mots discriminants
#########################################################
# Préparation des données chi2 + fréquence
res_stats_df_clean <- res_stats_df %>%
  rename(Mot = feature, Chi2 = chi2, p_value = p) %>%
  mutate(Mot = as.character(Mot))  # pour éviter les erreurs de fusion

# Fusion des deux tables
donnees_finales <- merge(
  df_mot_segments,
  res_stats_df_clean[, c("Mot", "Classe", "Chi2", "p_value")],
  by = c("Mot", "Classe"),
  all.x = TRUE
)

# Calcul de la fréquence brute pour chaque mot/classe
frequences_par_classe <- lapply(clusters, function(cl) {
  dfm_cl <- dfm[which(docvars(filtered_corpus)$Classes == cl), ]
  freq <- textstat_frequency(dfm_cl)
  freq$Classe <- cl
  return(freq)
})
frequences_df <- do.call(rbind, frequences_par_classe)
frequences_df <- rename(frequences_df, Mot = feature, Frequence = frequency)
frequences_df$Mot <- as.character(frequences_df$Mot)

# Fusion avec les fréquences
donnees_finales <- merge(
  donnees_finales,
  frequences_df[, c("Mot", "Classe", "Frequence")],
  by = c("Mot", "Classe"),
  all.x = TRUE
)

# Tri final : par Classe, puis Chi2 décroissant (ou Mot si tu préfères)
donnees_finales <- donnees_finales %>%
  arrange(as.numeric(Classe), desc(Chi2), Mot)

# Export final trié
csv_fusion_path <- file.path(export_dir, "mots_chi2_frequence_segments.csv")
write.csv(donnees_finales, file = csv_fusion_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("✅ Export CSV final trié par classe avec mots discriminants, chi2, fréquence, segments :", csv_fusion_path, "\n")



#########################################################
# Un fichier csv chi²/fréquence avec les segments par classe
#########################################################

# Répertoire dédié aux fichiers par classe
classe_dir <- file.path(export_dir, "mots_par_classe_csv")
dir.create(classe_dir, showWarnings = FALSE, recursive = TRUE)

# Création d’un fichier CSV par classe
classes_uniques <- sort(unique(donnees_finales$Classe))

for (cl in classes_uniques) {
  data_cl <- donnees_finales %>%
    filter(Classe == cl) %>%
    arrange(desc(Chi2), Mot)
  
  fichier_cl <- file.path(classe_dir, paste0("classe_", cl, "_mots_chi2_segments.csv"))

  
  # === Bloc de DEBUG ===
  cat("\n=== DEBUG EXPORT CSV (mots chi² + segments) ===\n")
  cat("Nombre de lignes dans df_mot_segments :", nrow(df_mot_segments), "\n")
  cat("Colonnes disponibles :", paste(colnames(df_mot_segments), collapse = ", "), "\n")
  cat("Aperçu des premières lignes :\n")
  print(head(df_mot_segments, 5))
  cat("Chemin prévu pour l'export :", csv_mot_segments_path, "\n")
  cat("Répertoire export existe ?", dir.exists(export_dir), "\n")
  cat("Peut-on écrire dans le dossier ? ", file.access(export_dir, 2) == 0, "\n")
  cat("=============================\n\n") #fin debug
  
  
    
  write.csv(data_cl, file = fichier_cl, row.names = FALSE, fileEncoding = "UTF-8")
  cat("✅ Export CSV pour la classe", cl, ":", fichier_cl, "\n")
}

#########################################################
# Affichage interactif rainette
#########################################################
rainette_explor(res, dfm, filtered_corpus)
