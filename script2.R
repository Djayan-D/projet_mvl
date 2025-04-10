#---------- 0. PRÉLIMINAIRES ----------

#----- 0.1. Mettre une seed -----

## Pour assurer la reproductibilité des résultats, on met une seed

set.seed(123)


#----- 0.2. Importer les packages -----

library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(patchwork)
library(GGally)
library(car)
library(pls)
library(EnvStats)
library(tibble)
library(moments)
library(corrplot)
# ATTENTION : VÉRIFIER QUE CARET N'EST PAS CHARGÉ, SINON SOUCIS DE R2 (IL SE CHARGÉ AUTOMATIQUEMENT AU BON MOMENT)





#----- 0.3. Importer les données -----

# Apprentissage

data_train <- read.table("data/UPENN.txt", 
                         header = TRUE,
                         sep = "\t",
                         stringsAsFactors = FALSE,
                         dec = "."
)

head(data_train)


# Test

data_test <- read.table("data/GT.txt", 
                        header = TRUE,
                        sep = "\t",
                        stringsAsFactors = FALSE,
                        dec = "."
)

head(data_test)


# Entier

data <- rbind(data_train,data_test)





#----- 0.4. Définir Y -----

data_train$EnergyTotal <- data_train$HeatTotal + data_train$CoolTotal

data_test$EnergyTotal <- data_test$HeatTotal + data_test$CoolTotal

data$EnergyTotal <- data$HeatTotal + data$CoolTotal





#----- 0.5. Retirer les variables inutiles et déplacer Y -----

## On retire les variables inutiles et on place la variable Y (EnergyTotal) 
## en première colonne

# Entier

data_2 <- data |>
  select(-ID, -HeatJan, -CoolJuly, -HeatTotal, -CoolTotal) |> 
  relocate(EnergyTotal, .before = 1)


# Apprentissage

data_train_2 <- data_train |>
  select(-ID, -HeatJan, -CoolJuly, -HeatTotal, -CoolTotal) |> 
  relocate(EnergyTotal, .before = 1)


# Test

data_test_2 <- data_test |>
  select(-ID, -HeatJan, -CoolJuly, -HeatTotal, -CoolTotal) |> 
  relocate(EnergyTotal, .before = 1)










#---------- 1. EXPLORATION DES DONNÉES ----------

#----- 1.1. Transformation racine carrée -----

# Entier

data_2 <- data_2 |> 
  mutate(EnergyTotal_sqrt = sqrt(EnergyTotal)) |> 
  relocate(EnergyTotal_sqrt, .before = 2)


# Apprentissage

data_train_2 <- data_train_2 |> 
  mutate(EnergyTotal_sqrt = sqrt(EnergyTotal)) |> 
  relocate(EnergyTotal_sqrt, .before = 2)


# Test

data_test_2 <- data_test_2 |> 
  mutate(EnergyTotal_sqrt = sqrt(EnergyTotal)) |> 
  relocate(EnergyTotal_sqrt, .before = 2)


# Définition vecteurs (pour la suite)

Y <- c("EnergyTotal", "EnergyTotal_sqrt")

X <- setdiff(names(data_2), Y)





#----- 1.2. Visualisation des variables à expliquer (EnergyTotal et EnergyTotal_sqrt) -----

#--- 1.2.1. Sans racine carrée ---

# Paramètres des graphiques (pour qu'ils soient pareils)

bin_w <- 10
x_limits <- c(0, 300)
y_limits <- c(0, 30)


# Entier

ggplot(data_2, aes(x = EnergyTotal)) +
  geom_histogram(binwidth = bin_w, fill = "darkblue", color = "white") +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(title = "Distribution - Entier", x = "EnergyTotal", y = "Fréquence")


# Apprentissage

ggplot(data_train_2, aes(x = EnergyTotal)) +
  geom_histogram(binwidth = bin_w, fill = "darkblue", color = "white") +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(title = "Distribution - Apprentissage", x = "EnergyTotal", y = "Fréquence")

# Test

ggplot(data_test_2, aes(x = EnergyTotal)) +
  geom_histogram(binwidth = bin_w, fill = "darkblue", color = "white") +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(title = "Distribution - Test", x = "EnergyTotal", y = "Fréquence")





#--- 1.2.2. Avec racine carrée ---

# Paramètres des graphiques

bin_w <- 1
x_limits <- c(0, 20)
y_limits <- c(0, 20)


# Entier

ggplot(data_2, aes(x = EnergyTotal_sqrt)) +
  geom_histogram(binwidth = bin_w, fill = "darkblue", color = "white") +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(title = "Distribution - Entier", x = "sqrt(EnergyTotal)", y = "Fréquence")


# Apprentissage

ggplot(data_train_2, aes(x = EnergyTotal_sqrt)) +
  geom_histogram(binwidth = bin_w, fill = "darkblue", color = "white") +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(title = "Distribution - Apprentissage", x = "sqrt(EnergyTotal)", y = "Fréquence")

# Test

ggplot(data_test_2, aes(x = EnergyTotal_sqrt)) +
  geom_histogram(binwidth = bin_w, fill = "darkblue", color = "white") +
  coord_cartesian(xlim = x_limits, ylim = y_limits) +
  labs(title = "Distribution - Test", x = "sqrt(EnergyTotal)", y = "Fréquence")





#----- 1.3. Visualisation des variables explicatives -----

#--- 1.3.1. Tracer les graphiques ---

plot_list <- lapply(X, function(var) {
  if (var == "bldg_mass_type") {
    ggplot(data_2, aes(x = factor(.data[[var]]))) +
      geom_bar(fill = "steelblue", color = "white") +
      labs(title = var, x = NULL, y = "Fréquence") +
      theme_minimal()
  } else {
    ggplot(data_2, aes(x = .data[[var]])) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(title = var, x = NULL, y = "Fréquence") +
      theme_minimal()
  }
})



#--- 1.3.2. Représenter les graphiques ---

for (i in seq(1, 28, by = 9)) {
  print(
    wrap_plots(plot_list[i:min(i + 8, 28)], ncol = 3) +
      plot_annotation(title = paste("Variables explicatives", i, "à", min(i + 8, 28)))
  )
}





#----- 1.4. Valeurs atypiques -----

#--- 1.4.1. Sans racine carrée ---

# Entier

ggplot(data_2, aes(y = EnergyTotal)) +
  geom_boxplot(fill = "lightblue", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Distribution de l'énergie totale dans le jeu entier", 
       y = "Énergie totale (EnergyTotal)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

rosnerTest(data_2$EnergyTotal, k = 7)

## 1 Atypique


# Apprentissage

ggplot(data_train_2, aes(y = EnergyTotal)) +
  geom_boxplot(fill = "lightblue", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Distribution de l'énergie totale dans le jeu d'apprentissage", 
       y = "Énergie totale (EnergyTotal)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

rosnerTest(data_2$EnergyTotal, k = 2)

## 1 Atypique


# Test

ggplot(data_test_2, aes(y = EnergyTotal)) +
  geom_boxplot(fill = "lightblue", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Distribution de l'énergie totale dans le jeu de test", 
       y = "Énergie totale (EnergyTotal)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## 0 Atypique


#--- 1.4.2. Avec racine carrée ---

# Entier

ggplot(data_2, aes(y = EnergyTotal_sqrt)) +
  geom_boxplot(fill = "lightblue", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Distribution de l'énergie totale dans le jeu entier", 
       y = "Énergie totale (EnergyTotal_sqrt)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## 0 Atypique


# Apprentissage

ggplot(data_train_2, aes(y = EnergyTotal_sqrt)) +
  geom_boxplot(fill = "lightblue", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Distribution de l'énergie totale dans le jeu d'apprentissage", 
       y = "Énergie totale (EnergyTotal_sqrt)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

rosnerTest(data_2$EnergyTotal_sqrt, k = 1)

## 0 Atypique


# Test

ggplot(data_test_2, aes(y = EnergyTotal_sqrt)) +
  geom_boxplot(fill = "lightblue", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Distribution de l'énergie totale dans le jeu de test", 
       y = "Énergie totale (EnergyTotal_sqrt)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## 0 Atypique





#----- 1.5. Statistiques descriptives ----

stats_desc <- function(...){
  # Rassembler les jeux de données dans une liste
  data_list <- list(...)
  dataset_names <- as.character(match.call(expand.dots = FALSE)$...)
  
  # Fonction pour calculer les statistiques pour un seul vecteur
  calculate_stats <- function(data) {
    moyenne <- mean(data, na.rm = TRUE)
    ecart_type <- sd(data, na.rm = TRUE)
    asymetrie <- skewness(data, na.rm = TRUE)
    aplatissement <- kurtosis(data, na.rm = TRUE)
    shapiro_test <- shapiro.test(data)$p.value
    quantiles <- quantile(data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    
    return(c(
      "Moyenne" = moyenne,
      "Écart-type" = ecart_type,
      "Skewness" = asymetrie,
      "Kurtosis" = aplatissement,
      "p-value Shapiro-Wilk" = shapiro_test,
      "Min" = quantiles[1],
      "1er Quartile (Q1)" = quantiles[2],
      "Médiane (Q2)" = quantiles[3],
      "3e Quartile (Q3)" = quantiles[4],
      "Max" = quantiles[5]
    ))
  }
  
  # Appliquer la fonction à chaque dataset
  results <- lapply(data_list, calculate_stats)
  
  # Combiner les résultats en un data.frame avec les noms en colonnes
  results_df <- as.data.frame(results)
  colnames(results_df) <- dataset_names
  
  # Ajouter une colonne pour les noms de statistiques
  results_df <- tibble::rownames_to_column(results_df, var = "Statistique")
  
  return(results_df)
}

stats_desc(data_2$EnergyTotal, 
           data_train_2$EnergyTotal, 
           data_test_2$EnergyTotal,
           data_2$EnergyTotal_sqrt, 
           data_train_2$EnergyTotal_sqrt, 
           data_test_2$EnergyTotal_sqrt)





#----- 1.6. Distribution des résidus -----

#--- 1.6.1. Sans racine ---

mod_lm <- lm(EnergyTotal ~ . -EnergyTotal_sqrt, data = data_2)

res <- residuals(mod_lm)

hist(res, 
     breaks = 30, 
     probability = TRUE,
     main = "Résidus du modèle sans transformation",
     col = "lightblue", 
     xlab = "Résidus")

# Ajouter la courbe de densité normale
curve(dnorm(x, mean = mean(res), sd = sd(res)),
      col = "red", lwd = 2, add = TRUE)

shapiro.test(res)


#--- 1.6.2. Avec racine ---

mod_lm_sqrt <- lm(EnergyTotal_sqrt ~ . -EnergyTotal, data = data_2)

res_sqrt <- residuals(mod_lm_sqrt)

hist(res_sqrt, 
     breaks = 30, 
     probability = TRUE,
     main = "Résidus du modèle avec transformation",
     col = "lightblue", 
     xlab = "Résidus")

# Ajouter la courbe de densité normale
curve(dnorm(x, mean = mean(res_sqrt), sd = sd(res_sqrt)),
      col = "red", lwd = 2, add = TRUE)


shapiro.test(res_sqrt)


#----- 1.7. Matrice de corrélation -----

corrplot::corrplot(cor(data_2), 
         method = "circle",
         type = "upper",
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.5,
         diag = FALSE,
)





#----- 1.8. Variance Inflation Factor (VIF) -----

#--- 1.8.1. Calcul du VIF à partir du modèle linéaire ---

model_vif <- lm(EnergyTotal ~ . - EnergyTotal_sqrt, data = data_2) |> 
  vif()



#--- 1.8.2. Transformer le vecteur VIF en data.frame ---

vif_df <- data.frame(
  Variable = names(model_vif),
  VIF = as.numeric(model_vif)
)



#--- 1.8.3. Création du graphique avec ggplot ---

ggplot(vif_df, aes(x = Variable, y = VIF)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = round(VIF, 2)), vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(x = "", y = "VIF", title = "Variance Inflation Factor (VIF)")





#----- 1.9. Analyse en Composantes Principales (ACP) -----

respca_ent <- PCA(data_2, 
                  scale.unit = TRUE, 
                  quanti.sup = which(names(data_2) %in% Y), 
                  graph = FALSE)
fviz_eig(respca_ent, addlabels = TRUE, cumulative = TRUE)
fviz_pca_var(respca_ent, repel = TRUE)
round(respca_ent$quanti.sup$coord, 2)


# Variance cumulée

eig <- as.data.frame(get_eigenvalue(respca_ent))
eig$Cumul <- cumsum(eig$`variance.percent`)
eig$Dim <- factor(1:nrow(eig))

ggplot(eig, aes(x = Dim, y = Cumul)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = round(Cumul, 1)), vjust = -0.5, size = 3.5) +
  labs(title = "Variance cumulée par composante principale",
       x = "Composante", y = "Variance cumulée (%)") +
  ylim(0, 110) +
  theme_minimal()







#---------- 2. PCR : RÉGRESSION SUR LES COMPOSANTES PRINCIPALES ----------

#----- 2.1. Définir les dimensions -----

n = nrow(data_train_2)         # Nombre d'observations
p = ncol(data_train_2) - 2     # Nombre de prédicteurs (-2 car 2 Y)





#------ 2.2. Appliquer la PCR -----

#--- 2.2.1. Sans racine ---

#- 2.2.1.1. Modéliser -

## La PCR est effectuée avec centrage/réduction (scale=TRUE)

modelpcr <- pcr(EnergyTotal ~ . -EnergyTotal_sqrt,
                data = data_train_2,
                ncomp = min(n - 1, p), 
                scale = TRUE,
                validation = "CV") # On met CV mais on peut le "désactiver" après





#- 2.2.1.2. Regarder les résultats de TRAIN -

# Summary

summary(modelpcr,
        estimate = "train")


# R2

R2(modelpcr,
   estimate = "train")

validationplot(modelpcr,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "train")

# ATTENTION : VÉRIFIER QUE CARET EST CHARGÉ AVANT PLS SINON CONFLITS


# RMSE

RMSEP(modelpcr,
      estimate = "train")

validationplot(modelpcr,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "train")





#- 2.2.1.3. Regarder les résultats de CROSS VALIDATION -

# Summary

summary(modelpcr,
        estimate = "CV")


# R2

R2(modelpcr,
   estimate = "CV")

validationplot(modelpcr,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "CV")


# RMSE

RMSEP(modelpcr,
      estimate = "CV")

validationplot(modelpcr,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "CV")





#- 2.2.1.4. Regarder les résultats de ALL -

## Reprend juste les 2 précédents

# Summary

summary(modelpcr,
        estimate = "all")


# R2

R2(modelpcr,
   estimate = "all")

validationplot(modelpcr,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "all")

# RMSE

RMSEP(modelpcr,
      estimate = "all")

validationplot(modelpcr,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "all")





#--- 2.2.2. Avec racine ---

#- 2.2.2.1. Modéliser -

modelpcr_sqrt <- pcr(EnergyTotal_sqrt ~ . -EnergyTotal,
                     data = data_train_2,
                     ncomp = min(n - 1, p), 
                     scale = TRUE,
                     validation = "CV")



#- 2.2.1.2. Regarder les résultats de TRAIN -

# Summary

summary(modelpcr_sqrt,
        estimate = "train")


# R2

R2(modelpcr_sqrt,
   estimate = "train")

validationplot(modelpcr_sqrt,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "train")


# RMSE

RMSEP(modelpcr_sqrt,
      estimate = "train")

validationplot(modelpcr,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "train")





#- 2.2.1.3. Regarder les résultats de CROSS VALIDATION -

# Summary

summary(modelpcr_sqrt,
        estimate = "CV")


# R2

R2(modelpcr_sqrt,
   estimate = "CV")

validationplot(modelpcr_sqrt,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "CV")


# RMSE

RMSEP(modelpcr_sqrt,
      estimate = "CV")

validationplot(modelpcr_sqrt,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "CV")





#- 2.2.1.4. Regarder les résultats de ALL -

# Summary

summary(modelpcr_sqrt,
        estimate = "all")


# R2

R2(modelpcr_sqrt,
   estimate = "all")

validationplot(modelpcr_sqrt,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "all")

# RMSE

RMSEP(modelpcr_sqrt,
      estimate = "all")

validationplot(modelpcr_sqrt,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "all")





#----- 2.3. Comparaison avec une régression linéaire classique -----

#--- 2.3.1. Sans racine ---

summary(lm(EnergyTotal ~ . -EnergyTotal_sqrt, data = data_train_2))




#--- 2.3.2. Avec racine ---

summary(lm(EnergyTotal_sqrt ~ . -EnergyTotal, data = data_train_2))





#----- 2.4. Prédiction sur le jeu test -----

#--- 2.4.1. Sans racine ---

#- 2.4.1.1. Sélectionner le nombre de composantes optimal (selon CV) -

k_opt_pcr <- selectNcomp(modelpcr, method = "onesigma", plot = TRUE) 

## 8 composantes



#- 2.4.1.2. Prédiction sur le jeu test -

pred_pcr_test <- predict(modelpcr, 
                         newdata = data_test_2,
                         ncomp = k_opt_pcr)


#- 2.4.1.3 R² sur test -

# R² global

1 - sum((data_test_2$EnergyTotal  - pred_pcr_test)^2) / sum((data_test_2$EnergyTotal  - mean(data_test_2$EnergyTotal ))^2)


# R² par composante

R2_par_comp_test <- sapply(1:k_opt_pcr, function(n) {
  pred <- predict(modelpcr, newdata = data_test_2, ncomp = n)
  1 - sum((data_test_2$EnergyTotal - pred)^2) / sum((data_test_2$EnergyTotal - mean(data_test_2$EnergyTotal))^2)
})

R2_table_test <- data.frame(Composantes = 1:k_opt_pcr, R2 = R2_par_comp_test)
R2_table_test

ggplot(R2_table_test, aes(x = Composantes, y = R2)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "R²", title = "Évolution du R² avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pcr) + 
  theme_minimal()



#- 2.4.1.4 RMSE sur test -

# RMSE global

sqrt(mean((data_test_2$EnergyTotal - pred_pcr_test)^2))


# RMSE par composante

RMSEP_par_comp_test <- sapply(1:k_opt_pcr, function(n) {
  pred <- predict(modelpcr, newdata = data_test_2, ncomp = n)
  sqrt(mean((data_test_2$EnergyTotal - pred)^2))
})

RMSEP_table_test <- data.frame(Composantes = 1:k_opt_pcr, RMSEP = RMSEP_par_comp_test)
RMSEP_table_test

ggplot(RMSEP_table_test, aes(x = Composantes, y = RMSEP)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "RMSEP", title = "Évolution du RMSEP avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pcr) + 
  theme_minimal()



#- 2.4.1.5 Réel vs Prévisions sur test -

# Créer le dataframe de comparaison

comparison_data <- data.frame(
  Vraies_Valeurs = data_test_2$EnergyTotal,
  Valeurs_Predites = as.vector(pred_pcr_test)
)


# Déterminer les limites actuelles des axes

x_min <- min(comparison_data$Vraies_Valeurs, na.rm = TRUE)
x_max <- max(comparison_data$Vraies_Valeurs, na.rm = TRUE)
y_min <- min(comparison_data$Valeurs_Predites, na.rm = TRUE)
y_max <- max(comparison_data$Valeurs_Predites, na.rm = TRUE)


# Créer un dataframe pour la ligne diagonale x = y

line_data <- data.frame(
  Vraies_Valeurs = c(min(x_min, y_min), max(x_max, y_max)),
  Valeurs_Predites = c(min(x_min, y_min), max(x_max, y_max))
)


# Tracer le graphique avec geom_line pour la ligne x = y

ggplot(comparison_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites)) +
  geom_point(color = "blue") +
  geom_line(data = line_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites), color = "red", linetype = "dashed") +
  labs(x = "Valeurs Réelles", y = "Valeurs Prédites", title = "Comparaison entre les Valeurs Prédites et Réelles PCR") +
  theme_minimal() +
  coord_fixed(ratio = 1)



#- 2.4.1.6. Prédiction sur le jeu d'apprentissage -

pred_pcr_train <- predict(modelpcr, 
                         newdata = data_train_2,
                         ncomp = k_opt_pcr)

## Je refais tous les trucs qui suivent, mais jusqu'au 2.4.1.9. pour garder la même forme  et vérifier les résultats
## mais en soi on peut déjà retrouver ça en haut dans le R2 train et juste regarder jusqu'à la 14e composante


#- 2.4.1.7. R² sur train -

# R² global

1 - sum((data_train_2$EnergyTotal  - pred_pcr_train)^2) / sum((data_train_2$EnergyTotal  - mean(data_train_2$EnergyTotal ))^2)


# R² par composante

R2_par_comp_train <- sapply(1:k_opt_pcr, function(n) {
  pred <- predict(modelpcr, newdata = data_train_2, ncomp = n)
  1 - sum((data_train_2$EnergyTotal - pred)^2) / sum((data_train_2$EnergyTotal - mean(data_train_2$EnergyTotal))^2)
})

R2_table_train <- data.frame(Composantes = 1:k_opt_pcr, R2 = R2_par_comp_train)
R2_table_train

ggplot(R2_table_train, aes(x = Composantes, y = R2)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "R²", title = "Évolution du R² avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pcr) + 
  theme_minimal()



#- 2.4.1.8. RMSE sur train -

# RMSE global

sqrt(mean((data_train_2$EnergyTotal - pred_pcr_train)^2))


# RMSE par composante

RMSEP_par_comp_train <- sapply(1:k_opt_pcr, function(n) {
  pred <- predict(modelpcr, newdata = data_train_2, ncomp = n)
  sqrt(mean((data_train_2$EnergyTotal - pred)^2))
})

RMSEP_table_train <- data.frame(Composantes = 1:k_opt_pcr, RMSEP = RMSEP_par_comp_train)
RMSEP_table_train

ggplot(RMSEP_table_train, aes(x = Composantes, y = RMSEP)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "RMSEP", title = "Évolution du RMSEP avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pcr) + 
  theme_minimal()



#- 2.4.1.9. Réel vs Prévisions sur train -

# Créer le dataframe de comparaison

comparison_data <- data.frame(
  Vraies_Valeurs = data_train_2$EnergyTotal,
  Valeurs_Predites = as.vector(pred_pcr_train)
)


# Déterminer les limites actuelles des axes

x_min <- min(comparison_data$Vraies_Valeurs, na.rm = TRUE)
x_max <- max(comparison_data$Vraies_Valeurs, na.rm = TRUE)
y_min <- min(comparison_data$Valeurs_Predites, na.rm = TRUE)
y_max <- max(comparison_data$Valeurs_Predites, na.rm = TRUE)


# Créer un dataframe pour la ligne diagonale x = y

line_data <- data.frame(
  Vraies_Valeurs = c(min(x_min, y_min), max(x_max, y_max)),
  Valeurs_Predites = c(min(x_min, y_min), max(x_max, y_max))
)


# Tracer le graphique avec geom_line pour la ligne x = y

ggplot(comparison_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites)) +
  geom_point(color = "blue") +
  geom_line(data = line_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites), color = "red", linetype = "dashed") +
  labs(x = "Valeurs Réelles", y = "Valeurs Prédites", title = "Comparaison entre les Valeurs Prédites et Réelles PCR") +
  theme_minimal() +
  coord_fixed(ratio = 1)





#--- 2.4.2. Avec racine

#- 2.4.2.1. Sélectionner le nombre de composantes optimal (selon CV) -

k_opt_pcr_sqrt <- selectNcomp(modelpcr_sqrt, method = "onesigma", plot = TRUE) 

## 14 composantes



#- 2.4.2.2. Prédiction sur le jeu test -

pred_pcr_test_sqrt <- predict(modelpcr_sqrt, 
                              newdata = data_test_2,
                              ncomp = k_opt_pcr_sqrt)


#- 2.4.2.3 R² sur test -

# R² global

1 - sum((data_test_2$EnergyTotal_sqrt  - pred_pcr_test_sqrt)^2) / sum((data_test_2$EnergyTotal_sqrt  - mean(data_test_2$EnergyTotal_sqrt ))^2)


# R² par composante

R2_par_comp_test_sqrt <- sapply(1:k_opt_pcr_sqrt, function(n) {
  pred <- predict(modelpcr_sqrt, newdata = data_test_2, ncomp = n)
  1 - sum((data_test_2$EnergyTotal_sqrt - pred)^2) / sum((data_test_2$EnergyTotal_sqrt - mean(data_test_2$EnergyTotal_sqrt))^2)
})

R2_table_test_sqrt <- data.frame(Composantes = 1:k_opt_pcr_sqrt, R2 = R2_par_comp_test_sqrt)
R2_table_test_sqrt

ggplot(R2_table_test_sqrt, aes(x = Composantes, y = R2)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "R²", title = "Évolution du R² avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pcr_sqrt) + 
  theme_minimal()



#- 2.4.2.4. RMSE sur test -

# RMSE global

sqrt(mean((data_test_2$EnergyTotal_sqrt - pred_pcr_test_sqrt)^2))


# RMSE par composante

RMSEP_par_comp_test_sqrt <- sapply(1:k_opt_pcr_sqrt, function(n) {
  pred <- predict(modelpcr_sqrt, newdata = data_test_2, ncomp = n)
  sqrt(mean((data_test_2$EnergyTotal_sqrt - pred)^2))
})

RMSEP_table_test_sqrt <- data.frame(Composantes = 1:k_opt_pcr_sqrt, RMSEP = RMSEP_par_comp_test_sqrt)
RMSEP_table_test_sqrt

ggplot(RMSEP_table_test_sqrt, aes(x = Composantes, y = RMSEP)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "RMSEP", title = "Évolution du RMSEP avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pcr_sqrt) + 
  theme_minimal()



#- 2.4.2.5. RMSE sur test -

# Créer le dataframe de comparaison

comparison_data <- data.frame(
  Vraies_Valeurs = data_test_2$EnergyTotal_sqrt,
  Valeurs_Predites = as.vector(pred_pcr_test_sqrt)
)


# Déterminer les limites actuelles des axes

x_min <- min(comparison_data$Vraies_Valeurs, na.rm = TRUE)
x_max <- max(comparison_data$Vraies_Valeurs, na.rm = TRUE)
y_min <- min(comparison_data$Valeurs_Predites, na.rm = TRUE)
y_max <- max(comparison_data$Valeurs_Predites, na.rm = TRUE)


# Créer un dataframe pour la ligne diagonale x = y

line_data <- data.frame(
  Vraies_Valeurs = c(min(x_min, y_min), max(x_max, y_max)),
  Valeurs_Predites = c(min(x_min, y_min), max(x_max, y_max))
)


# Tracer le graphique avec geom_line pour la ligne x = y

ggplot(comparison_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites)) +
  geom_point(color = "blue") +
  geom_line(data = line_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites), color = "red", linetype = "dashed") +
  labs(x = "Valeurs Réelles", y = "Valeurs Prédites", title = "Comparaison entre les Valeurs Prédites et Réelles PCR sqrt") +
  theme_minimal() +
  coord_fixed(ratio = 1)



#- 2.4.2.6. Prédiction sur le jeu d'apprentissage -

pred_pcr_train_sqrt <- predict(modelpcr_sqrt, 
                          newdata = data_train_2,
                          ncomp = k_opt_pcr_sqrt)


#- 2.4.2.7. R² sur train -

# R² global

1 - sum((data_train_2$EnergyTotal_sqrt  - pred_pcr_train_sqrt)^2) / sum((data_train_2$EnergyTotal_sqrt  - mean(data_train_2$EnergyTotal_sqrt ))^2)


# R² par composante

R2_par_comp_train_sqrt <- sapply(1:k_opt_pcr_sqrt, function(n) {
  pred <- predict(modelpcr_sqrt, newdata = data_train_2, ncomp = n)
  1 - sum((data_train_2$EnergyTotal_sqrt - pred)^2) / sum((data_train_2$EnergyTotal_sqrt - mean(data_train_2$EnergyTotal_sqrt))^2)
})

R2_table_train_sqrt <- data.frame(Composantes = 1:k_opt_pcr_sqrt, R2 = R2_par_comp_train_sqrt)
R2_table_train_sqrt

ggplot(R2_table_train_sqrt, aes(x = Composantes, y = R2)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "R²", title = "Évolution du R² avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pcr_sqrt) + 
  theme_minimal()



#- 2.4.2.8. RMSE sur train -

# RMSE global

sqrt(mean((data_train_2$EnergyTotal_sqrt - pred_pcr_train_sqrt)^2))


# RMSE par composante

RMSEP_par_comp_train_sqrt <- sapply(1:k_opt_pcr_sqrt, function(n) {
  pred <- predict(modelpcr_sqrt, newdata = data_train_2, ncomp = n)
  sqrt(mean((data_train_2$EnergyTotal_sqrt - pred)^2))
})

RMSEP_table_train_sqrt <- data.frame(Composantes = 1:k_opt_pcr_sqrt, RMSEP = RMSEP_par_comp_train_sqrt)
RMSEP_table_train_sqrt

ggplot(RMSEP_table_train_sqrt, aes(x = Composantes, y = RMSEP)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "RMSEP", title = "Évolution du RMSEP avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pcr_sqrt) + 
  theme_minimal()



#- 2.4.2.9. Réel vs Prévisions sur train -

# Créer le dataframe de comparaison

comparison_data <- data.frame(
  Vraies_Valeurs = data_train_2$EnergyTotal_sqrt,
  Valeurs_Predites = as.vector(pred_pcr_train_sqrt)
)


# Déterminer les limites actuelles des axes

x_min <- min(comparison_data$Vraies_Valeurs, na.rm = TRUE)
x_max <- max(comparison_data$Vraies_Valeurs, na.rm = TRUE)
y_min <- min(comparison_data$Valeurs_Predites, na.rm = TRUE)
y_max <- max(comparison_data$Valeurs_Predites, na.rm = TRUE)


# Créer un dataframe pour la ligne diagonale x = y

line_data <- data.frame(
  Vraies_Valeurs = c(min(x_min, y_min), max(x_max, y_max)),
  Valeurs_Predites = c(min(x_min, y_min), max(x_max, y_max))
)


# Tracer le graphique avec geom_line pour la ligne x = y

ggplot(comparison_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites)) +
  geom_point(color = "blue") +
  geom_line(data = line_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites), color = "red", linetype = "dashed") +
  labs(x = "Valeurs Réelles", y = "Valeurs Prédites", title = "Comparaison entre les Valeurs Prédites et Réelles PCR sqrt") +
  theme_minimal() +
  coord_fixed(ratio = 1)





#- 2.4.2.10. Prédiction sur le jeu de test avec lm -

library(caret)

# Sans racine - Test

mod_lm <- lm(EnergyTotal ~ . -EnergyTotal_sqrt, data = data_train_2)

predictions <- predict(mod_lm, newdata = data_test_2)

r2_test_lm <- 1 - sum((data_test_2$EnergyTotal - predictions)^2) / sum((data_test_2$EnergyTotal - mean(data_test_2$EnergyTotal))^2)
r2_test_lm

rmsep_lm <- sqrt(mean((data_test_2$EnergyTotal - predictions)^2))
rmsep_lm


# Sans racine - CV

ctrl <- trainControl(method = "cv", number = 10)

mod_cv_lm <- train(EnergyTotal ~ . -EnergyTotal_sqrt,
                   data = data_train_2,
                   method = "lm",
                   trControl = ctrl)

mod_cv_lm$results


# Avec racine - Test

mod_lm_sqrt <- lm(EnergyTotal_sqrt ~ . -EnergyTotal, data = data_train_2)

predictions_sqrt <- predict(mod_lm_sqrt, newdata = data_test_2)

r2_test_lm_sqrt <- 1 - sum((data_test_2$EnergyTotal_sqrt - predictions_sqrt)^2) / sum((data_test_2$EnergyTotal_sqrt - mean(data_test_2$EnergyTotal_sqrt))^2)
r2_test_lm_sqrt

rmsep_lm_sqrt <- sqrt(mean((data_test_2$EnergyTotal_sqrt - predictions_sqrt)^2))
rmsep_lm_sqrt


# Avec racine - CV

mod_cv_lm_sqrt <- train(EnergyTotal_sqrt ~ . -EnergyTotal,
                        data = data_train_2,
                        method = "lm",
                        trControl = ctrl)

mod_cv_lm_sqrt$results


detach("package:caret", unload = TRUE)





#---- 2.5. Conclusion PCR -----










#---------- 3. PLS : RÉGRESSION PAR MOINDRES CARRÉS PARTIELS ----------

#------ 3.1. Appliquer la PLS -----

#--- 3.1.1. Sans racine ---

#- 3.1.1.1. Modéliser -

## La PCR est effectuée avec centrage/réduction (scale=TRUE)

modelpls <- plsr(EnergyTotal ~ . -EnergyTotal_sqrt,
                 data = data_train_2,
                 ncomp = min(n - 1, p), 
                 scale = TRUE,
                 validation = "CV")





#- 3.1.1.2. Regarder les résultats de TRAIN -

# Summary

summary(modelpls,
        estimate = "train")


# R2

R2(modelpls,
   estimate = "train")

validationplot(modelpls,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PLS sans racine",
               estimate = "train")


# RMSE

RMSEP(modelpls,
      estimate = "train")

validationplot(modelpls,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PLS sans racine",
               estimate = "train")





#- 3.1.1.3. Regarder les résultats de CROSS VALIDATION -

# Summary

summary(modelpls,
        estimate = "CV")


# R2

R2(modelpls,
   estimate = "CV")

validationplot(modelpcr,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PLS sans racine",
               estimate = "CV")


# RMSE

RMSEP(modelpls,
      estimate = "CV")

validationplot(modelpls,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "CV")





#- 3.1.1.4. Regarder les résultats de ALL -

## Reprend juste les 2 précédents

# Summary

summary(modelpls,
        estimate = "all")


# R2

R2(modelpls,
   estimate = "all")

validationplot(modelpls,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PLS sans racine",
               estimate = "all")

# RMSE

RMSEP(modelpls,
      estimate = "all")

validationplot(modelpls,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PLS sans racine",
               estimate = "all")





#--- 3.1.2. Avec racine ---

#- 3.1.2.1. Modéliser -

modelpls_sqrt <- plsr(EnergyTotal_sqrt ~ . -EnergyTotal,
                      data = data_train_2,
                      ncomp = min(n - 1, p), 
                      scale = TRUE,
                      validation = "CV")





#- 3.1.2.2. Regarder les résultats de TRAIN -

# Summary

summary(modelpls_sqrt,
        estimate = "train")


# R2

R2(modelpls_sqrt,
   estimate = "train")

validationplot(modelpls_sqrt,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "train")


# RMSE

RMSEP(modelpls_sqrt,
      estimate = "train")

validationplot(modelpls_sqrt,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "train")





#- 3.1.2.3. Regarder les résultats de CROSS VALIDATION -

# Summary

summary(modelpls_sqrt,
        estimate = "CV")


# R2

R2(modelpls_sqrt,
   estimate = "CV")

validationplot(modelpls_sqrt,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "CV")


# RMSE

RMSEP(modelpls_sqrt,
      estimate = "CV")

validationplot(modelpls_sqrt,
               val.type="RMSEP",
               type="b",
               main = "Courbe de RMSEP selon le nombre de composantes - PCR sans racine",
               estimate = "CV")





#- 3.1.2.4. Regarder les résultats de ALL -

# Summary

summary(modelpls_sqrt,
        estimate = "all")


# R2

R2(modelpls_sqrt,
   estimate = "all")

validationplot(modelpls_sqrt,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "all")

# RMSE

RMSEP(modelpls_sqrt,
      estimate = "all")

validationplot(modelpls_sqrt,
               val.type="RMSEP",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine",
               estimate = "all")





#----- 3.2. Comparaison avec une régression linéaire classique -----

#--- 3.2.1. Sans racine ---

summary(lm(EnergyTotal ~ . -EnergyTotal_sqrt, data = data_train_2))



#--- 3.2.2. Avec racine ---

summary(lm(EnergyTotal_sqrt ~ . -EnergyTotal, data = data_train_2))





#----- 3.3. Prédiction sur le jeu test -----

#--- 3.3.1. Sans racine ---

#- 3.3.1.1. Sélectionner le nombre de composantes optimal (selon CV) -

k_opt_pls <- selectNcomp(modelpls, method = "onesigma", plot = TRUE) 

## 2 composantes



#- 2.4.1.2. Prédiction sur le jeu test -

pred_pls_test <- predict(modelpls, 
                         newdata = data_test_2,
                         ncomp = k_opt_pls)


#- 2.4.1.3 R² sur test -

# R² global

1 - sum((data_test_2$EnergyTotal  - pred_pls_test)^2) / sum((data_test_2$EnergyTotal  - mean(data_test_2$EnergyTotal ))^2)


# R² par composante

R2_par_comp_test <- sapply(1:k_opt_pls, function(n) {
  pred <- predict(modelpls, newdata = data_test_2, ncomp = n)
  1 - sum((data_test_2$EnergyTotal - pred)^2) / sum((data_test_2$EnergyTotal - mean(data_test_2$EnergyTotal))^2)
})

R2_table_test <- data.frame(Composantes = 1:k_opt_pls, R2 = R2_par_comp_test)
R2_table_test

ggplot(R2_table_test, aes(x = Composantes, y = R2)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "R²", title = "Évolution du R² avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pls) + 
  theme_minimal()



#- 2.4.1.4 RMSE sur test -

# RMSE global

sqrt(mean((data_test_2$EnergyTotal - pred_pls_test)^2))


# RMSE par composante

RMSEP_par_comp_test <- sapply(1:k_opt_pls, function(n) {
  pred <- predict(modelpls, newdata = data_test_2, ncomp = n)
  sqrt(mean((data_test_2$EnergyTotal - pred)^2))
})

RMSEP_table_test <- data.frame(Composantes = 1:k_opt_pls, RMSEP = RMSEP_par_comp_test)
RMSEP_table_test

ggplot(RMSEP_table_test, aes(x = Composantes, y = RMSEP)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "RMSEP", title = "Évolution du RMSEP avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pls) + 
  theme_minimal()



#- 2.4.1.5 Réel vs Prévisions sur test -

# Créer le dataframe de comparaison

comparison_data <- data.frame(
  Vraies_Valeurs = data_test_2$EnergyTotal,
  Valeurs_Predites = as.vector(pred_pls_test)
)


# Déterminer les limites actuelles des axes

x_min <- min(comparison_data$Vraies_Valeurs, na.rm = TRUE)
x_max <- max(comparison_data$Vraies_Valeurs, na.rm = TRUE)
y_min <- min(comparison_data$Valeurs_Predites, na.rm = TRUE)
y_max <- max(comparison_data$Valeurs_Predites, na.rm = TRUE)


# Créer un dataframe pour la ligne diagonale x = y

line_data <- data.frame(
  Vraies_Valeurs = c(min(x_min, y_min), max(x_max, y_max)),
  Valeurs_Predites = c(min(x_min, y_min), max(x_max, y_max))
)


# Tracer le graphique avec geom_line pour la ligne x = y

ggplot(comparison_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites)) +
  geom_point(color = "blue") +
  geom_line(data = line_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites), color = "red", linetype = "dashed") +
  labs(x = "Valeurs Réelles", y = "Valeurs Prédites", title = "Comparaison entre les Valeurs Prédites et Réelles PLS") +
  theme_minimal() +
  coord_fixed(ratio = 1)



#- 2.4.1.6. Prédiction sur le jeu d'apprentissage -

pred_pls_train <- predict(modelpls, 
                          newdata = data_train_2,
                          ncomp = k_opt_pls)



#- 2.4.1.7. R² sur train -

# R² global

1 - sum((data_train_2$EnergyTotal  - pred_pls_train)^2) / sum((data_train_2$EnergyTotal  - mean(data_train_2$EnergyTotal ))^2)


# R² par composante

R2_par_comp_train <- sapply(1:k_opt_pls, function(n) {
  pred <- predict(modelpls, newdata = data_train_2, ncomp = n)
  1 - sum((data_train_2$EnergyTotal - pred)^2) / sum((data_train_2$EnergyTotal - mean(data_train_2$EnergyTotal))^2)
})

R2_table_train <- data.frame(Composantes = 1:k_opt_pls, R2 = R2_par_comp_train)
R2_table_train

ggplot(R2_table_train, aes(x = Composantes, y = R2)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "R²", title = "Évolution du R² avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pls) + 
  theme_minimal()



#- 2.4.1.8. RMSE sur train -

# RMSE global

sqrt(mean((data_train_2$EnergyTotal - pred_pls_train)^2))


# RMSE par composante

RMSEP_par_comp_train <- sapply(1:k_opt_pls, function(n) {
  pred <- predict(modelpls, newdata = data_train_2, ncomp = n)
  sqrt(mean((data_train_2$EnergyTotal - pred)^2))
})

RMSEP_table_train <- data.frame(Composantes = 1:k_opt_pls, RMSEP = RMSEP_par_comp_train)
RMSEP_table_train

ggplot(RMSEP_table_train, aes(x = Composantes, y = RMSEP)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "RMSEP", title = "Évolution du RMSEP avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pls) + 
  theme_minimal()



#- 2.4.1.9. Réel vs Prévisions sur train -

# Créer le dataframe de comparaison

comparison_data <- data.frame(
  Vraies_Valeurs = data_train_2$EnergyTotal,
  Valeurs_Predites = as.vector(pred_pls_train)
)


# Déterminer les limites actuelles des axes

x_min <- min(comparison_data$Vraies_Valeurs, na.rm = TRUE)
x_max <- max(comparison_data$Vraies_Valeurs, na.rm = TRUE)
y_min <- min(comparison_data$Valeurs_Predites, na.rm = TRUE)
y_max <- max(comparison_data$Valeurs_Predites, na.rm = TRUE)


# Créer un dataframe pour la ligne diagonale x = y

line_data <- data.frame(
  Vraies_Valeurs = c(min(x_min, y_min), max(x_max, y_max)),
  Valeurs_Predites = c(min(x_min, y_min), max(x_max, y_max))
)


# Tracer le graphique avec geom_line pour la ligne x = y

ggplot(comparison_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites)) +
  geom_point(color = "blue") +
  geom_line(data = line_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites), color = "red", linetype = "dashed") +
  labs(x = "Valeurs Réelles", y = "Valeurs Prédites", title = "Comparaison entre les Valeurs Prédites et Réelles PLS") +
  theme_minimal() +
  coord_fixed(ratio = 1)





#--- 2.4.2. Avec racine

#- 2.4.2.1. Sélectionner le nombre de composantes optimal (selon CV) -

k_opt_pls_sqrt <- selectNcomp(modelpls_sqrt, method = "onesigma", plot = TRUE) 

## 3 composantes



#- 2.4.2.2. Prédiction sur le jeu test -

pred_pls_test_sqrt <- predict(modelpls_sqrt, 
                              newdata = data_test_2,
                              ncomp = k_opt_pls_sqrt)



#- 2.4.2.3 R² sur test -

# R² global

1 - sum((data_test_2$EnergyTotal_sqrt  - pred_pls_test_sqrt)^2) / sum((data_test_2$EnergyTotal_sqrt  - mean(data_test_2$EnergyTotal_sqrt ))^2)


# R² par composante

R2_par_comp_test_sqrt <- sapply(1:k_opt_pls_sqrt, function(n) {
  pred <- predict(modelpls_sqrt, newdata = data_test_2, ncomp = n)
  1 - sum((data_test_2$EnergyTotal_sqrt - pred)^2) / sum((data_test_2$EnergyTotal_sqrt - mean(data_test_2$EnergyTotal_sqrt))^2)
})

R2_table_test_sqrt <- data.frame(Composantes = 1:k_opt_pls_sqrt, R2 = R2_par_comp_test_sqrt)
R2_table_test_sqrt

ggplot(R2_table_test_sqrt, aes(x = Composantes, y = R2)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "R²", title = "Évolution du R² avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pls_sqrt) + 
  theme_minimal()



#- 2.4.2.4. RMSE sur test -

# RMSE global

sqrt(mean((data_test_2$EnergyTotal_sqrt - pred_pls_test_sqrt)^2))


# RMSE par composante

RMSEP_par_comp_test_sqrt <- sapply(1:k_opt_pls_sqrt, function(n) {
  pred <- predict(modelpls_sqrt, newdata = data_test_2, ncomp = n)
  sqrt(mean((data_test_2$EnergyTotal_sqrt - pred)^2))
})

RMSEP_table_test_sqrt <- data.frame(Composantes = 1:k_opt_pls_sqrt, RMSEP = RMSEP_par_comp_test_sqrt)
RMSEP_table_test_sqrt

ggplot(RMSEP_table_test_sqrt, aes(x = Composantes, y = RMSEP)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "RMSEP", title = "Évolution du RMSEP avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pls_sqrt) + 
  theme_minimal()



#- 2.4.2.5. Réel vs prévision sur test -

# Créer le dataframe de comparaison

comparison_data <- data.frame(
  Vraies_Valeurs = data_test_2$EnergyTotal_sqrt,
  Valeurs_Predites = as.vector(pred_pls_test_sqrt)
)


# Déterminer les limites actuelles des axes

x_min <- min(comparison_data$Vraies_Valeurs, na.rm = TRUE)
x_max <- max(comparison_data$Vraies_Valeurs, na.rm = TRUE)
y_min <- min(comparison_data$Valeurs_Predites, na.rm = TRUE)
y_max <- max(comparison_data$Valeurs_Predites, na.rm = TRUE)


# Créer un dataframe pour la ligne diagonale x = y

line_data <- data.frame(
  Vraies_Valeurs = c(min(x_min, y_min), max(x_max, y_max)),
  Valeurs_Predites = c(min(x_min, y_min), max(x_max, y_max))
)


# Tracer le graphique avec geom_line pour la ligne x = y

ggplot(comparison_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites)) +
  geom_point(color = "blue") +
  geom_line(data = line_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites), color = "red", linetype = "dashed") +
  labs(x = "Valeurs Réelles", y = "Valeurs Prédites", title = "Comparaison entre les Valeurs Prédites et Réelles PLS sqrt") +
  theme_minimal() +
  coord_fixed(ratio = 1)



#- 2.4.2.6. Prédiction sur le jeu d'apprentissage -

pred_pls_train_sqrt <- predict(modelpls_sqrt, 
                               newdata = data_train_2,
                               ncomp = k_opt_pls_sqrt)


#- 2.4.2.7. R² sur train -

# R² global

1 - sum((data_train_2$EnergyTotal_sqrt  - pred_pls_train_sqrt)^2) / sum((data_train_2$EnergyTotal_sqrt  - mean(data_train_2$EnergyTotal_sqrt ))^2)


# R² par composante

R2_par_comp_train_sqrt <- sapply(1:k_opt_pls_sqrt, function(n) {
  pred <- predict(modelpls_sqrt, newdata = data_train_2, ncomp = n)
  1 - sum((data_train_2$EnergyTotal_sqrt - pred)^2) / sum((data_train_2$EnergyTotal_sqrt - mean(data_train_2$EnergyTotal_sqrt))^2)
})

R2_table_train_sqrt <- data.frame(Composantes = 1:k_opt_pls_sqrt, R2 = R2_par_comp_train_sqrt)
R2_table_train_sqrt

ggplot(R2_table_train_sqrt, aes(x = Composantes, y = R2)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "R²", title = "Évolution du R² avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pls_sqrt) + 
  theme_minimal()



#- 2.4.2.8. RMSE sur train -

# RMSE global

sqrt(mean((data_train_2$EnergyTotal_sqrt - pred_pls_train_sqrt)^2))


# RMSE par composante

RMSEP_par_comp_train_sqrt <- sapply(1:k_opt_pls_sqrt, function(n) {
  pred <- predict(modelpls_sqrt, newdata = data_train_2, ncomp = n)
  sqrt(mean((data_train_2$EnergyTotal_sqrt - pred)^2))
})

RMSEP_table_train_sqrt <- data.frame(Composantes = 1:k_opt_pls_sqrt, RMSEP = RMSEP_par_comp_train_sqrt)
RMSEP_table_train_sqrt

ggplot(RMSEP_table_train_sqrt, aes(x = Composantes, y = RMSEP)) +
  geom_line() +
  geom_point() +
  labs(x = "Nombre de composantes", y = "RMSEP", title = "Évolution du RMSEP avec le nombre de composantes") +
  scale_x_continuous(breaks = 1:k_opt_pls_sqrt) + 
  theme_minimal()



#- 2.4.2.9. Réel vs Prévisions sur train -

# Créer le dataframe de comparaison

comparison_data <- data.frame(
  Vraies_Valeurs = data_train_2$EnergyTotal_sqrt,
  Valeurs_Predites = as.vector(pred_pls_train_sqrt)
)


# Déterminer les limites actuelles des axes

x_min <- min(comparison_data$Vraies_Valeurs, na.rm = TRUE)
x_max <- max(comparison_data$Vraies_Valeurs, na.rm = TRUE)
y_min <- min(comparison_data$Valeurs_Predites, na.rm = TRUE)
y_max <- max(comparison_data$Valeurs_Predites, na.rm = TRUE)


# Créer un dataframe pour la ligne diagonale x = y

line_data <- data.frame(
  Vraies_Valeurs = c(min(x_min, y_min), max(x_max, y_max)),
  Valeurs_Predites = c(min(x_min, y_min), max(x_max, y_max))
)


# Tracer le graphique avec geom_line pour la ligne x = y

ggplot(comparison_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites)) +
  geom_point(color = "blue") +
  geom_line(data = line_data, aes(x = Vraies_Valeurs, y = Valeurs_Predites), color = "red", linetype = "dashed") +
  labs(x = "Valeurs Réelles", y = "Valeurs Prédites", title = "Comparaison entre les Valeurs Prédites et Réelles PLS sqrt") +
  theme_minimal() +
  coord_fixed(ratio = 1)







#---------- 3. Comparaison des modèles ----------

loadings(modelpcr)
loadings(modelpcr_sqrt)
loadings(modelpls)
loadings(modelpls_sqrt)