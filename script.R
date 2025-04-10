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
# Vérifier que caret n'est pas chargé sinon ça fait buguer





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





#----- 1.4. Matrice de corrélation -----

corrplot(cor(data_2), 
         method = "circle",
         type = "upper",
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.5,
         diag = FALSE,
)





#----- 1.5. Variance Inflation Factor (VIF) -----

#--- 1.5.1. Calcul du VIF à partir du modèle linéaire ---

model_vif <- lm(EnergyTotal ~ . - EnergyTotal_sqrt, data = data_2) |> 
  vif()



#--- 1.5.2. Transformer le vecteur VIF en data.frame ---

vif_df <- data.frame(
  Variable = names(model_vif),
  VIF = as.numeric(model_vif)
)



#--- 1.5.3. Création du graphique avec ggplot ---

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





#----- 1.6. Analyse en Composantes Principales (ACP) -----

respca_ent <- PCA(data_2, 
                  scale.unit = TRUE, 
                  quanti.sup = which(names(data_2) %in% Y), 
                  graph = FALSE)
fviz_screeplot(respca_ent)
fviz_pca_var(respca_ent, repel = TRUE)
round(respca_ent$quanti.sup$coord, 2)










#---------- 2. PCR : RÉGRESSION SUR LES COMPOSANTES PRINCIPALES ----------

#----- 2.2. Application de la PCR -----

#--- 2.2.1. Définition des dimensions ---

n = nrow(data_train_2)         # Nombre d'observations
p = ncol(data_train_2) - 2     # Nombre de prédicteurs (-2 car 2 Y)



#--- 2.2.2. Effectuer la PCR ---

#- 2.2.2.1. Sans racine -

## La PCR est effectuée avec centrage/réduction (scale=TRUE)

modelpcr <- pcr(EnergyTotal ~ . -EnergyTotal_sqrt,
                data = data_train_2,
                ncomp = min(n - 1, p), 
                scale = TRUE)

summary(modelpcr)

## Interprétation :
## - 63% de la variance de Y est expliquée avec les 28 composantes
## - Le gain majeur se fait jusqu’à ~13-15 composantes
## - La variance expliquée de X atteint 100% avec les 28 composantes



#- 2.2.2.2. Avec racine -

modelpcr_sqrt <- pcr(EnergyTotal_sqrt ~ . -EnergyTotal, 
                     data = data_train_2, 
                     ncomp = min(n - 1, p),
                     scale = TRUE)

summary(modelpcr_sqrt)





#----- 2.3. R² d’apprentissage -----

#--- 2.3.1. Sans racine ---

R2(modelpcr)

validationplot(modelpcr,
               val.type="R2",
               type="b",
               main = "Courbe de R² selon le nombre de composantes - PCR sans racine")

## Interprétation :
## - R² faible avec peu de composantes (ex : ~3.6% avec 1-2 comp.)
## - S'améliore sensiblement jusqu'à ~60% à partir de 13-23 composantes
## - R² max ~63% avec les 28 composantes



#--- 2.3.2. Avec racine ---

R2(modelpcr_sqrt)

validationplot(modelpcr_sqrt, 
               val.type = "R2", 
               type = "b", 
               main = "Courbe de R² selon le nombre de composantes - PCR avec racine")

## Interprétation :
## - R² faible avec peu de composantes (ex : ~4.9% avec 1-2 comp.)
## - S'améliore sensiblement jusqu'à ~70% à partir de 13 composantes
## - R² max ~83% avec les 28 composantes



#----- 2.4. RMSEP -----

#--- 2.4.1. Sans racine ---

RMSEP(modelpcr)

validationplot(modelpcr, 
               type = "b",
               main = "Courbe du RMSE selon le nombre de composantes - PCR sans racine")

## Interprétation :
## - RMSEP diminue fortement jusqu'à ~13 composantes, puis se stabilise (~20.7)
## - Gain marginal au-delà de 20 composantes
## - Un compromis raisonnable : 13 à 15 composantes



#--- 2.4.2. Avec racine ---

RMSEP(modelpcr_sqrt)

validationplot(modelpcr_sqrt, 
               type = "b",
               main = "Courbe du RMSE selon le nombre de composantes - PCR avec racine")



#----- 2.5. Comparaison avec une régression linéaire classique -----

#--- 2.5.1. Sans racine ---

summary(lm(EnergyTotal ~ . -EnergyTotal_sqrt, data = data_train_2))

## Interprétation :
## - R² ajusté ≈ 0.51
## - RMSE ≈ 23.89
## - La PCR fait mieux, surtout en termes de RMSE
## - Certaines variables sont significatives (à 1,5 et 10%) :
##     - op_uValue, zone_occ_density, zone_light_heat_flow_rate, totalocc, ...



#--- 2.5.2. Avec racine ---

summary(lm(EnergyTotal_sqrt ~ . -EnergyTotal, data = data_train_2))





#----- 2.6. Validation croisée (CV) -----

## On effectue la validation croisée sur le modèle PCR

#--- 2.6.1. Sans racine ---

modelpcr_cv <- pcr(EnergyTotal ~ . -EnergyTotal_sqrt, 
                   data = data_train_2, 
                   ncomp = min(n - 1, p), 
                   scale = TRUE, 
                   validation = "CV")


## Courbe R2 (validation croisée)

validationplot(modelpcr_cv, 
               val.type = "R2", 
               type = "b", 
               main = "R2 en validation croisée - PCR sans racine")


## Courbe RMSEP (validation croisée)

validationplot(modelpcr_cv, 
               val.type = "RMSEP", 
               type = "b", 
               main = "RMSEP en validation croisée - PCR sans racine")

## Interprétation :
## - Le RMSEP en validation croisée suit une tendance similaire à celle de 
##   l'apprentissage.
## - Le nombre optimal de composantes en validation croisée semble se situer 
##   autour de 10-15 composantes.





#--- 2.6.2. Avec racine ---

modelpcr_sqrt_cv <- pcr(EnergyTotal_sqrt ~ . -EnergyTotal, 
                        data = data_train_2, 
                        ncomp = min(n - 1, p), 
                        scale = TRUE, 
                        validation = "CV")


## Courbe R2 (validation croisée)

validationplot(modelpcr_sqrt_cv, 
               val.type = "R2", 
               type = "b", 
               main = "R2 en validation croisée - PCR avec racine")


## Courbe RMSEP (validation croisée)

validationplot(modelpcr_sqrt_cv, 
               val.type = "RMSEP", 
               type = "b", 
               main = "RMSEP en validation croisée - PCR avec racine")





#----- 2.7. Prédiction sur le jeu test -----

#--- 2.7.1. Sans racine ---

#- 2.7.1.1. Sélectionner le nombre de composantes optimal (selon CV) -

k_opt_pcr <- selectNcomp(modelpcr_cv, method = "onesigma", plot = TRUE)

## 10 composantes sont choisies, ce qui semble assez logique d'après le graph



#- 2.7.1.2. Prédiction sur le jeu test -

pred_pcr_test <- predict(modelpcr_cv, 
                         newdata = data_test_2, # Pas besoin de retirer EnergyTotal_sqrt car il est déjà retiré dans modelpcr_cv
                         ncomp = k_opt_pcr)



#- 2.7.1.3. RMSE sur le jeu test -

sqrt(mean((data_test_2$EnergyTotal - pred_pcr_test)^2))

## Interprétation  : 
## Le RMSE sur le jeu de test est de 135.35, tandis que pour le jeu 
## d'entraînement, le RMSE est beaucoup plus faible, autour de 22.67 (au niveau 
## de 15 composantes). Cela montre une augmentation significative du RMSE 
## lorsqu'on passe du jeu d'entraînement au jeu de test, ce qui peut indiquer 
## un certain overfitting. Le modèle semble être bien ajusté aux données 
## d'entraînement, mais moins performant sur des données nouvelles.



#- 2.7.1.4. Conclusion -

## La PCR explique 63% de la variance avec 28 composantes, mais les meilleures 
## performances se situent autour de 13-15 composantes. En comparaison avec la 
## régression linéaire classique, la PCR est plus performante, surtout en termes 
## de RMSE. Cependant, sur le jeu de test, le RMSE est de 133.05, bien plus 
## élevé que sur l'entraînement (22.67), ce qui indique un possible overfitting. 
## Le modèle est bien ajusté aux données d'entraînement mais moins performant 
## sur des données nouvelles.



#--- 2.7.2. Avec racine

#- 2.7.1.1. Sélectionner le nombre de composantes optimal (selon CV) -

k_opt_pcr_sqrt <- selectNcomp(modelpcr_sqrt_cv, method = "onesigma", plot = TRUE) 

## 13 composantes sélectionnées, assez logique aussi



#- 2.7.1.2. Prédiction sur le jeu test -

pred_pcr_sqrt_test <- predict(modelpcr_sqrt_cv, 
                              newdata = data_test_2,
                              ncomp = k_opt_pcr_sqrt)



#- 2.7.1.3. RMSE sur le jeu test -

sqrt(mean((data_test_2$EnergyTotal_sqrt - pred_pcr_sqrt_test)^2))



#- 2.7.1.4. Conclusion -










#---------- 2. PLS : RÉGRESSION PAR MOINDRES CARRÉS PARTIELS ----------

#----- 2.1. Application de la PLS -----

#--- 2.1.1. Sans racine ---

modelpls <- plsr(EnergyTotal ~ . -EnergyTotal_sqrt, 
                 data = data_train_2, 
                 ncomp = min(n - 1, p), 
                 scale = TRUE)

summary(modelpls)

## Interprétation
## - Le modèle PLS explique environ 63% de la variance de la variable cible 
##   (EnergyTotal) avec 28 composantes.
## - À partir de 13 composantes, la variance expliquée atteint un plateau (63%), 
##   indiquant que l'ajout de composantes supplémentaires n'apporte plus de 
##   gains significatifs.
## - Cette tendance est similaire à celle observée dans la PCR, où le modèle se 
##   stabilise après un certain nombre de composantes.



#--- 2.1.2. Avec racine ---

modelpls_sqrt <- plsr(EnergyTotal_sqrt ~ . -EnergyTotal, 
                 data = data_train_2, 
                 ncomp = min(n - 1, p), 
                 scale = TRUE)

summary(modelpls_sqrt)





#----- 2.2. R² d’apprentissage -----

#--- 2.2.1. Sans racine ---

R2(modelpls)

validationplot(modelpls, 
               val.type = "R2", 
               type = "b", 
               main = "Courbe de R² selon le nombre de composantes - PLS sans racine")



#--- 2.2.2. Avec racine ---

R2(modelpls_sqrt)

validationplot(modelpls_sqrt, 
               val.type = "R2", 
               type = "b", 
               main = "Courbe de R² selon le nombre de composantes - PLS avec racine")





#----- 2.3. RMSEP -----

#--- 2.3.1. Sans racine ---

RMSEP(modelpls)

validationplot(modelpls, 
               type = "b",
               main = "Courbe du RMSEP selon le nombre de composantes - PLS sans racine")

## Interprétation :
## - Le RMSEP diminue considérablement au début, surtout entre 1 et 5 
##   composantes, atteignant 20.66 après 10 composantes.
## - Après 10 composantes, le RMSEP reste stable à environ 20.66, ce qui 
##   suggère que l'ajout de plus de composantes n'améliore pas significativement 
##   le modèle.
## - Indique qu'un nombre optimal de 10 à 15 composantes pourrait offrir un bon 
##   compromis entre la complexité du modèle et la performance de prédiction.



#--- 2.3.2. Avec racine ---

RMSEP(modelpls_sqrt)

validationplot(modelpls_sqrt, 
               type = "b",
               main = "Courbe du RMSEP selon le nombre de composantes - PLS avec racine")



#----- 2.4. Comparaison avec une régression linéaire classique -----

#--- 2.4.1. Sans racine ---

summary(lm(EnergyTotal ~ . -EnergyTotal_sqrt, data = data_train_2))

## Interprétation :
## - R² ajusté ≈ 0.51
## - RMSE ≈ 23.89
## - La PCR fait mieux, surtout en termes de RMSE
## - Certaines variables sont significatives (à 1,5 et 10%) :
##     - op_uValue, zone_occ_density, zone_light_heat_flow_rate, totalocc, ...



#--- 2.4.2. Avec racine ---

summary(lm(EnergyTotal_sqrt ~ . -EnergyTotal, data = data_train_2))





#----- 2.5. Validation croisée (CV) -----

## On effectue la validation croisée sur le modèle PLS

#--- 2.5.1. Sans racine ---

modelpls_cv <- plsr(EnergyTotal ~ . -EnergyTotal_sqrt, 
                   data = data_train_2, 
                   ncomp = min(n - 1, p), 
                   scale = TRUE,
                   validation = "CV")


## Courbe RMSEP (validation croisée)

validationplot(modelpls_cv, 
               val.type = "RMSEP", 
               type = "b", 
               main = "RMSEP en validation croisée - PLS sans racine",
               estimate = "train")



#--- 2.5.2. Avec racine ---

modelpls_sqrt_cv <- plsr(EnergyTotal_sqrt ~ . -EnergyTotal, 
                        data = data_train_2, 
                        ncomp = min(n - 1, p), 
                        scale = TRUE, 
                        validation = "CV")


## Courbe RMSEP (validation croisée)

validationplot(modelpls_sqrt_cv, 
               val.type = "RMSEP", 
               type = "b", 
               main = "RMSEP en validation croisée - PLS avec racine")





#----- 2.6. Prédiction sur le jeu test -----

#--- 2.6.1. Sans racine ---

#- 2.4.1.1. Sélectionner le nombre de composantes optimal (selon CV) -

k_opt_pls <- selectNcomp(modelpls_cv, method = "onesigma", plot = TRUE) 

## 2 composantes



#- 2.4.1.2. Prédiction sur le jeu test -

pred_pls_test <- predict(modelpls, 
                         newdata = data_test_2, 
                         ncomp = k_opt_pls)



#- 2.4.1.3. RMSE sur le jeu test -

sqrt(mean((data_test_2$EnergyTotal - pred_pls_test)^2))



#- 2.4.1.4. Conclusion -



#--- 2.4.2. Avec racine ---

#- 2.4.2.1. Sélectionner le nombre de composantes optimal (selon CV) -

k_opt_pls_sqrt <- selectNcomp(modelpls_sqrt_cv, method = "onesigma", plot = TRUE) 

## 2



#- 2.4.2.2. Prédiction sur le jeu test -

pred_pls_sqrt_test <- predict(modelpls_sqrt, 
                              newdata = data_test_2, 
                              ncomp = k_opt_pls_sqrt)



#- 2.4.2.3. RMSE sur le jeu test -

sqrt(mean((data_test_2$EnergyTotal_sqrt - pred_pls_sqrt_test)^2))



#- 2.4.2.4. Conclusion -



