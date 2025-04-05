#---------- 0. PRÉLIMINAIRES ----------

#----- 0.1. Importer les packages -----

library(ggplot2)
library(pls)
library(dplyr)





#----- 0.2. Importer les données -----

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





#----- 0.3. DÉFINIR Y -----

data_train$EnergyTotal <- data_train$HeatTotal + data_train$CoolTotal

data_test$EnergyTotal <- data_test$HeatTotal + data_test$CoolTotal





#---------- 1. EXPLORATION DES DONNÉES ----------

#----- 1.1. Vérification de la variable cible -----

ggplot(data_train, aes(x = EnergyTotal)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  labs(title = "Distribution de HeatTotal (UPENN)", x = "HeatTotal", y = "Fréquence")



#----- 1.2. Transformation racine carrée -----

data_train <- data_train |> 
  mutate(EnergyTotal_sqrt = sqrt(EnergyTotal))



#----- 1.3. Comparaison des distributions -----

par(mfrow = c(1, 2))
hist(data_train$EnergyTotal, main = "EnergyTotal", col = "skyblue", xlab = "Valeur brute")
hist(data_train$EnergyTotal_sqrt, main = "sqrt(EnergyTotal)", col = "lightgreen", xlab = "Racine carrée")



#----- 1.4. Régression linéaire pour évaluer les résidus -----

#--- 1.4.1. Sélectionner les variables ---

X_vars <- data_train |> 
  select(-ID, -HeatJan, -CoolJuly, -HeatTotal, -CoolTotal, -EnergyTotal, -EnergyTotal_sqrt)



#--- 1.4.2. Estimer le modèle ---

modele_lm <- lm(EnergyTotal_sqrt ~ ., data = cbind(X_vars, EnergyTotal_sqrt = data_train$EnergyTotal_sqrt))



#--- 1.4.3. Représenter les résidus ---

plot(residuals(modele_lm), main = "Résidus du modèle linéaire", col = "blue", pch = 20)
abline(h = 0, col = "red")



#--- 1.4.4. Tester la normalité ---

shapiro.test(residuals(modele_lm))

## Le test de normalité de Shapiro-Wilk sur les résidus du modèle linéaire donne 
## une p-value de 4.362e-05, ce qui est largement inférieur au seuil de 0.05. 
## Cela indique que les résidus ne suivent pas une distribution normale, ce qui 
## suggère que l'hypothèse de normalité des erreurs n'est pas vérifiée dans ce 
## modèle.





#---------- 2. PCR : RÉGRESSION SUR LES COMPOSANTES PRINCIPALES ----------

#----- 2.1. Préparation des données -----

## On retire les variables inutiles et on place la variable cible (EnergyTotal) 
## en première colonne

data_train_2 <- data_train |>
  select(-ID, -HeatJan, -CoolJuly, -HeatTotal, -CoolTotal, -EnergyTotal_sqrt) |> 
  relocate(EnergyTotal, .before = 1)



#----- 2.2. Application de la PCR -----

#--- 2.2.1. Définition des dimensions ---

n = nrow(data_train_2)         # Nombre d'observations
p = ncol(data_train_2) - 1     # Nombre de variables explicatives



#--- 2.2.2. Effectuer la PCR ---

## La PCR est effectuée avec centrage/réduction (scale=TRUE)

modelpcr <- pcr(EnergyTotal ~ ., data = data_train_2, ncomp = min(n - 1, p), scale = TRUE)    
summary(modelpcr)

## Interprétation :
## - 63% de la variance de Y est expliquée avec les 28 composantes
## - Le gain majeur se fait jusqu’à ~13-15 composantes
## - La variance expliquée de X atteint 100% avec les 28 composantes



#----- 2.3. R² d’apprentissage -----

R2(modelpcr)

validationplot(modelpcr, 
               val.type = "R2", 
               type = "b", 
               main = "Courbe de R² selon le nombre de composantes")

## Interprétation :
## - R² faible avec peu de composantes (ex : ~3.6% avec 1-2 comp.)
## - S'améliore sensiblement jusqu'à ~60% à partir de 13-23 composantes
## - R² max ~63% avec les 28 composantes



#----- 2.4. RMSEP -----

RMSEP(modelpcr)

validationplot(modelpcr, 
               type = "b",
               main = "Courbe du RMSE selon le nombre de composantes")

## Interprétation :
## - RMSEP diminue fortement jusqu'à ~13 composantes, puis se stabilise (~20.7)
## - Gain marginal au-delà de 20 composantes
## - Un compromis raisonnable : 13 à 15 composantes



#----- 2.5. Comparaison avec une régression linéaire classique -----

summary(lm(EnergyTotal ~ ., data = data_train_2))

## Interprétation :
## - R² ajusté ≈ 0.51
## - RMSE ≈ 23.89
## - La PCR fait mieux, surtout en termes de RMSE
## - Certaines variables sont significatives (à 1,5 et 10%) :
##     - op_uValue, zone_occ_density, zone_light_heat_flow_rate, totalocc, ...



#----- 2.6. Validation croisée (CV) -----

## On effectue la validation croisée sur le modèle PCR

modelpcr_cv <- pcr(EnergyTotal ~ ., 
                   data = data_train_2, 
                   ncomp = min(n - 1, p), 
                   scale = TRUE, 
                   validation = "CV")


## Courbe RMSEP (validation croisée)

validationplot(modelpcr_cv, 
               val.type = "RMSEP", 
               type = "b", 
               main = "RMSEP en validation croisée")

## Interprétation :
## - Le RMSEP en validation croisée suit une tendance similaire à celle de 
##   l'apprentissage.
## - Le nombre optimal de composantes en validation croisée semble se situer 
##   autour de 13-15 composantes.



#----- 2.7. Prédiction sur le jeu test -----

#--- 2.7.1. Sélectionner le nombre de composantes optimal (selon CV) ---

k_opt <- 15 



#--- 2.7.2. Prédiction sur le jeu test ---

pred_pcr_test <- predict(modelpcr_cv, 
                         newdata = data_test, 
                         ncomp = k_opt)



#--- 2.7.3. RMSE sur le jeu test ---

sqrt(mean((data_test$EnergyTotal - pred_pcr_test)^2))

## Interprétation  : 
## Le RMSE sur le jeu de test est de 133.05, tandis que pour le jeu 
## d'entraînement, le RMSE est beaucoup plus faible, autour de 22.67 (au niveau 
## de 15 composantes). Cela montre une augmentation significative du RMSE 
## lorsqu'on passe du jeu d'entraînement au jeu de test, ce qui peut indiquer 
## un certain overfitting. Le modèle semble être bien ajusté aux données 
## d'entraînement, mais moins performant sur des données nouvelles.


#----- 2.8. Conclusion -----

## La PCR explique 63% de la variance avec 28 composantes, mais les meilleures 
## performances se situent autour de 13-15 composantes. En comparaison avec la 
## régression linéaire classique, la PCR est plus performante, surtout en termes 
## de RMSE. Cependant, sur le jeu de test, le RMSE est de 133.05, bien plus 
## élevé que sur l'entraînement (22.67), ce qui indique un possible overfitting. 
## Le modèle est bien ajusté aux données d'entraînement mais moins performant 
## sur des données nouvelles.


#---------- 2. PLS : RÉGRESSION PAR MOINDRES CARRÉS PARTIELS ----------

#----- 2.1. Application de la PLS -----

modelpls <- plsr(EnergyTotal ~ ., data = data_train_2, ncomp = min(n - 1, p), scale = TRUE)    
summary(modelpls)

## Interprétation
## - Le modèle PLS explique environ 63% de la variance de la variable cible 
##   (EnergyTotal) avec 28 composantes.
## - À partir de 13 composantes, la variance expliquée atteint un plateau (63%), 
##   indiquant que l'ajout de composantes supplémentaires n'apporte plus de 
##   gains significatifs.
## - Cette tendance est similaire à celle observée dans la PCR, où le modèle se 
##   stabilise après un certain nombre de composantes.



#----- 2.2. R² d’apprentissage -----

R2(modelpls)

validationplot(modelpls, 
               val.type = "R2", 
               type = "b", 
               main = "Courbe de R² selon le nombre de composantes")

## Interprétation :
## - Avec 1 composante, R² est faible (0%).
## - À partir de 2 composantes, R² augmente rapidement jusqu'à 62.9% avec 9 
##   composantes.
## - À partir de 10 composantes, R² se stabilise autour de 63%, et ce jusqu'à 28 
##   composantes.
## - Montre que la capacité explicative du modèle se stabilise après un certain
##   nombre de composantes, ce qui indique un bon compromis entre complexité et
##   performance.



#----- 2.3. RMSEP -----

RMSEP(modelpls)

validationplot(modelpls, 
               type = "b",
               main = "Courbe du RMSEP selon le nombre de composantes")

## Interprétation :
## - Le RMSEP diminue considérablement au début, surtout entre 1 et 5 
##   composantes, atteignant 20.66 après 10 composantes.
## - Après 10 composantes, le RMSEP reste stable à environ 20.66, ce qui 
##   suggère que l'ajout de plus de composantes n'améliore pas significativement 
##   le modèle.
## - Indique qu'un nombre optimal de 10 à 15 composantes pourrait offrir un bon 
##   compromis entre la complexité du modèle et la performance de prédiction.



#----- 2.4. Prédiction sur le jeu test -----

#--- 2.4.1. Sélectionner le nombre de composantes optimal (selon CV) ---

k_opt_pls <- 10 



#--- 2.4.2. Prédiction sur le jeu test ---

pred_pls_test <- predict(modelpls, 
                         newdata = data_test, 
                         ncomp = k_opt_pls)



#--- 2.4.3. RMSE sur le jeu test ---

sqrt(mean((data_test$EnergyTotal - pred_pls_test)^2))

## Interprétation :
## - Le RMSE sur le jeu de test est de 135.58, ce qui montre l'erreur de 
##   prédiction du modèle PLS sur des données nouvelles.
## - Comparé à celui du jeu d'apprentissage (où le RMSE est plus faible), 
##   cette augmentation peut indiquer un certain overfitting, bien que le 
##   modèle reste relativement performant.



#----- 2.5. Conclusion -----

## - Le modèle PLS montre une bonne capacité à expliquer la variance de la 
##   variable cible, mais la performance sur le jeu de test reste modérée avec 
##   un RMSE d'environ 135.58.
## - Le choix optimal du nombre de composantes semble se situer autour de 13-15 
##   composantes, où le modèle présente un bon compromis entre complexité et 
##   performance.
## - Comparé à la régression PCR, la PLS semble légèrement plus performante en 
##   termes de RMSE, mais le modèle souffre encore d'overfitting sur les données 
##   d'entraînement.

