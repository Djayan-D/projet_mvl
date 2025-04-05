#---------- 0. PRÉLIMINAIRES ----------

#----- 0.1. Importer les packages -----

library(ggplot2)
library(pls)





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

# Le test de normalité de Shapiro-Wilk sur les résidus du modèle linéaire donne 
# une p-value de 4.362e-05, ce qui est largement inférieur au seuil de 0.05. 
# Cela indique que les résidus ne suivent pas une distribution normale, ce qui 
# suggère que l'hypothèse de normalité des erreurs n'est pas vérifiée dans ce 
# modèle.





#---------- 2. PCR ----------