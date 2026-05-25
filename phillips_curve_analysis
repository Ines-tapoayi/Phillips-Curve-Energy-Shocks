install.packages("devtools")
library(devtools)
install_github("taceconomics/taceconomics-r")

## Chargement des packages
library(taceconomics)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(stats)

## Clé API TAC
taceconomics.apikey("sk_vi3tyYZnKTionSr9hAhXp2yxsPBbKSzSOq6TAZ87ykk")


## Choix des variables

Poids_inflation_energy_FRA <- getdata ("EUROSTAT/PRC_HICP_INW_NRG/FRA")

Poids_inflation_energy_ITA <- getdata ("EUROSTAT/PRC_HICP_INW_NRG/ITA")

Inflation_FRA <- getdata ("EUROSTAT/PRC_HICP_MIDX_CP00_I15/FRA?transform=growth_yoy")

Inflation_ITA <- getdata ("EUROSTAT/PRC_HICP_MIDX_CP00_I15/ITA?transform=growth_yoy")

Chomage_FRA <- getdata("EUROSTAT/EI_LMHR_M_LM-UN-T-TOT_SA_PC_ACT/FRA")

Chomage_ITA <- getdata("EUROSTAT/EI_LMHR_M_LM-UN-T-TOT_SA_PC_ACT/ITA")

Prix_petrole <- getdata("PINKSHEET/CRUDE_BRENT/WLD?transform=growth_yoy")



# (pour choix des pays)
# Mise en forme des données
df_fra <- data.frame(
  Date = as.Date(index(Poids_inflation_energy_FRA)),
  Poids_Energie = as.numeric(Poids_inflation_energy_FRA),
  Pays = "France"
)

df_ita <- data.frame(
  Date = as.Date(index(Poids_inflation_energy_ITA)),
  Poids_Energie = as.numeric(Poids_inflation_energy_ITA),
  Pays = "Italie"
)

data_energy <- rbind(df_fra, df_ita)

# Graphique

png("dependance_energetique_distribution_FRA_ITA_points.png",
    width = 2000, height = 1500, res = 300)

ggplot(data_energy, aes(x = Pays, y = Poids_Energie, fill = Pays)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(
    title = "Dépendance énergétique dans l’inflation domestique",
    subtitle = "Distribution du poids de l’énergie (1996–2025)",
    x = "Pays",
    y = "Poids de l’énergie (indice HICP)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

dev.off()


# APPLICATION EMPIRIQUE


# Inflation∼anticipations adaptatives+chomage (Modele 1)

# anticipations adaptatives = inflation retardée

Inflation_lag_FRA <- lag(Inflation_FRA, 1)
Inflation_lag_ITA <- lag(Inflation_ITA, 1)


# DataFrame France

df_inf_FRA <- data.frame(
  Date = as.Date(index(Inflation_FRA)),
  Inflation = as.numeric(Inflation_FRA)
)
df_chom_FRA <- data.frame(
  Date = as.Date(index(Chomage_FRA)),
  Chomage = as.numeric(Chomage_FRA)
)

df_petrole <- data.frame(
  Date = as.Date(index(Prix_petrole)),
  Petrole = as.numeric(Prix_petrole)
)

df_inf_FRA$Annee <- year(df_inf_FRA$Date)
df_chom_FRA$Annee <- year(df_chom_FRA$Date)
df_petrole$Annee <- year(df_petrole$Date)

inf_ann_FRA <- df_inf_FRA %>%
  group_by(Annee) %>%
  summarise(Inflation = mean(Inflation, na.rm = TRUE))

chom_ann_FRA <- df_chom_FRA %>%
  group_by(Annee) %>%
  summarise(Chomage = mean(Chomage, na.rm = TRUE))

petrole_ann <- df_petrole %>%
  group_by(Annee) %>%
  summarise(Petrole = mean(Petrole, na.rm = TRUE))


df_FRA_final <- inf_ann_FRA %>%
  left_join(chom_ann_FRA, by = "Annee") %>%
  left_join(petrole_ann, by = "Annee")

df_FRA_final <- df_FRA_final %>%
  mutate(Inflation_lag = lag(Inflation, 1)) %>%
  na.omit()

# Estimation modele France
modele_1_FRA <- lm(Inflation ~ Inflation_lag + Chomage, data = df_FRA_final)
summary(modele_1_FRA)



# DataFrame Italie
df_inf_ITA <- data.frame(
  Date = as.Date(index(Inflation_ITA)),
  Inflation = as.numeric(Inflation_ITA)
)
df_chom_ITA <- data.frame(
  Date = as.Date(index(Chomage_ITA)),
  Chomage = as.numeric(Chomage_ITA)
)

df_petrole <- data.frame(
  Date = as.Date(index(Prix_petrole)),
  Petrole = as.numeric(Prix_petrole)
)

df_inf_ITA$Annee <- year(df_inf_ITA$Date)
df_chom_ITA$Annee <- year(df_chom_ITA$Date)
df_petrole$Annee <- year(df_petrole$Date)

inf_ann_ITA <- df_inf_ITA %>%
  group_by(Annee) %>%
  summarise(Inflation = mean(Inflation, na.rm = TRUE))

chom_ann_ITA <- df_chom_ITA %>%
  group_by(Annee) %>%
  summarise(Chomage = mean(Chomage, na.rm = TRUE))

petrole_ann <- df_petrole %>%
  group_by(Annee) %>%
  summarise(Petrole = mean(Petrole, na.rm = TRUE))


df_ITA_final <- inf_ann_ITA %>%
  left_join(chom_ann_ITA, by = "Annee") %>%
  left_join(petrole_ann, by = "Annee")

df_ITA_final <- df_ITA_final %>%
  mutate(Inflation_lag = lag(Inflation, 1)) %>%
  na.omit()

# Estimation modele Italie
modele_1_ITA <- lm(Inflation ~ Inflation_lag + Chomage, data = df_ITA_final)
summary(modele_1_ITA)


# Inflation∼anticipations adaptatives+chomage + petrole (Modele 2)

# France
modele_2_FRA <- lm(
  Inflation ~ Inflation_lag + Chomage + Petrole,
  data = df_FRA_final
)

summary(modele_2_FRA)


# Italie
modele_2_ITA <- lm(
  Inflation ~ Inflation_lag + Chomage + Petrole,
  data = df_ITA_final
)

summary(modele_2_ITA)


# Graphiques pour expliquer la pertinence des modèles 

# Inflation observée vs prédite

# France
df_FRA_final$pred_m1 <- fitted(modele_1_FRA)
df_FRA_final$pred_m2 <- fitted(modele_2_FRA)

library(ggplot2)

png("Inflation_predite_France.png", width = 900, height = 600)

ggplot(df_FRA_final, aes(x = Annee)) +
  geom_line(aes(y = Inflation, color = "Inflation observée"), linewidth = 1) +
  geom_line(aes(y = pred_m1, color = "Modèle 1"), linetype = "dashed") +
  geom_line(aes(y = pred_m2, color = "Modèle 2"), linetype = "dotdash") +
  labs(
    title = "Inflation observée et inflation prédite — France",
    x = "Année",
    y = "Inflation (%)",
    color = ""
  ) +
  theme_minimal()

dev.off()

# Italie
df_ITA_final$pred_m1 <- fitted(modele_1_ITA)
df_ITA_final$pred_m2 <- fitted(modele_2_ITA)

png("Inflation_predite_Italie.png", width = 900, height = 600)

ggplot(df_ITA_final, aes(x = Annee)) +
  geom_line(aes(y = Inflation, color = "Inflation observée"), linewidth = 1) +
  geom_line(aes(y = pred_m1, color = "Modèle 1"), linetype = "dashed") +
  geom_line(aes(y = pred_m2, color = "Modèle 2"), linetype = "dotdash") +
  labs(
    title = "Inflation observée et inflation prédite — Italie",
    x = "Année",
    y = "Inflation (%)",
    color = ""
  ) +
  theme_minimal()

dev.off()


# Résidus des modèles

# France
df_FRA_final$resid_m1 <- residuals(modele_1_FRA)
df_FRA_final$resid_m2 <- residuals(modele_2_FRA)

png("Residus_France.png", width = 900, height = 600)

ggplot(df_FRA_final, aes(x = Annee)) +
  geom_line(aes(y = resid_m1, color = "Modèle 1"), linetype = "dashed") +
  geom_line(aes(y = resid_m2, color = "Modèle 2")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(
    title = "Résidus des modèles — France",
    x = "Année",
    y = "Résidu",
    color = ""
  ) +
  theme_minimal()

dev.off()


# Italie
df_ITA_final$resid_m1 <- residuals(modele_1_ITA)
df_ITA_final$resid_m2 <- residuals(modele_2_ITA)

png("Residus_Italie.png", width = 900, height = 600)

ggplot(df_ITA_final, aes(x = Annee)) +
  geom_line(aes(y = resid_m1, color = "Modèle 1"), linetype = "dashed") +
  geom_line(aes(y = resid_m2, color = "Modèle 2")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(
    title = "Résidus des modèles — Italie",
    x = "Année",
    y = "Résidu",
    color = ""
  ) +
  theme_minimal()

dev.off()



# TESTS ECONOMETRIQUES

#Stationnarité des séries — Test ADF

library(tseries)

# France
# Inflation
adf.test(df_FRA_final$Inflation)

# Inflation retardée
adf.test(df_FRA_final$Inflation_lag)

# Chômage
adf.test(df_FRA_final$Chomage)

# Prix du pétrole
adf.test(df_FRA_final$Petrole)


# Italie
# Inflation
adf.test(df_ITA_final$Inflation)

# Inflation retardée
adf.test(df_ITA_final$Inflation_lag)

# Chômage
adf.test(df_ITA_final$Chomage)

# Prix du pétrole
adf.test(df_ITA_final$Petrole)



# Autocorrélation des résidus — Durbin–Watson

library(lmtest)

# France
dwtest(modele_1_FRA)
dwtest(modele_2_FRA)

# Italie
dwtest(modele_1_ITA)
dwtest(modele_2_ITA)


# Hétéroscédasticité — Breusch–Pagan

# France
bptest(modele_1_FRA)
bptest(modele_2_FRA)

# Italie
bptest(modele_1_ITA)
bptest(modele_2_ITA)


# Normalité des résidus — Jarque–Bera

# France
jarque.bera.test(residuals(modele_1_FRA))
jarque.bera.test(residuals(modele_2_FRA))

# Italie
jarque.bera.test(residuals(modele_1_ITA))
jarque.bera.test(residuals(modele_2_ITA))



# CONCLUSION

# Courbes Phillips conditionnelles 

# France
# Grille de chômage
chom_grid <- seq(
  min(df_FRA_final$Chomage),
  max(df_FRA_final$Chomage),
  length.out = 100
)

# Data frame pour prédiction
df_phillips_FRA <- data.frame(
  Chomage = chom_grid,
  Inflation_lag = mean(df_FRA_final$Inflation_lag),
  Petrole = mean(df_FRA_final$Petrole)
)

# Prédictions
df_phillips_FRA$pred_m1 <- predict(
  modele_1_FRA,
  newdata = df_phillips_FRA
)

df_phillips_FRA$pred_m2 <- predict(
  modele_2_FRA,
  newdata = df_phillips_FRA
)

png("Phillips_France_conditionnelle.png", width = 900, height = 600)

library(ggplot2)

ggplot(df_phillips_FRA, aes(x = Chomage)) +
  geom_line(aes(y = pred_m1, color = "Modèle 1"), linetype = "dashed") +
  geom_line(aes(y = pred_m2, color = "Modèle 2")) +
  labs(
    title = "Courbe de Phillips conditionnelle — France",
    x = "Taux de chômage (%)",
    y = "Inflation prédite (%)",
    color = ""
  ) +
  theme_minimal()

dev.off()

# Italie 

# Grille de chômage
chom_grid_ITA <- seq(
  min(df_ITA_final$Chomage),
  max(df_ITA_final$Chomage),
  length.out = 100
)
# Data frame pour prédiction
df_phillips_ITA <- data.frame(
  Chomage = chom_grid_ITA,
  Inflation_lag = mean(df_ITA_final$Inflation_lag),
  Petrole = mean(df_ITA_final$Petrole)
)

# Prédictions
df_phillips_ITA$pred_m1 <- predict(
  modele_1_ITA,
  newdata = df_phillips_ITA
)

df_phillips_ITA$pred_m2 <- predict(
  modele_2_ITA,
  newdata = df_phillips_ITA
)

png("Phillips_Italie_conditionnelle.png", width = 900, height = 600)

library(ggplot2)

ggplot(df_phillips_ITA, aes(x = Chomage)) +
  geom_line(aes(y = pred_m1, color = "Modèle 1"), linetype = "dashed") +
  geom_line(aes(y = pred_m2, color = "Modèle 2")) +
  labs(
    title = "Courbe de Phillips conditionnelle — Italie",
    x = "Taux de chômage (%)",
    y = "Inflation prédite (%)",
    color = ""
  ) +
  theme_minimal()

dev.off()


# Courbes de Phillips conditionnelles selon le niveau du pétrole
# France 
# Niveaux de pétrole
petrole_bas   <- quantile(df_FRA_final$Petrole, 0.25, na.rm = TRUE)
petrole_moyen <- mean(df_FRA_final$Petrole, na.rm = TRUE)
petrole_haut  <- quantile(df_FRA_final$Petrole, 0.75, na.rm = TRUE)

# Grille de chômage
chomage_seq_FRA <- seq(
  min(df_FRA_final$Chomage),
  max(df_FRA_final$Chomage),
  length.out = 100
)

# Inflation retardée fixée à la moyenne
infl_lag_moy_FRA <- mean(df_FRA_final$Inflation_lag, na.rm = TRUE)

# Prédictions 
library(dplyr)

df_pred_FRA <- bind_rows(
  data.frame(
    Chomage = chomage_seq_FRA,
    Inflation_pred = predict(
      modele_2_FRA,
      newdata = data.frame(
        Chomage = chomage_seq_FRA,
        Inflation_lag = infl_lag_moy_FRA,
        Petrole = petrole_bas
      )
    ),
    Niveau_petrole = "Pétrole bas"
  ),
  data.frame(
    Chomage = chomage_seq_FRA,
    Inflation_pred = predict(
      modele_2_FRA,
      newdata = data.frame(
        Chomage = chomage_seq_FRA,
        Inflation_lag = infl_lag_moy_FRA,
        Petrole = petrole_moyen
      )
    ),
    Niveau_petrole = "Pétrole moyen"
  ),
  data.frame(
    Chomage = chomage_seq_FRA,
    Inflation_pred = predict(
      modele_2_FRA,
      newdata = data.frame(
        Chomage = chomage_seq_FRA,
        Inflation_lag = infl_lag_moy_FRA,
        Petrole = petrole_haut
      )
    ),
    Niveau_petrole = "Pétrole élevé"
  )
)

png("Phillips_conditionnelle_petrole_France.png", width = 900, height = 600)

ggplot(df_pred_FRA, aes(x = Chomage, y = Inflation_pred, color = Niveau_petrole)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Courbe de Phillips conditionnelle selon le prix du pétrole — France",
    x = "Taux de chômage (%)",
    y = "Inflation prédite (%)",
    color = "Niveau du pétrole"
  ) +
  theme_minimal()

dev.off()



# Italie 
# Niveaux de pétrole
petrole_bas   <- quantile(df_ITA_final$Petrole, 0.25, na.rm = TRUE)
petrole_moyen <- mean(df_ITA_final$Petrole, na.rm = TRUE)
petrole_haut  <- quantile(df_ITA_final$Petrole, 0.75, na.rm = TRUE)

# Grille de chômage
chomage_seq_ITA <- seq(
  min(df_ITA_final$Chomage),
  max(df_ITA_final$Chomage),
  length.out = 100
)

# Inflation retardée fixée à la moyenne
infl_lag_moy_ITA <- mean(df_ITA_final$Inflation_lag, na.rm = TRUE)

# Prédictions
df_pred_ITA <- bind_rows(
  data.frame(
    Chomage = chomage_seq_ITA,
    Inflation_pred = predict(
      modele_2_ITA,
      newdata = data.frame(
        Chomage = chomage_seq_ITA,
        Inflation_lag = infl_lag_moy_ITA,
        Petrole = petrole_bas
      )
    ),
    Niveau_petrole = "Pétrole bas"
  ),
  data.frame(
    Chomage = chomage_seq_ITA,
    Inflation_pred = predict(
      modele_2_ITA,
      newdata = data.frame(
        Chomage = chomage_seq_ITA,
        Inflation_lag = infl_lag_moy_ITA,
        Petrole = petrole_moyen
      )
    ),
    Niveau_petrole = "Pétrole moyen"
  ),
  data.frame(
    Chomage = chomage_seq_ITA,
    Inflation_pred = predict(
      modele_2_ITA,
      newdata = data.frame(
        Chomage = chomage_seq_ITA,
        Inflation_lag = infl_lag_moy_ITA,
        Petrole = petrole_haut
      )
    ),
    Niveau_petrole = "Pétrole élevé"
  )
)

png("Phillips_conditionnelle_petrole_Italie.png", width = 900, height = 600)

ggplot(df_pred_ITA, aes(x = Chomage, y = Inflation_pred, color = Niveau_petrole)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Courbe de Phillips conditionnelle selon le prix du pétrole — Italie",
    x = "Taux de chômage (%)",
    y = "Inflation prédite (%)",
    color = "Niveau du pétrole"
  ) +
  theme_minimal()

dev.off()
