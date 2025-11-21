# 1. Die Beta-Verteilung------

pdf("Abbildungen/beta_plot.pdf")
shape1 <- 2
shape2 <- 5
curve(dbeta(x, shape1 = shape1, shape2 = shape2), 
      from = 0, to = 1, 
      col = "steelblue", lwd = 2,
      ylab = "Dichte", xlab = "x", 
      main = "Beta(2, 5)")
dev.off()

shape1 <- 10
shape2 <- 5
curve(dbeta(x, shape1 = shape1, shape2 = shape2), 
      from = 0, to = 1, 
      col = "darkgreen", lwd = 2,
      ylab = "Dichte", xlab = "x", 
      main = "Beta(2, 5)")

# 2. Reparametrisierung der Beta-Vtlg. 

mu <- 0.5
phi <- 5

alpha <- mu * phi
beta <- (1 - mu) * phi

curve(dbeta(x, alpha, beta),
      from = 0, to = 1,
      col = "tomato", lwd = 2,
      ylab = "Dichte", xlab = "x",
      main = paste("Beta(", alpha, ",", beta, ")"))

# 3. Schätze eine Einfach-Beta-Regression ------
load("Data/Geovisualisierung.RData")
library("glmmTMB")
geovisualisierung$bip_je_einwohner <- geovisualisierung$bip_je_einwohner/10000 # Umskalieren des Einkommens
model_w <- glmmTMB(
  afd_prop ~ bip_je_einwohner ,
  family = beta_family(link = "logit"),
  data = geovisualisierung
)

library(modelsummary)

modelsummary(model_w,
             shape = term ~ component,
             statistic = "conf.int",
             output = "markdown")


# Vorhersagen erzeugen
geovisualisierung$pred <- predict(model_w, type = "response")
pdf("Abbildungen/bip.pdf")
ggplot(geovisualisierung, aes(x = bip_je_einwohner, y = pred)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(aes(y = afd_prop), alpha = 0.4) +
  labs(x = "bip_je_einwohner", y = "Vorhergesagter AfD-Stimmenanteil",
       title = "Beta-Regression: bip_je_Einwohner und AfD-Anteil") +
  theme_minimal()
dev.off()

# 4. Schätze eine Beta-Regression ------
model_w2 <- glmmTMB(
  afd_prop ~ ausl_proz + bip_je_einwohner,
  family = beta_family(link = "logit"),
  data = geovisualisierung
)

modelsummary(model_w2,
             shape = term ~ component,
             statistic = "conf.int",
             output = "markdown")

# Vorhersagen erzeugen
geovisualisierung$pred2 <- predict(model_w2, type = "response")
pdf("Abbildungen/bipundausländer.pdf")
ggplot(geovisualisierung, aes(x = einkommen, y = pred2)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(aes(y = afd_prop), alpha = 0.4) +
  labs(x = "bip_ausl", y = "Vorhergesagter AfD-Stimmenanteil",
       title = "Beta-Regression: bip und aus und AfD-Anteil") +
  theme_minimal()
dev.off()