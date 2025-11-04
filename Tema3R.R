# Verificare director curent
cat("Directorul curent de lucru este:", getwd(), "\n")

# Listare fișiere din director
cat("\nFișiere disponibile în directorul curent:\n")
print(list.files())

# Dacă fișierele sunt în alt director, setează directorul corect
# Exemplu: setwd("C:/Users/NumeTau/Documents/Statistica")

# ============================================================================
# EXERCIȚIUL 1 - Analiza consumului de alcool și decese cardiace
# ============================================================================

# Încărcare date - încercăm mai multe variante de nume
if(file.exists("Alcool_dataset.txt")) {
  alcool <- read.table("Alcool_dataset.txt", header = TRUE)
} else if(file.exists("alcool.dat")) {
  alcool <- read.table("alcool.dat", header = TRUE)
} else {
  # Creare manuală a datelor din document
  alcool <- data.frame(
    Tara = c("Australia", "Austria", "Belgia", "Canada", "Danemarca", 
             "Finlanda", "Franta", "Islanda", "Irlanda", "Italia", 
             "Olanda", "Noua_Zeelanda", "Norvegia", "Spania", "Suedia", 
             "Elvetia", "Marea_Britanie", "SUA", "Germania"),
    Alcool_din_vin = c(2.5, 3.9, 2.9, 2.4, 2.9, 0.8, 9.1, 0.8, 0.7, 7.9, 
                       1.8, 1.9, 0.8, 6.5, 1.6, 5.8, 1.3, 1.2, 2.7),
    Decese_datorate_afectiunilor_cardiace = c(211, 167, 131, 191, 220, 297, 
                                              71, 211, 300, 107, 167, 266, 
                                              227, 86, 207, 115, 285, 199, 172)
  )
  cat("Date create manual din documentul furnizat\n")
}

# Afișare primele rânduri pentru verificare
cat("\nPrimele rânduri din setul de date alcool:\n")
print(head(alcool))

# 1. Diagrama de împrăștiere
plot(alcool$Alcool_din_vin, alcool$Decese_datorate_afectiunilor_cardiace, 
     xlab = "Consum mediu de alcool din vin (litri/persoană/an)", 
     ylab = "Decese/100000 locuitori",
     main = "Relația dintre consumul de alcool și decesele cardiace",
     pch = 19, col = "blue", cex = 1.2)

# Adăugare etichete cu numele țărilor
text(alcool$Alcool_din_vin, alcool$Decese_datorate_afectiunilor_cardiace, 
     labels = alcool$Tara, pos = 3, cex = 0.6)

# 2. Coeficient de corelație
cor_alcool <- cor(alcool$Alcool_din_vin, alcool$Decese_datorate_afectiunilor_cardiace)
cat("\n============================================================\n")
cat("EXERCIȚIUL 1 - REZULTATE\n")
cat("============================================================\n")
cat("Coeficientul de corelație:", round(cor_alcool, 4), "\n\n")

# Test de semnificație
cor_test <- cor.test(alcool$Alcool_din_vin, alcool$Decese_datorate_afectiunilor_cardiace)
print(cor_test)

cat("\nINTERPRETARE:\n")
cat("Coeficientul de corelație este", round(cor_alcool, 4), "\n")
if(cor_alcool < 0) {
  cat("Există o corelație NEGATIVĂ (inversă) între consumul de alcool din vin\n")
  cat("și decesele cauzate de afecțiuni cardiace.\n")
  cat("Aceasta sugerează 'paradoxul francez' - țările cu consum mai mare de vin\n")
  cat("tind să aibă rate mai scăzute de decese cardiace.\n")
} else {
  cat("Există o corelație POZITIVĂ între consumul de alcool din vin\n")
  cat("și decesele cauzate de afecțiuni cardiace.\n")
}

if(cor_test$p.value < 0.05) {
  cat("Corelația este SEMNIFICATIVĂ statistic (p =", round(cor_test$p.value, 4), ")\n")
} else {
  cat("Corelația NU este semnificativă statistic (p =", round(cor_test$p.value, 4), ")\n")
}


# ============================================================================
# EXERCIȚIUL 2 - Relația IQ și nota la examen
# ============================================================================

# Încărcare date - încercăm mai multe variante
if(file.exists("IQ_dataset.txt")) {
  iq <- read.table("IQ_dataset.txt", header = TRUE)
} else if(file.exists("iq.dat")) {
  iq <- read.table("iq.dat", header = TRUE)
} else {
  # Creare manuală a datelor din document
  iq <- data.frame(
    Student = 1:12,
    IQ = c(110, 112, 118, 119, 122, 125, 127, 130, 132, 134, 136, 138),
    Nota = c(42.5, 53, 46, 61.75, 70.5, 56.5, 70.5, 60, 81, 70.5, 77.5, 88)
  )
  cat("\nDate create manual din documentul furnizat\n")
}

# Afișare primele rânduri pentru verificare
cat("\nPrimele rânduri din setul de date IQ:\n")
print(head(iq))

# 1. Diagrama de împrăștiere
plot(iq$IQ, iq$Nota, 
     xlab = "IQ", 
     ylab = "Nota la examen",
     main = "Relația dintre IQ și nota la examen",
     pch = 19, col = "red", cex = 1.2)

# 2. Model de regresie simplă
model_iq <- lm(Nota ~ IQ, data = iq)

cat("\n\n============================================================\n")
cat("EXERCIȚIUL 2 - REZULTATE\n")
cat("============================================================\n")
summary(model_iq)

# Adăugare dreaptă de regresie pe grafic
abline(model_iq, col = "blue", lwd = 2)

# Adăugare ecuația pe grafic
coef_iq <- coef(model_iq)
text(115, 85, 
     paste("y =", round(coef_iq[1], 2), "+", round(coef_iq[2], 2), "* x"),
     col = "blue", cex = 1.2)

# Predicții pentru IQ = 115 și IQ = 130
predicții <- predict(model_iq, newdata = data.frame(IQ = c(115, 130)))

cat("\n============================================================\n")
cat("PREDICȚII\n")
cat("============================================================\n")
cat("Nota estimată pentru IQ = 115:", round(predicții[1], 2), "\n")
cat("Nota estimată pentru IQ = 130:", round(predicții[2], 2), "\n")

# Vizualizare predicții pe grafic
points(c(115, 130), predicții, col = "green", pch = 17, cex = 1.5)
text(c(115, 130), predicții, 
     labels = paste("Pred:", round(predicții, 1)), 
     pos = 4, col = "green")


# ============================================================================
# EXERCIȚIUL 3 - Funcție pentru generarea observațiilor
# ============================================================================

genereaza_regresie <- function(m, a, b, xmin, xmax, sigma) {
  x <- runif(m, min = xmin, max = xmax)
  epsilon <- rnorm(m, mean = 0, sd = sigma)
  y <- a + b * x + epsilon
  return(data.frame(x = x, y = y))
}

cat("\n\n============================================================\n")
cat("EXERCIȚIUL 3 - Testare funcție genereaza_regresie\n")
cat("============================================================\n")

date_exemplu <- genereaza_regresie(m = 100, a = 10, b = 0.8, 
                                   xmin = -200, xmax = 200, sigma = 1.5)
cat("Primele 6 observații generate:\n")
print(head(date_exemplu))


# ============================================================================
# EXERCIȚIUL 4 - Funcție pentru estimarea coeficienților
# ============================================================================

estimeaza_regresie <- function(date) {
  model <- lm(y ~ x, data = date)
  coef_estimati <- coef(model)
  a_estimat <- coef_estimati[1]
  b_estimat <- coef_estimati[2]
  intervale <- confint(model, level = 0.95)
  
  rezultate <- list(
    a_estimat = a_estimat,
    b_estimat = b_estimat,
    IC_a = intervale[1, ],
    IC_b = intervale[2, ],
    model = model
  )
  
  return(rezultate)
}

cat("\n============================================================\n")
cat("EXERCIȚIUL 4 - Testare funcție estimeaza_regresie\n")
cat("============================================================\n")

test_estimare <- estimeaza_regresie(date_exemplu)
cat("Coeficienți estimați:\n")
cat("  â =", round(test_estimare$a_estimat, 4), "\n")
cat("  b̂ =", round(test_estimare$b_estimat, 4), "\n")
cat("Intervale de încredere 95%:\n")
cat("  â: [", round(test_estimare$IC_a[1], 4), ",", 
    round(test_estimare$IC_a[2], 4), "]\n")
cat("  b̂: [", round(test_estimare$IC_b[1], 4), ",", 
    round(test_estimare$IC_b[2], 4), "]\n")


# ============================================================================
# EXERCIȚIUL 5 - Aplicarea funcțiilor pentru diferite scenarii
# ============================================================================

a_real <- 10
b_real <- 0.8

analizeaza_scenariu <- function(nume_caz, m, xmin, xmax, sigma, a_real, b_real) {
  date <- genereaza_regresie(m, a_real, b_real, xmin, xmax, sigma)
  rezultate <- estimeaza_regresie(date)
  
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Scenariul ", nume_caz, "\n", sep = "")
  cat(rep("=", 60), "\n", sep = "")
  cat("m =", m, ", xmin =", xmin, ", xmax =", xmax, ", sigma =", sigma, "\n\n")
  cat("Coeficienți estimați:\n")
  cat("  â =", round(rezultate$a_estimat, 4), 
      " (adevărat: a =", a_real, ")\n")
  cat("  b̂ =", round(rezultate$b_estimat, 4), 
      " (adevărat: b =", b_real, ")\n\n")
  cat("Intervale de încredere 95%:\n")
  cat("  â: [", round(rezultate$IC_a[1], 4), ",", 
      round(rezultate$IC_a[2], 4), "]\n")
  cat("  b̂: [", round(rezultate$IC_b[1], 4), ",", 
      round(rezultate$IC_b[2], 4), "]\n")
  
  pdf(paste0("scenariu_", nume_caz, ".pdf"), width = 8, height = 6)
  
  plot(date$x, date$y, 
       xlab = "x", ylab = "y",
       main = paste("Scenariu", nume_caz, 
                    "\nm =", m, ", sigma =", sigma,
                    ", interval x: [", xmin, ",", xmax, "]"),
       pch = 19, col = "gray", cex = 0.8)
  
  x_vals <- seq(min(date$x), max(date$x), length.out = 100)
  lines(x_vals, a_real + b_real * x_vals, 
        col = "blue", lwd = 2, lty = 1)
  lines(x_vals, rezultate$a_estimat + rezultate$b_estimat * x_vals, 
        col = "red", lwd = 2, lty = 2)
  
  legend("topleft", 
         legend = c(paste("y =", a_real, "+", b_real, "* x (adevărat)"),
                    paste("y =", round(rezultate$a_estimat, 2), "+", 
                          round(rezultate$b_estimat, 2), "* x (estimat)")),
         col = c("blue", "red"), lwd = 2, lty = c(1, 2), cex = 0.9)
  
  dev.off()
  
  return(rezultate)
}

cat("\n\n")
cat(rep("=", 70), "\n", sep = "")
cat("EXERCIȚIUL 5 - Analiza celor 6 scenarii\n")
cat(rep("=", 70), "\n", sep = "")

set.seed(123)

scenarii <- list(
  a = list(m = 100, xmin = -200, xmax = 200, sigma = 1.5),
  b = list(m = 10, xmin = -5, xmax = 5, sigma = 1),
  c = list(m = 10000, xmin = -5, xmax = 5, sigma = 1),
  d = list(m = 10, xmin = 5, xmax = 5.2, sigma = 1),
  e = list(m = 10000, xmin = 5, xmax = 5.2, sigma = 1),
  f = list(m = 10, xmin = 5, xmax = 5.2, sigma = 0.01)
)

rezultate_toate <- list()
for (caz in names(scenarii)) {
  params <- scenarii[[caz]]
  rezultate_toate[[caz]] <- analizeaza_scenariu(
    caz, params$m, params$xmin, params$xmax, params$sigma, a_real, b_real
  )
}

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("REZUMAT FINAL - Comparație scenarii\n")
cat(rep("=", 70), "\n", sep = "")
for (caz in names(scenarii)) {
  params <- scenarii[[caz]]
  rez <- rezultate_toate[[caz]]
  cat("\nScenariu", caz, ": m =", params$m, ", interval x: [", 
      params$xmin, ",", params$xmax, "], sigma =", params$sigma, "\n")
  cat("  â =", round(rez$a_estimat, 4), 
      ", IC 95%: [", round(rez$IC_a[1], 4), ",", round(rez$IC_a[2], 4), "]\n")
  cat("  b̂ =", round(rez$b_estimat, 4), 
      ", IC 95%: [", round(rez$IC_b[1], 4), ",", round(rez$IC_b[2], 4), "]\n")
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("Fișierele PDF au fost generate în:", getwd(), "\n")
for (caz in names(scenarii)) {
  cat("  - scenariu_", caz, ".pdf\n", sep = "")
}
cat(rep("=", 70), "\n", sep = "")

