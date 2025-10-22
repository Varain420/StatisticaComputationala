
if (require(rstudioapi)) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}


cat("Directorul de lucru:", getwd(), "\n\n")



# EXERCITIUL 1


# Functie pentru afisarea graficului functiei cos(x) pe intervalul [a, b]
plot_cosinus <- function(a, b) {
  # Generam valori pentru x pe intervalul [a, b]
  x <- seq(a, b, length.out = 1000)
  # Calculam valorile functiei cos(x)
  y <- cos(x)
  
  # Cream graficul
  plot(x, y, 
       type = "l",           # tip linie
       col = "red",          # culoare rosie
       lwd = 2,              # grosime linie 2
       main = paste0("f(x) = cos(x), x in [", a, ", ", b, "]"),
       xlab = "x",
       ylab = "f(x)",
       las = 1)
  
  
  grid()
}

# Afisam graficul in Plots
plot_cosinus(-2*pi, 2*pi)


png("exercitiul1_cosinus.png", width = 800, height = 600, res = 100)
plot_cosinus(-2*pi, 2*pi)
dev.off()
cat("Salvat: exercitiul1_cosinus.png\n\n")


readline(prompt = "Apasa Enter pentru a continua la Exercitiul 2...")



# EXERCITIUL 2


# Refacem graficele pentru distributia binomiala B(n=20, x, p)
# pentru p = 0.1, 0.2, ..., 0.9

n <- 20
x <- 0:n

p_values <- seq(0.1, 0.9, by = 0.1)

for (p in p_values) {
  probabilitati <- dbinom(x, size = n, prob = p)
  
  # Afisam in Plots
  plot(x, probabilitati,
       type = "h",
       lwd = 3,
       col = "blue",
       main = paste0("Distributia Binomiala B(20, ", p, ")"),
       xlab = "x (numar de succese)",
       ylab = "Probabilitate",
       las = 1,
       ylim = c(0, max(probabilitati) * 1.1))
  
  points(x, probabilitati, pch = 19, col = "blue", cex = 1.2)
  grid()
  
  
  png(paste0("exercitiul2_binomial_n20_p", p, ".png"), 
      width = 800, height = 600, res = 100)
  
  plot(x, probabilitati,
       type = "h",
       lwd = 3,
       col = "blue",
       main = paste0("Distributia Binomiala B(20, ", p, ")"),
       xlab = "x (numar de succese)",
       ylab = "Probabilitate",
       las = 1,
       ylim = c(0, max(probabilitati) * 1.1))
  
  points(x, probabilitati, pch = 19, col = "blue", cex = 1.2)
  grid()
  
  dev.off()
  
  cat("Salvat: exercitiul2_binomial_n20_p", p, ".png\n", sep = "")
  
  
  Sys.sleep(0.5)
}

cat("\n")
readline(prompt = "Apasa Enter pentru a continua la Exercitiul 3...")



# EXERCITIUL 3


# Desenam graficele densitatii distributiei normale N(0, sigma^2)
# pentru sigma = 0.5, 1, 2 pe intervalul [-5, 5]

# Definim intervalul
x <- seq(-5, 5, length.out = 1000)

# Definim valorile pentru sigma
sigma_values <- c(0.5, 1, 2)
culori <- c("red", "blue", "green")
mu <- 0

# Initializam graficul cu prima distributie
y1 <- dnorm(x, mean = mu, sd = sigma_values[1])


plot(x, y1,
     type = "l",
     col = culori[1],
     lwd = 2,
     main = "Densitatea Distributiei Normale N(0, sigma^2)",
     xlab = "x",
     ylab = "Densitate",
     ylim = c(0, max(dnorm(x, mean = mu, sd = sigma_values[1]))),
     las = 1)

# Adaugam celelalte distributii
for (i in 2:length(sigma_values)) {
  y <- dnorm(x, mean = mu, sd = sigma_values[i])
  lines(x, y, col = culori[i], lwd = 2)
}


legend("topright",
       legend = paste0("sigma = ", sigma_values),
       col = culori,
       lwd = 2,
       bty = "n")


grid()


png("exercitiul3_normale.png", width = 800, height = 600, res = 100)

y1 <- dnorm(x, mean = mu, sd = sigma_values[1])

plot(x, y1,
     type = "l",
     col = culori[1],
     lwd = 2,
     main = "Densitatea Distributiei Normale N(0, sigma^2)",
     xlab = "x",
     ylab = "Densitate",
     ylim = c(0, max(dnorm(x, mean = mu, sd = sigma_values[1]))),
     las = 1)

for (i in 2:length(sigma_values)) {
  y <- dnorm(x, mean = mu, sd = sigma_values[i])
  lines(x, y, col = culori[i], lwd = 2)
}

legend("topright",
       legend = paste0("sigma = ", sigma_values),
       col = culori,
       lwd = 2,
       bty = "n")

grid()

dev.off()
cat("Salvat: exercitiul3_normale.png\n\n")

readline(prompt = "Apasa Enter pentru a continua la Exercitiul 4...")



# EXERCITIUL 4


# (a) 
# Functie CLT pentru distributia uniforma U[0, 20]

CLT <- function(n) {
  # Numarul de esantioane
  num_esantioane <- 1000
  
  # Vector pentru stocarea mediilor
  medii <- numeric(num_esantioane)
  
  # Generam 1000 de esantioane si calculam media fiecaruia
  for (i in 1:num_esantioane) {
    # Generam un esantion de dimensiune n din U[0, 20]
    esantion <- runif(n, min = 0, max = 20)
    # Calculam media esantionului
    medii[i] <- mean(esantion)
  }
  
  # Returnam vectorul mediilor
  return(medii)
}


# (b)
# Construim histogramele pentru n = 1, 5, 10, 100


n_values <- c(1, 5, 10, 100)


par(mfrow = c(2, 2))

for (n in n_values) {
  # Generam mediile folosind functia CLT
  medii <- CLT(n)
  
  
  hist(medii,
       breaks = 30,
       col = "lightblue",
       border = "black",
       main = paste0("Histograma mediilor (n = ", n, ")"),
       xlab = "Media esantionului",
       ylab = "Frecventa",
       las = 1,
       probability = TRUE)
  
  # Suprapunem curba densitatii normale teoretice
  mean_teoretic <- 10
  sd_teoretic <- sqrt((20^2 / 12) / n)
  
  curve(dnorm(x, mean = mean_teoretic, sd = sd_teoretic),
        col = "red",
        lwd = 2,
        add = TRUE)
  
  
  legend("topright",
         legend = c("Observat", "Normal teoretic"),
         fill = c("lightblue", NA),
         border = c("black", NA),
         lty = c(NA, 1),
         col = c(NA, "red"),
         lwd = c(NA, 2),
         bty = "n",
         cex = 0.8)
}

par(mfrow = c(1, 1))


png("exercitiul4b_histograme_uniforma.png", width = 1000, height = 1000, res = 100)

par(mfrow = c(2, 2))

for (n in n_values) {
  medii <- CLT(n)
  
  hist(medii,
       breaks = 30,
       col = "lightblue",
       border = "black",
       main = paste0("Histograma mediilor (n = ", n, ")"),
       xlab = "Media esantionului",
       ylab = "Frecventa",
       las = 1,
       probability = TRUE)
  
  mean_teoretic <- 10
  sd_teoretic <- sqrt((20^2 / 12) / n)
  
  curve(dnorm(x, mean = mean_teoretic, sd = sd_teoretic),
        col = "red",
        lwd = 2,
        add = TRUE)
  
  legend("topright",
         legend = c("Observat", "Normal teoretic"),
         fill = c("lightblue", NA),
         border = c("black", NA),
         lty = c(NA, 1),
         col = c(NA, "red"),
         lwd = c(NA, 2),
         bty = "n",
         cex = 0.8)
}

par(mfrow = c(1, 1))

dev.off()
cat("Salvat: exercitiul4b_histograme_uniforma.png\n\n")

readline(prompt = "Apasa Enter pentru a continua la Exercitiul 4(c)...")


# ----- Partea (c) -----
# Rescriem functia CLT pentru distributia binomiala B(x, 20, 0.9)

CLT_binomial <- function(n) {
  
  num_esantioane <- 1000
  
  
  medii <- numeric(num_esantioane)
  
  # Generam 1000 de esantioane si calculam media fiecaruia
  for (i in 1:num_esantioane) {
    # Generam un esantion de dimensiune n din B(20, 0.9)
    esantion <- rbinom(n, size = 20, prob = 0.9)
    # Calculam media esantionului
    medii[i] <- mean(esantion)
  }
  
  
  return(medii)
}


par(mfrow = c(2, 2))

for (n in n_values) {
  # Generam mediile folosind functia CLT_binomial
  medii <- CLT_binomial(n)
  
  
  hist(medii,
       breaks = 30,
       col = "lightgreen",
       border = "black",
       main = paste0("Histograma mediilor B(20, 0.9) (n = ", n, ")"),
       xlab = "Media esantionului",
       ylab = "Frecventa",
       las = 1,
       probability = TRUE)
  
  # Suprapunem curba densitatii normale teoretice
  mean_teoretic <- 20 * 0.9
  sd_teoretic <- sqrt((20 * 0.9 * 0.1) / n)
  
  curve(dnorm(x, mean = mean_teoretic, sd = sd_teoretic),
        col = "red",
        lwd = 2,
        add = TRUE)
  
  # Adaugam legenda
  legend("topright",
         legend = c("Observat", "Normal teoretic"),
         fill = c("lightgreen", NA),
         border = c("black", NA),
         lty = c(NA, 1),
         col = c(NA, "red"),
         lwd = c(NA, 2),
         bty = "n",
         cex = 0.8)
}

par(mfrow = c(1, 1))

# Salvam graficul ca PNG
png("exercitiul4c_histograme_binomiala.png", width = 1000, height = 1000, res = 100)

par(mfrow = c(2, 2))

for (n in n_values) {
  medii <- CLT_binomial(n)
  
  hist(medii,
       breaks = 30,
       col = "lightgreen",
       border = "black",
       main = paste0("Histograma mediilor B(20, 0.9) (n = ", n, ")"),
       xlab = "Media esantionului",
       ylab = "Frecventa",
       las = 1,
       probability = TRUE)
  
  mean_teoretic <- 20 * 0.9
  sd_teoretic <- sqrt((20 * 0.9 * 0.1) / n)
  
  curve(dnorm(x, mean = mean_teoretic, sd = sd_teoretic),
        col = "red",
        lwd = 2,
        add = TRUE)
  
  legend("topright",
         legend = c("Observat", "Normal teoretic"),
         fill = c("lightgreen", NA),
         border = c("black", NA),
         lty = c(NA, 1),
         col = c(NA, "red"),
         lwd = c(NA, 2),
         bty = "n",
         cex = 0.8)
}

par(mfrow = c(1, 1))

dev.off()
cat("Salvat: exercitiul4c_histograme_binomiala.png\n\n")




