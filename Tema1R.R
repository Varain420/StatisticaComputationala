#==================================================================
#               RECAPITULARE COD R - EXERCIȚII
#==================================================================

# Șterge toate variabilele din mediul de lucru pentru o pornire curată
rm(list = ls())


#----------------------------------------------------
# Exercitiu 2
#----------------------------------------------------
print("================== REZULTATE EXERCIȚIUL 2 ==================")

# Definirea vectorului x
x <- c(1, 9, 2, 6, 2, 7, 8, 7, 5, 5)
print("Vectorul x:")
print(x)

# a. Calculul mediei
media_x <- mean(x)
print("a. Media vectorului x:")
print(media_x)

# b. Logaritmul natural pentru fiecare element
log_x <- log(x)
print("b. Logaritmul natural al elementelor lui x:")
print(log_x)

# c. Diferenta dintre maxim si minim
diferenta_max_min <- max(x) - min(x)
print("c. Diferenta dintre maxim si minim:")
print(diferenta_max_min)

# d. Calculul vectorului y
y <- (x - 5.2) / 2.740641
print("d. Vectorul y:")
print(y)

# e. Media si deviatia standard a vectorului y
media_y <- mean(y)
sd_y <- sd(y)
print("e. Media vectorului y:")
print(media_y)
print("e. Deviatia standard a vectorului y:")
print(sd_y)
print(" ") # Linie goală pentru lizibilitate


#----------------------------------------------------
# Exercitiu 3
#----------------------------------------------------
print("================== REZULTATE EXERCIȚIUL 3 ==================")

# Introducerea datelor intr-o variabila numita factura
factura <- c(46, 33, 49, 37, 36, 50, 58, 32, 49, 35, 30, 58)
print("Vectorul 'factura':")
print(factura)

# Cuantumul anual al facturii
total_anual <- sum(factura)
print("Cuantumul anual al facturii:")
print(total_anual)

# Cea mai mica si cea mai mare valoare platita
valoare_minima <- min(factura)
valoare_maxima <- max(factura)
print("Cea mai mica valoare platita intr-o luna:")
print(valoare_minima)
print("Cea mai mare valoare platita intr-o luna:")
print(valoare_maxima)

# Numarul de luni in care factura a depasit 40
numar_luni_peste_40 <- sum(factura > 40)
print("Numarul de luni cu factura peste 40:")
print(numar_luni_peste_40)

# Procentul acestor luni din total
procent_luni_peste_40 <- (numar_luni_peste_40 / length(factura)) * 100
print("Procentul lunilor cu factura peste 40 (%):")
print(procent_luni_peste_40)
print(" ") # Linie goală pentru lizibilitate


#----------------------------------------------------
# Exercitiu 4
#----------------------------------------------------
print("================== REZULTATE EXERCIȚIUL 4 ==================")

# Citirea unui vector cu 7 numere de la tastatura
print(">>> Acțiune necesară: Introduceți 7 numere reale separate prin spațiu sau Enter, apoi apăsați Enter din nou.")
vector_citit <- scan(n = 7)

# Afisarea vectorului citit pentru verificare
print("Vectorul pe care l-ati introdus este:")
print(vector_citit)

# a. Calculati maximul, minimul, media, mediana si abaterea standard
print("a. Statisticile descriptive:")
print(paste("Maximul:", max(vector_citit)))
print(paste("Minimul:", min(vector_citit)))
print(paste("Media:", mean(vector_citit)))
print(paste("Mediana:", median(vector_citit)))
print(paste("Abaterea standard:", sd(vector_citit)))

# b. Sortati vectorul citit
vector_sortat <- sort(vector_citit)
print("b. Vectorul sortat:")
print(vector_sortat)

# c. Standardizati vectorul
vector_standardizat <- scale(vector_citit)
print("c. Vectorul standardizat (scoruri Z):")
print(vector_standardizat)
print("================== SFÂRȘIT SCRIPT ==================")

