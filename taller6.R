x1 <- rnorm(20000, 15, 10) 
x2 <- rnorm(20000, 3, 91)
e <- rnorm(20000, 0, 1)
y <- 1 + 5 * x1 - 8 * x2 + e
n1 <- 200
n2 <- 5000

df <- data.frame("y"=y,
                 "x1"=x1,
                 "x2"=x2)
# consistentes e insesgados
b1_200 <- c()
b2_200 <- c()
t1_200 <- c()
t2_200 <- c()
p1_200 <- c()
p2_200 <- c()
ici1_200 <- c()
ics1_200 <- c()
ici2_200 <- c()
ics2_200 <- c()

# sesgados e inconsistentes
b2_b_200 <- c()
t2_b_200 <- c()
p2_b_200 <- c()
ici_b_200 <- c()
ics_b_200 <- c()

for (i in 1:200){
  muestra <- df[sample(1:dim(df)[1], 200), ]
  reg_full_1 <- lm(y ~ x1 + x2, data=muestra)
  reg_bias_1 <- lm(y ~ x2, data=muestra)
  b1_200 <- c(reg_full_1$coefficients["x1"], b1_200)
  b2_200 <- c(reg_full_1$coefficients["x2"], b2_200)
  b2_b_200 <- c(reg_bias_1$coefficients["x2"], b2_b_200)
}

# consistentes y e insesgados
b1_5000 <- c()
b2_5000 <- c()
t1_5000 <- c()
t2_5000 <- c()
p1_5000 <- c()
p2_5000 <- c()
ici1_5000 <- c()
ics1_5000 <- c()
ici2_5000 <- c()
ics2_5000 <- c()

# sesgados e inconsistentes
b2_b_5000 <- c()
t2_b_5000 <- c()
p2_b_5000 <- c()
ici_b_5000 <- c()
ics_b_5000 <- c()

for (i in 1:200){
  muestra <- df[sample(1:dim(df)[1], 5000), ]
  reg_full_2 <- lm(y ~ x1 + x2, data=muestra)
  reg_bias_2 <- lm(y ~ x2, data=muestra)
  b1_5000 <- c(reg_full_2$coefficients["x1"], b1_5000)
  b2_5000 <- c(reg_full_2$coefficients["x2"], b2_5000)
  b2_b_5000 <- c(reg_bias_2$coefficients["x2"], b2_b_5000)
}

plot(density(b1_200))
lines(density(b1_5000))

# guardar estadísticos t (summary, coefficients, t value)
# guardar p valor (summary, coefficients, Pr(>|t|))
# guardar intervalos de confianza (confint(reg) 2.5% 97.5%)
# tanto para sesgadas e inconsistentes como para los otros
# construir un resumen de los estimadores
# construir un resumen de los estadísticos
