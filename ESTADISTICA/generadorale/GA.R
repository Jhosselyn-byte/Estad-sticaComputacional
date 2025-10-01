# 1.1 LCG
lcg_generator <- function(n, seed = 12345) {
  x <- seed
  vals <- numeric(n)
  for (i in 1:n) {
    x <- (1664525 * x + 1013904223) %% 2^32
    vals[i] <- x / 2^32
  }
  vals
}
# 1.2 Mersenne Twister (wrapper)
mt_generator <- function(n, seed = 12345) {
  set.seed(seed)
  runif(n)
}
# 1.3 Xorshift
xorshift_generator <- function(n, seed = 12345) {
  x <- as.integer(seed)
  vals <- numeric(n)
  for (i in 1:n) {
    x <- bitwXor(x, bitwShiftL(x, 13))
    x <- bitwXor(x, bitwShiftR(x, 17))
    x <- bitwXor(x, bitwShiftL(x, 5))
    vals[i] <- (x + 2^31) / 2^32
  }
  vals
}
#Chi-cuadrado 
chi_test <- function(vals, k = 100, name) {
  n <- length(vals)
  obs <- table(cut(vals, breaks = seq(0, 1, length.out = k + 1)))
  exp <- n / k
  stat <- sum((obs - exp)^2 / exp)
  pval <- pchisq(stat, df = k - 1, lower.tail = FALSE)
  cat(sprintf("\n=== Chi² - %s ===\n", name))
  cat(sprintf("Estadístico: %.4f | p-valor: %.4f | intervalos: %d\n", stat, pval, k))
}
#generador para 1 millon
n <- 1e6
lcg_vals <- lcg_generator(n)
mt_vals  <- mt_generator(n)
xor_vals <- xorshift_generator(n)
#se visualiza
chi_test(lcg_vals, name = "LCG")
chi_test(mt_vals, name = "Mersenne Twister")
chi_test(xor_vals, name = "Xorshift")
#utilizando el profvis para visualizar cuanta memoria esta utilizando
profvis({
  lcg_generator(1e6)
  mt_generator(1e6)
  xorshift_generator(1e6)
})

