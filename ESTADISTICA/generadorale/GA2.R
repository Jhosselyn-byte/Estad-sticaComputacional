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
#Kolmogorov-Smirnov
ks_test <- function(vals, name) {
  test_result <- ks.test(vals, "punif", 0, 1)
  cat(sprintf("\n=== KS - %s ===\n", name))
  cat(sprintf("Estadístico D: %.6f | p-valor: %.4f\n", 
              test_result$statistic, test_result$p.value))
}
#generador para 1 millon
n <- 1e6
lcg_vals <- lcg_generator(n)
mt_vals  <- mt_generator(n)
xor_vals <- xorshift_generator(n)
#Se visualiza
ks_test(lcg_vals, "LCG")
ks_test(mt_vals, "Mersenne Twister")
ks_test(xor_vals, "Xorshift")
#utilizando el profvis para visualizar cuanta memoria esta utilizando
profvis({
  lcg_generator(1e6)
  mt_generator(1e6)
  xorshift_generator(1e6)
})

