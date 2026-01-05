devtools::load_all()

mm3 <- function(a, b, c) {
  declare(
    type(a = double(m, k)),
    type(b = double(k, n)),
    type(c = double(n, p))
  )
  ab <- a %*% b
  out <- ab %*% c
  out
}

xtx_scale <- function(x) {
  declare(type(x = double(NA, NA)))
  xtx <- crossprod(x)
  half_xtx <- 0.5 * xtx
  out <- xtx + half_xtx
  out
}

atb_c <- function(a, b, c) {
  declare(
    type(a = double(m, k)),
    type(b = double(n, k)),
    type(c = double(n, p))
  )
  atb <- a %*% t(b)
  out <- atb %*% c
  out
}

sum_of_products <- function(a, b, c, d) {
  declare(
    type(a = double(m, k)),
    type(b = double(k, n)),
    type(c = double(m, r)),
    type(d = double(r, n))
  )
  left <- a %*% b
  right <- c %*% d
  out <- left + right
  out
}

chain_plus <- function(a, b, c, j) {
  declare(
    type(a = double(m, k)),
    type(b = double(k, n)),
    type(c = double(n, p)),
    type(j = double(m, p))
  )
  q <- a %*% b %*% c + j
  q
}

chain_mix <- function(a, b, c) {
  declare(
    type(a = double(m, k)),
    type(b = double(n, k)),
    type(c = double(n, n))
  )
  q <- (a %*% t(b)) %*% c + 0.25 * (a %*% t(b))
  q
}

crossprod_plus <- function(x, y, j) {
  declare(
    type(x = double(m, k)),
    type(y = double(k, n)),
    type(j = double(k, n))
  )
  q <- crossprod(x) %*% y + j
  q
}

sum_of_products_line <- function(a, b, c, d, j) {
  declare(
    type(a = double(m, k)),
    type(b = double(k, n)),
    type(c = double(m, r)),
    type(d = double(r, n)),
    type(j = double(m, n))
  )
  q <- a %*% b + c %*% d + j
  q
}

bad_conformable <- function(a, b) {
  declare(
    type(a = double(m, k)),
    type(b = double(n, p))
  )
  a %*% b
}

cat("=== r2f: mm3 ===\n")
print(r2f(mm3))
cat("\n=== r2f: xtx_scale ===\n")
print(r2f(xtx_scale))
cat("\n=== r2f: atb_c ===\n")
print(r2f(atb_c))
cat("\n=== r2f: sum_of_products ===\n")
print(r2f(sum_of_products))
cat("\n=== r2f: chain_plus ===\n")
print(r2f(chain_plus))
cat("\n=== r2f: chain_mix ===\n")
print(r2f(chain_mix))
cat("\n=== r2f: crossprod_plus ===\n")
print(r2f(crossprod_plus))
cat("\n=== r2f: sum_of_products_line ===\n")
print(r2f(sum_of_products_line))
cat("\n=== r2f: bad_conformable (expect warning) ===\n")
print(r2f(bad_conformable))

set.seed(1)
m <- 80
k <- 60
n <- 40
p <- 50

a <- matrix(runif(m * k), m, k)
b <- matrix(runif(k * n), k, n)
c <- matrix(runif(n * p), n, p)
j <- matrix(runif(m * p), m, p)

x <- matrix(runif(120 * 80), 120, 80)

a2 <- matrix(runif(90 * 30), 90, 30)
b2 <- matrix(runif(70 * 30), 70, 30)
c2 <- matrix(runif(70 * 40), 70, 40)

c_left <- matrix(runif(80 * 20), 80, 20)
d_right <- matrix(runif(20 * 50), 20, 50)
a_sp <- matrix(runif(80 * 60), 80, 60)
b_sp <- matrix(runif(60 * 50), 60, 50)
c_sp <- matrix(runif(80 * 20), 80, 20)
d_sp <- matrix(runif(20 * 50), 20, 50)

b_chain <- matrix(runif(55 * 30), 55, 30)
c_chain <- matrix(runif(55 * 55), 55, 55)
a_chain <- matrix(runif(90 * 30), 90, 30)

x_cp <- matrix(runif(200 * 70), 200, 70)
y_cp <- matrix(runif(70 * 60), 70, 60)
j_cp <- matrix(runif(70 * 60), 70, 60)

a_sum <- matrix(runif(100 * 80), 100, 80)
b_sum <- matrix(runif(80 * 60), 80, 60)
c_sum <- matrix(runif(100 * 30), 100, 30)
d_sum <- matrix(runif(30 * 60), 30, 60)
j_sum <- matrix(runif(100 * 60), 100, 60)

q_mm3 <- quick(mm3)
q_xtx_scale <- quick(xtx_scale)
q_atb_c <- quick(atb_c)
q_sum_of_products <- quick(sum_of_products)
q_chain_plus <- quick(chain_plus)
q_chain_mix <- quick(chain_mix)
q_crossprod_plus <- quick(crossprod_plus)
q_sum_of_products_line <- quick(sum_of_products_line)

cat("\n=== bench: mm3 ===\n")
print(bench::mark(
  mm3(a, b, c),
  q_mm3(a, b, c),
  check = TRUE
))

cat("\n=== bench: xtx_scale ===\n")
print(bench::mark(
  xtx_scale(x),
  q_xtx_scale(x),
  check = TRUE
))

cat("\n=== bench: atb_c ===\n")
print(bench::mark(
  atb_c(a2, b2, c2),
  q_atb_c(a2, b2, c2),
  check = TRUE
))

cat("\n=== bench: sum_of_products ===\n")
print(bench::mark(
  sum_of_products(a_sp, b_sp, c_sp, d_sp),
  q_sum_of_products(a_sp, b_sp, c_sp, d_sp),
  check = TRUE
))

cat("\n=== bench: chain_plus ===\n")
print(bench::mark(
  chain_plus(a, b, c, j),
  q_chain_plus(a, b, c, j),
  check = TRUE
))

cat("\n=== bench: chain_mix ===\n")
print(bench::mark(
  chain_mix(a_chain, b_chain, c_chain),
  q_chain_mix(a_chain, b_chain, c_chain),
  check = TRUE
))

cat("\n=== bench: crossprod_plus ===\n")
print(bench::mark(
  crossprod_plus(x_cp, y_cp, j_cp),
  q_crossprod_plus(x_cp, y_cp, j_cp),
  check = TRUE
))

cat("\n=== bench: sum_of_products_line ===\n")
print(bench::mark(
  sum_of_products_line(a_sum, b_sum, c_sum, d_sum, j_sum),
  q_sum_of_products_line(a_sum, b_sum, c_sum, d_sum, j_sum),
  check = TRUE
))
