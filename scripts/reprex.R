library(reprex)
library(tibble)

# Existing Data
df <- list(
  list(
    a = tibble(id = rownames(mtcars)[1]),
    b = tibble(mtcars[1:3, 2:4]),
    c = tibble(mtcars[1:3, 5:7])
  ),
  list(
    a = tibble(rownames(mtcars)[2]),
    b = tibble(mtcars[4:6, 2:4]),
    c = tibble(mtcars[4:6, 5:7])
  )
)

# What I Want

a <- tibble(id = rownames(mtcars)[1:2])
b <- tibble(id = c(rep(a$id[1], 3), 
                   rep(a$id[2], 3)), 
            mtcars[1:6, 2:4])
c <- tibble(id = c(rep(a$id[1], 3), 
                   rep(a$id[2], 3)),
            mtcars[1:6, 5:7])



