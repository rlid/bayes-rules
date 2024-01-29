# Ex 2.13
pi <- c(0.4, 0.5, 0.6, 0.7)
prior <- c(0.1, 0.2, 0.44, 0.26)

n <- 80
k <- 47

posterior <- choose(n, k) * pi^k * (1-pi)^(n-k) * prior
posterior <- posterior / sum(posterior)
posterior


# Ex 2.15
pi <- c(0.6, 0.65, 0.7, 0.75)
prior <- c(0.3, 0.4, 0.2, 0.1)

n <- 15
k <- 10

posterior <- choose(n, k) * pi^k * (1-pi)^(n-k) * prior
posterior <- posterior / sum(posterior)
posterior


# Ex 2.17

p_infected <- 0.18
n_sim <- 1000000

trees <- data.frame(type = c("Infected", "Not Infected"))
prior <- c(p_infected, 1 - p_infected)

trees_sim <- sample_n(trees, size = n_sim, weight = prior, replace = TRUE)

trees_sim <- trees_sim %>% mutate(elms = case_when(type == "Infected" ~ 0.15,
                                                   type == "Not Infected" ~ 0.2),
                                  maples = case_when(type == "Infected" ~ 0.8,
                                                     type == "Not Infected" ~ 0.1))

species <- c("Elm", "Maple", "Other")
trees_sim <- trees_sim %>% group_by(1:n()) %>% mutate(species = sample(species, size = 1, prob = c(elms, maples, 1 - elms - maples)))

trees_sim %>% filter(species == "Maple") %>% tabyl(type)


# Ex 2.18
n_sim <- 1000000
pi <- data.frame(pi = c(0.4, 0.5, 0.6, 0.7))
prior <- c(0.1, 0.2, 0.44, 0.26)

pi_sim <- sample_n(pi, size = n_sim, weight = prior, replace = TRUE)

n <- 80
k <- 47

intol_sim <- pi_sim %>% mutate(y = rbinom(n_sim, size = n, prob = pi))
intol_sim %>% filter(y == k) %>% tabyl(pi)


# Ex 2.19
n_sim <- 1000000
pi <- data.frame(pi = c(0.6, 0.65, 0.7, 0.75))
prior <- c(0.3, 0.4, 0.2, 0.1)

pi_sim <- sample_n(pi, size = n_sim, weight = prior, replace = TRUE)

n <- 15
k <- 10

intol_sim <- pi_sim %>% mutate(y = rbinom(n_sim, size = n, prob = pi))
intol_sim %>% filter(y == k) %>% tabyl(pi)


# Ex 2.20
p_cat <- 0.08
n_sim <- 100000

images <- data.frame(type = c("Cat", "Not Cat"))
prior <- c(p_cat, 1 - p_cat)

images_sim <- sample_n(images, size = n_sim, weight = prior, replace = TRUE)

images_sim <- images_sim %>% mutate(prob_cat = case_when(type == "Cat" ~ 0.8,
                                                         type == "Not Cat" ~ 0.5))

labels <- c(TRUE, FALSE)
images_sim <- images_sim %>% group_by(1:n()) %>% mutate(pred_cat = sample(labels, size = 1, prob = c(prob_cat, 1 - prob_cat)))

images_sim %>% filter(pred_cat) %>% tabyl(type)


# Ex 2.21
p_disease <- 0.03
n_sim <- 100000

tests <- data.frame(type = c(TRUE, FALSE))
prior <- c(p_disease, 1 - p_disease)

tests_sim <- sample_n(tests, size = n_sim, weight = prior, replace = TRUE)

tests_sim <- tests_sim %>% mutate(prob = case_when(type ~ 0.93,
                                                   !type ~ 0.07))

labels <- c(TRUE, FALSE)
tests_sim <- tests_sim %>% group_by(1:n()) %>% mutate(pred = sample(labels, size = 1, prob = c(prob, 1 - prob)))

tests_sim %>% filter(pred) %>% tabyl(type)