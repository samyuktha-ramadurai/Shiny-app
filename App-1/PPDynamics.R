library(deSolve)

PPDynamics <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <-  a * X - b * X * Y
    dY <-  -g * Y + d * X * Y
    list(c(dX, dY))
  })
}

# x = prey population density
# dx (dx/dt) = growth rate of prey population
# y = predator population density
# dy (dy/dt) = predator population growth rate
# a = maximum growth rate of prey population
# b = effect of predator on prey population
# g = death rate of predator population
# d = predator growth rate per prey eaten

parameters <- c(a = 2/3, b = 4/3, g = 0.5, d = 1)
state      <- c(X = 1, Y = 1)
times      <- seq(0, 100, by = 0.01)

out <- ode(y = state, times = times, func = PPDynamics, parms = parameters)

print(out)
result <- PPDynamics()
print(result)

out_df <- as.data.frame(out)
plot(out_df$time, out_df$X, type = "l", col = "blue", lwd = 2,  xlab = "Time", ylab = "Population Density", main = "Predator-Prey Dynamics")
lines(out_df$time, out_df$Y, col = "red")

#plot(out)
#plot(out[,2], out[,3]) # Prey vs Predator


# Reshaping data for ggplot
out_long <- pivot_longer(out_df,
                         cols = c("X", "Y"),
                         names_to = "Population",
                         values_to = "Density")

print(out_long)

PPDynamicsPlot <- ggplot(out_long, aes(x = time, y = Density, color = Population)) +
  geom_line() +
  labs(title = "Predator-Prey Population Dynamics",
       x = "Time",
       y = "Population Density") +
  theme_minimal() +
  scale_color_manual(values = c("X" = "blue", "Y" = "red"))
PPDynamicsPlot
