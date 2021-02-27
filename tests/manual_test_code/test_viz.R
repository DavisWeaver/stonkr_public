library(ggplot2)

df_model <- renarin_short("DBX", look_ahead = 30, zoom_in = FALSE)

ggplot(data = df_model, aes(x = date, y = fitted_actual)) +
  geom_point(aes(y = close)) + geom_line(aes(y = fitted_actual)) +
  geom_ribbon(aes(ymin = lo_95, ymax = hi_95, alpha = 0.2))

sweep::sw_glance(fit)
