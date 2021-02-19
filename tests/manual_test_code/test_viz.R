ggplot(data = df_model, aes(x = date, y = close)) +
  geom_point(aes(color = training)) + geom_line(aes(y = fitted_actual)) +
  geom_ribbon(aes(ymin = lo_95, ymax = hi_95, alpha = 0.2))

sweep::sw_glance(fit)
