ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data,
             aes(x = arid_index, y = evap_index, color = factor(KG)),
             alpha = 1) +
  scale_colour_hue(l = 45) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6

ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 15,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 45) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 26,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 45) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 29 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 45) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 29 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 14 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 0 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))




# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 25 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 7 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 27 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 8 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 9 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 18 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))

# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 19 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 17 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 4 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 5 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))

# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 6 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))





