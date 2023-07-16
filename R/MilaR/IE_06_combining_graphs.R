#### patchworking ####

plot_total <- ((p3 / p6) | (p2 / p1 / p5) | (p4 / p7))+ 
  plot_annotation(tag_levels = "A")

ggsave("data/output/IE_total_300.png", plot = plot_total, width=650, height=700, units = "mm", dpi = 300)