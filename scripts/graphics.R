









# Diagramme de tranches pour le groupe "Transect"
transect_graph <- create_histogram_faceted(data = data,
                                           groupe = "Transect",
                                           facet.labs = c(
                                             "T1" = "Premier 100m",
                                             "T2" = "Deuxième 100m",
                                             "T4" = "Quatrième 100m",
                                             "T5" = "Cinquième 100m",
                                             "T6" = "Sixième 100m"),
                                           ylab = "Nombres d'arbres",
                                           xlab = "Diamètre (en cm)",
                                           titre = "Diagramme de tranches de diamètre",
                                           sous_titre = "Transect",
                                           sourcE = "Source: Données d'inventaire",
                                           ncol = 3)

# Diagramme de tranches pour le groupe "Placeau"
placeau_graph <- create_histogram_faceted(data = data,
                                          groupe = "Placeau",
                                          ncol = 2,
                                          facet.labs = c("P2" = "Placeau 2",
                                                         "P1" = "Placeau 1"),
                                          ylab = "Nombres d'arbres",
                                          xlab = "Diamètre (en cm)",
                                          titre = "Diagramme de tranches de diamètre",
                                          sous_titre = "Placeaux",
                                          sourcE = "Source: Données d'inventaire")

# Diagramme de tranches pour l'ensemble des données
tout_graph <- ggplot(data, aes(x = cut(`DIAMETRE EN CM`, breaks = c(10, 20, 30, 40, 50, 80, 100, 300), labels = c("11-20", "21-30", "31-40", "41-50", "51-80", "81-100", "101-300")), fill = groupe)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ groupe, ncol = 2) +
  scale_color_ipsum( ) +
  scale_fill_ipsum( ) +
  theme_ipsum_rc() +
  labs(y = "Nombres d'arbres", x = "Diamètre (en cm)",
       title = "Diagramme de tranches",
       subtitle = "En fonction de la méthode",
       caption = "Source : Données d'inventaire") +
  theme(legend.position = "none", axis.text.x = element_text(size = 8))

# Enregistrement des graphiques en tant qu'images PNG
ggsave(filename = "imgs/transect.png",
       plot = transect_graph,
       width = 10,
       height = 6,
       bg = "white")

ggsave(filename = "imgs/placeau.png",
       plot = placeau_graph,
       width = 10,
       height = 6,
       bg = "white")

ggsave(filename = "imgs/tout_graph.png",
       plot = tout_graph,
       width = 10,
       height = 6,
       bg = "white")
