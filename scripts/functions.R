






# Définition de la fonction "GetHeight" pour calculer la hauteur en fonction du diamètre et de l'altitude
GetHeight <- function(diam, alt){
  # Déterminer la catégorie d'altitude
  categorie <- ifelse(alt >= 1250 & alt < 1500, 1,
                      ifelse(alt >= 1500 & alt < 1800, 2,
                             ifelse(alt >= 1800 & alt < 2400, 3,
                                    ifelse(alt >= 2400 & alt <= 2600, 4, NA))))
  
  # Appliquer la formule en fonction de la catégorie
  height <- ifelse(!is.na(categorie), 
                   ifelse(categorie == 1, 30.61*exp(-2.7*exp(-0.95*diam)),
                          ifelse(categorie == 2, 30*exp(3.2*exp(-0.94*diam)),
                                 ifelse(categorie == 3, 22.7-24.41*exp(-exp(-3.3)*diam),
                                        ifelse(categorie == 4, -15.26+11.57*log(diam)-1.17*((log(diam)^2)), NA)))),
                   NA)
  
  return(round(height, 2))
}


# Calcul du volume de chaque arbre en fonction de la circonférence et de la hauteur
getvolume <- function(circonference, height){
  # Cette formule permet de calculer le volume par arbre
  volume = (((circonference^2)/100)*height)/4*3.14
  
  return(round(volume, 2))
}


# Définition d'une fonction pour calculer la surface terrière en m²/ha en fonction du diamètre
getBasalArea <- function(diam){
  # Cette fonction calcule la surface terrière en m²/ha
  BasalArea = (3.14*(diam*0.01)^2)/4
  
  return(round(BasalArea, 2))
}


# Définition d'une fonction pour obtenir la densité du bois en fonction du genre et de l'épithète de l'arbre
getWooDen <- function(genus, species, region = "World" ){
  # Load Wood density
  density <- getWoodDensity(
    genus = genus,
    species = species,
    stand = NULL,
    family = NULL,
    region = region,
    addWoodDensityData = NULL,
    verbose = TRUE
  )
  
  meanWD = density$meanWD
  # return(meanWD = density$meanWD)
  return(round(meanWD, 2))
}


# Définition d'une fonction pour calculer la biomasse des arbres en fonction du diamètre, de la hauteur et de la densité
GetBiomass <- function(diam, height, density){
  # diam : Diamètre en cm
  # height : Hauteur en m
  
  biomass_en_kg = density * exp(-2.977 + log(density * (diam*1)^2 * height))
  
  return(round(biomass_en_kg, 2))
}


#Getpresence_absence
Getpresence <- function(data, especes, sites){
  
  #data : dataframme
  #species : species' column
  #sites : sites' column
  
  # Création du dataframe de présence/absence
  presence_absence <- data %>%
    group_by(especes, sites) %>%
    summarise(presence = 1) %>%
    pivot_wider(names_from = sites, values_from = presence, values_fill = 0) %>%
    ungroup()
  
  #Réordonner les colonnes par ordre alphabetique
  presence_absence <- presence_absence %>%
    select(especes, everything()) %>%
    
    return(presence_absence)
}


#occurence
GetOccurence <- function(df, espece, sites){
  result <- df %>%
    group_by({{espece}}, {{sites}}) %>%
    summarise(NombreOccurrences = n()) %>%
    ungroup()
  # Utilisez pivot_wider pour obtenir un tableau à deux entrées
  OccuTab <- pivot_wider(result, names_from = {{sites}}, values_from = NombreOccurrences, values_fill = 0)
  # Remplacez les valeurs NA par 0
  OccuTab[is.na(OccuTab)] <- 0
  
  return(OccuTab)
}


# Fonction pour créer les diagrammes de tranches (histogrammes) facettés
create_histogram_faceted <- function(data, ncol, groupe, facet.labs, ylab, xlab, titre, sous_titre, sourcE) {
  filtered_data <- data[data$groupe == groupe, ]
  
  ggplot(filtered_data, aes(x = cut(`DIAMETRE EN CM`, breaks = c(10, 20, 30, 40, 50, 80, 100, 300), labels = c("11-20", "21-30", "31-40", "41-50", "51-80", "81-100", "101-300")), fill = METHODES)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Histogramme des arbres pour le groupe", groupe),
         x = "Diamètre",
         y = "Nombre d'arbres") +
    facet_wrap(~ METHODES, ncol = ncol) +
    scale_color_ipsum( ) +
    scale_fill_ipsum( ) +
    theme_ipsum_rc() +
    labs(y = ylab, x = xlab,
         title = titre,
         subtitle = sous_titre,
         caption = sourcE) +
    theme(legend.position = "none", axis.text.x = element_text(size = 8)) -> gg
  
  return(gg)
}


