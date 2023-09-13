# Script pour faire des calculs d'iventaire

Ce repository contient un script R pour générer un rapport d'inventaire forestier pour le Parc National de Kahuzi-Biega. Le rapport comprend des analyses des données d'inventaire, des visualisations et des calculs de biomasse, de densité et plus encore.

## Table des matières

- [Installation](#installation)
- [Utilisation](#utilisation)
- [Exemples](#exemples)
- [Contribuer](#contribuer)
- [Licence](#licence)

## Installation

Pour utiliser ce script, vous devez suivre ces étapes d'installation :

1. Clonez ce dépôt GitHub sur votre machine locale :

   ```bash
   git clone https://github.com/votre-utilisateur/parc-kahuzi-biega-inventaire.git

Assurez-vous d'avoir R installé sur votre système.
Installez les packages R nécessaires en exécutant le script

`install_packages.R` :

   ```bash
      Rscript install_packages.R
```

# Utilisation
Pour générer les calculs d'inventaire, suivez ces étapes simples :

Placez vos données d'inventaire au format Excel dans le répertoire Data.
Ouvrez le script R scrip_inventaire.R avec RStudio ou un autre environnement R.

Exécutez le script en suivant les commentaires pour personnaliser les paramètres et générer vos résultats.
Rendez-vous dans le dossier output pour voir vos résultats.

Ce script R est conçu pour automatiser l'analyse des données d'inventaire forestier pour le parc Kahuzi-Biega. Il prend en charge le calcul de diverses variables, la création de graphiques pour la visualisation des données, et l'exportation des résultats dans un fichier Excel.

## Initialisation et Nettoyage

Le script commence par initialiser l'environnement de travail en effaçant la mémoire et en chargeant les bibliothèques nécessaires.

## Importation des Données

Les données d'inventaire sont importées à partir d'un fichier Excel nommé "data.xlsx" et stockées dans un objet nommé "data".

## Calcul des Variables

Plusieurs variables sont calculées, notamment la hauteur des arbres, le volume, la surface terrière, la densité du bois, la biomasse, etc.

## Diagrammes de Tranches

Des diagrammes de tranches (histogrammes) sont générés pour visualiser la distribution des diamètres d'arbres.

## Analyse Statistique

Des analyses statistiques sont effectuées pour examiner le nombre d'espèces et d'individus par groupe.

## Liste des Espèces Uniques

La liste des espèces uniques et des genres uniques dans les données est extraite.

## Tableau des Occurrences

Un tableau des occurrences d'espèces en fonction de la méthode d'inventaire est créé.

## Analyse des Données

L'analyse des données comprend le calcul de la densité d'espèces et de genres par hectare, ainsi que des statistiques sur le volume, la surface terrière et la biomasse par hectare.

## Exportation des Données

Les données agrégées sont exportées dans un fichier Excel nommé "myData3.xlsx" avec plusieurs feuilles.





Pour utiliser ce script, assurez-vous d'avoir les bibliothèques R nécessaires installées. Suivez les commentaires dans le script pour personnaliser les paramètres selon vos besoins.


