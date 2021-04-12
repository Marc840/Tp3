#R pour scientifique tp3 
#Stephanie Kelley et Marc-Antoine Chiasson

#Lecture des 4 fichiers 
#questionnaire1 + reponses
library(jsonlite)
questionnaire_service_clients <- fromJSON("data-raw/questionnaire_service_clients.json")
str(questionnaire_service_clients)
reponses_service_clients<-read.csv("data-raw/reponses_service_clients_2021_02.csv",sep=";")
#questionnaire2 + reponse
questionnaire_performances_responsables <- fromJSON("data-raw/questionnaire_performances_responsables.json")
str(questionnaire_performances_responsables)
reponses_performances_responsables<-read.csv("data-raw/reponses_performances_responsables_2020.csv",sep=";")

#Étape 1 ecrire les fonctions

#' Lecture de fichier JSON
#' 
#' Lecture d'un questionnaire de type JSON
#' 
#' @param fichier le chemin ou le questionnaire se trouve 
#' @return une seule valeur : une liste qui contient un element par question
#' @author Stephanie kelley
#' @export
#' @examples
#' lecture_fichier_JSON("data-raw/questionnaire_service_clients.json")
lecture_fichier_JSON<-function(chemin){
  fichier <- fromJSON("chemin")
  liste<-list()
  i=1
  while (i <= length(row.names(questionnaire_service_clients))){    
    liste[["id"]]<-questionnaire_service_clients$id[1]
    liste[["question"]]<-questionnaire_service_clients$question[1]  
    liste[["choix"]]<-questionnaire_service_clients$choix[1]
    print(liste)
    i= i+1
  }
  if( reponse = NULL) { #tu veux faire quoi ici? parce que reponse est déja NULL pour que ton if 
    reponse<-NULL 
  }
  return(liste)
}

#' Méthode print pour la fonction utilitaire lecture_fichier_JSON
#' 
#' Impression qui mentionne que le fichier est un questionnaire et qui mentionne le nombre de questions de types choix de reponses ou ouvertes
#' 
#' @param fichier le fichier qu'on veut imprimer 
#' @param ... 
#' @return ??? cest pas un return 
#' @author Stephanie Kelley
#' @export
#' @examples
#' lecture_fichier_JSON.print(questionnaire_service_clients)
lecture_fichier_JSON.print<-function(fichier,...){
  x<-sum(fichier$choix == "NULL")
  y=length(row.names(fichier)) - x
  cat("Voici un questionnaire :  \n Il contient", y ,"questions à choix de réponse et",
      x ,"questions ouvertes.")
}

#' Methode summary pour la fonction utilitaire lecture_fichier_JSON
#' 
#' Affiche les libelles de toutes les questions du questionnaire 
#' 
#' @param object l'objet pour lequel on veut le summary
#' @param ... arguments additionnels
#' @param impression True si l'utilisateur veut afficher les choix de reponses aux questions
#' @return une seule valeur : 
#' @author Stephanie Kelley
#' @export
#' @examples
#' lecture_fichier_JSON.summary()
lecture_fichier_JSON.summary<-function(object,...,impression=FALSE){
  if (is.nullimpression & object$choix){
    print(list(object$question , object$choix))
  } else {
    print(list(object$question))
  }
}

#étape 2
#usethis::create_package(
#  path = "C:/Users/Stephanie/Documents/R pour scientifique/Nomdupackage/", #lancer seulement si rendu la
#  rstudio = TRUE, 
#  open = TRUE
#)


