#R pour scientifique tp3 
#Stephanie Kelley et Marc-Antoine Chiasson

#Lecture des 4 fichiers 
#questionnaire1 + reponses
library(jsonlite)
questionnaire_service_clients <- fromJSON("data-raw/questionnaire_service_clients.json")
str(questionnaire_service_clients)
reponses_service_clients<-read.csv("data-raw/reponses_service_clients_2021_02.csv",sep=";",encoding="UTF-8")
#questionnaire2 + reponse
questionnaire_performances_responsables <- fromJSON("data-raw/questionnaire_performances_responsables.json")
str(questionnaire_performances_responsables)
reponses_performances_responsables<-read.csv("data-raw/reponses_performances_responsables_2020.csv",sep=";",encoding="UTF-8")

#Étape 1 ecrire les fonctions

#' Lecture de fichier JSON
#' 
#' Lecture d'un questionnaire de type JSON
#' 
#' @param fichier le chemin ou le questionnaire se trouve 
#' @return une seule valeur : une liste qui contient un element par question
#' @export
#' @examples
#' lecture_fichier_JSON("data-raw/questionnaire_service_clients.json")
library(testthat)
library(jsonlite)
lecture_fichier_JSON<-function(chemin){
  if(file.exists(chemin)==FALSE) {
    warning("Entrer un chemin valide qui terminer par .json ",immediate.=TRUE)}
  fichier <- fromJSON(chemin)     #pas certaine du chemin
  i=1
  liste<- list()
  while (i <= length(row.names(fichier))){
    liste_tempo<-list()
    liste_tempo[["id"]]<-fichier$id[i]
    liste_tempo[["question"]]<-fichier$question[i]  
    liste_tempo[["choix"]]<-fichier$choix[i]
    liste <- c(liste, list(liste_tempo))
    i= i+1
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
#' @export
#' @examples
#' lecture_fichier_JSON.summary(questionnaire_service_clients,impression=TRUE)
#' lecture_fichier_JSON.summary(questionnaire_performances_responsables)
lecture_fichier_JSON.summary<-function(object,...,impression=FALSE){
  if(is.data.frame(object)==FALSE) {
    warning("Entrer un fichier valide",immediate.=TRUE)}
  if (impression){
    liste<-list()
    i=1
    while (i <= length(row.names(object))){   
      nouveau_choix<-object$choix[!unlist(lapply(object$choix,is.null))]
      liste_tempo<-list()
      #liste_tempo[["id"]]<-object$id[i]
      liste_tempo[["question"]]<-object$question[i]  
      liste_tempo[["choix"]]<-nouveau_choix[i]
      liste<- c(liste, list(liste_tempo))
      i= i+1
    }
     print(unlist(liste))   
} else {
  question<-list()
  a = 1
  while (a <= length(row.names(object))){ 
    question_tempo<- list()                                
    #question_tempo[["id"]]<-object$id[a]
    question_tempo[["question"]]<-object$question[a]
    question<-c(question,list(question_tempo))
    a = a+1
    }
  print(question)  # apparait sous forme de liste
}
}

