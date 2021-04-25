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
#' @author Stephanie kelley
#' @export
#' @examples
#' lecture_fichier_JSON("data-raw/questionnaire_service_clients.json")
lecture_fichier_JSON<-function(chemin){
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

#Exemple exctraction id question de la liste
#question<- c()
#a <-1
#while (a <= length(liste)){
 # question <-  c(question, as.character(liste[[a]]["id"]))
 # a = a+1
 # }
 # jeu_reponses <- reponses_performances_responsables[1:15]
  
 # #savoir si question commune
 # b <- 1
 # while (b <= ncol(jeu_reponses)){
  #  if (colnames(jeu_reponses[b]) %in% question){
   #   #Affichage libellé
    #  print(colnames(jeu_reponses[b]))

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
#' lecture_fichier_JSON.summary(questionnaire_service_clients,impression=TRUE)
lecture_fichier_JSON.summary<-function(object,...,impression=FALSE){
  if (impression){
    liste<-list()
    i=1
    while (i <= length(row.names(object))){   #on ne veut pas imprimer le choix null de la derniere...
      object$choix<-object$choix[!unlist(lapply(object$choix,is.null))]
      liste_tempo<-list()
      liste_tempo[["id"]]<-object$id[i]
      liste_tempo[["question"]]<-object$question[i]  
      liste_tempo[["choix"]]<-object$choix[i] # on veut arreter à i-1 
      #question<-c(object,as.character(object[[i]]["id"]))
      liste<- c(liste, list(liste_tempo))
      i= i+1
      print(liste)
      }
} else {
  question<-list()
  a = 1
  while (a <= length(row.names(object))){ 
    question_tempo<- list()                                # ca print 1-11 puis 1-12 ... 
    question_tempo[["id"]]<-object$id[a]
    question_tempo[["question"]]<-object$question[a]
    question<-c(question,list(question_tempo))
    a = a+1
    print(question)}
  }
}

#Exemple exctraction id question de la liste
#     question<- c()
 #    a <-1
  #   while (a <= length(liste)){
    #   question <-  c(question, as.character(liste[[a]]["id"]))
     #  a = a+1
    #   print(question)
    # }
     #else {print(list(object$question))}
  #     }
#}
 
# jeu_reponses <- reponses_performances_responsables[1:15]

# #savoir si question commune
# b <- 1
# while (b <= ncol(jeu_reponses)){
#  if (colnames(jeu_reponses[b]) %in% question){
#   #Affichage libellé
#  print(colnames(jeu_reponses[b]))
