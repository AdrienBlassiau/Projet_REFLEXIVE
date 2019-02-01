(* print_list_int_string

@param l une liste de couple (int,chaine de caractere)
@return affiche les résultats du générateur

val print_list_string : (int * string) list * int -> unit = <fun>
*)
val print_list_string : (int * string) list * int -> unit

(** chaine_vers_liste_de_chaine

@param s une chaîne de caractères, n un compteur pour itérer sur cette chaîne, buf un buffer qui récupère chacun des mots afin de les placer dans la liste et l la liste de chaînes de caractères renvoyée
@return transforme une chaîne de caractères en une liste de ses mots en coupant au niveau des espaces.
Équivalent de String.split_on_char " " s. 

val chaine_vers_liste_de_chaine : string -> int -> string -> string list -> string list = <fun>
*)
val chaine_vers_liste_de_chaine : string -> int -> string -> string list -> string list

(** tradliste

@param s une chaîne de caractères
@return utilise la fonction ci-dessus, donc transforme une chaîne de caractères en une liste de ses mots en coupant au niveau des espaces.

val tradliste : string -> string list = <fun>
*)
val tradliste : string -> string list


(** tradmot

@param l une liste de chaînes de caractères
@return la chaîne de caractères associée à la liste de chaînes de caractères entrée en concatènent chacune des chaînes 

val tradmot : string list -> string = <fun>
*)
val tradmot : string list -> string


(** arbre
Type d'arbre binaire de recherche qui sera adaptée au parcours des lettres de l'alphabet.
*)
type arbre = Vide | Noeud of ((char*int)*arbre*arbre);;

(** infixe 

@param abr un arbre binaire de recherche 
@return l'index affixe associé à l'arbre binaire de recherche entré

val infixe : arbre -> (char * int) list = <fun>

*)
val infixe : arbre -> (char * int) list

(** creation_index 

@param alea un booléen qui vaut true si on veut un remplissage de l'arbre par des nombres aléatoires et false pour un remplissage avec des 0, abr un arbre à initialiser 
@return l'arbre initialisé

val creation_index : arbre -> bool -> arbre = <fun>

*)
val creation_index : arbre -> bool -> arbre


(** compte_lettre_arbre 

@param abr un arbre binaire de recherche et lettre la lettre mise à jour dans l'arbre
@return l'arbre avec la lettre voulue incrémentée de 1

val compte_lettre_arbre : arbre -> char -> arbre = <fun>

*)
val compte_lettre_arbre : arbre -> char -> arbre


(** compte_mot_global 

@param abr une arbre binaire de recherche, mot la chaîne de caractères courante dont on cherche à recenser les lettres, n un compteur pour itérer sur les lettres de mot
@return l'arbre mis à jour après avoir compté les occurrences des différentes lettres de mot par le biais de la fonction compte_lettre_arbre (voir ci-dessus)

val compte_mot_global : arbre -> string -> int -> arbre = <fun>
*)
val compte_mot_global : arbre -> string -> int -> arbre


(** compte_mot_h 

Voir définition au dessus de compte_mot_global, ici on impose que le compteur d'itération soit à 0

val compte_mot_h : arbre -> string -> arbre = <fun>
*)
val compte_mot_h : arbre -> string -> arbre

(** compte_lettre_dans_mot 

@param mot le mot dont on veut compter le nombre d’occurrences de la lettre lettre_pitrat, compte qui récupère le nombre de lettre_pitrat et n un compteur pour initialiser le compte de départ
@return le nombre d'occurrences de la lettre lettre_pitrat dans mot

val compte_lettre_dans_mot : string -> int -> char -> int -> int = <fun>
*)
val compte_lettre_dans_mot : string -> int -> char -> int -> int

(** compte_mot_h 

Voir définition au dessus de compte_lettre_dans_mot, ici on impose que le compteur d'itération soit à 0

val compte_lettre : string -> int -> char -> int = <fun>
*)
val compte_lettre : string -> int -> char -> int


(** compte_lettre_dans_mot 

@param abr l'arbre binaire de recherche que l'on souhaite mettre à jour, lettre la lettre dont le compte est mis à jour et nombre son nouveau compte
@return l'arbre mis à jour
val maj_index_courant : arbre -> char -> int -> arbre = <fun>
*)
val maj_index_courant : arbre -> char -> int -> arbre

(** litt 

@param nombre un entier à convertir et langue la langue de conversion ('f' pour français, 'b' pour belge et 's' pour suisse)
@pre: 0<nombre<=100
@return l'écriture littérale d'un nombre dans la langue choisie
@raises Failure "erreur nombre rentré" si le nombre est entre entre -1 et -9 et 
Failure "Votre nombre doit etre compris entre 0 exclus et 100 inclus" si le nombre n'est pas compris entre 0 et 100


val litt : int -> char -> string = <fun>
*)
val litt : int -> char -> string


(** pre_choix_alea_gabarits 

@param l une liste d'éléments, n un compteur qui permet de faire défiler les éléments de la liste et choix le choix d'éléments de la liste
@return un élément pris au hasard dans cette liste
@raises: Failure "liste vide" si l est vide

val pre_choix_alea_gabarits : 'a list -> int -> int -> 'a = <fun>
*)
val pre_choix_alea_gabarits : 'a list -> int -> int -> 'a

(** choix_alea_gabarits

Voir définition au dessus de pre_choix_alea_gabarits, ici on impose que le compteur d'itérations soit à 0 et que le choix soit fait aléatoirement parmi un des éléments de la liste.

val choix_alea_gabarits : 'a list -> 'a = <fun>
*)
val choix_alea_gabarits : 'a list -> 'a

(** initiale_maj

@param s une chaîne de caractères
@return La même chaîne de caractère s avec une majuscule sur la première lettre

val initiale_maj : string -> string = <fun>
*)
val initiale_maj : string -> string 

(** compare_arbre

@param arbre1 et arbre2 les deux arbres à comparer et le_nombre le nombre de lettres à comparer en partant de 'a'
@return true si les deux arbres sont les mêmes et false sinon

val compare_arbre : arbre -> arbre -> int -> bool = <fun>
*)
val compare_arbre : arbre -> arbre -> int -> bool


(** decoupe_liste

@param n la taille de la liste souhaitée et l la liste à réduire de taille
@return la liste réduite

val decoupe_liste : int -> 'a list -> 'a list = <fun>
*)
val decoupe_liste : int -> 'a list -> 'a list



(** est_fixe

@param l une lettre de l'alphabet
@return true si la lettre est une lettre dite fixe c'est à dire une lettre dont le nombre ne varie plus après la phase d'initialisation de hofstadter ou pitrat. En d'autre terme, cette lettre ne figure dans aucune écriture littérale des nombres compris entre 0 et 100.

val est_fixe : char -> bool = <fun>
*)
val est_fixe : char -> bool


(** fragment_phrase

@param arb un arbre binaire de recherche, f une fonction qui à un couple (lettre,nombre d’occurrence) et à une chaîne de caractère associe une nouvelle chaîne de caractère et le_nombre le nombre de lettres de l'alphabet sélectionné
@return une chaîne de caractères particulière


val fragment_phrase : arbre -> (char * int -> string -> string) -> int -> string = <fun>
*)
val fragment_phrase : arbre -> (char * int -> string -> string) -> int -> string



(** hofstadter

@param index_fixe l'index de la partie fixe de la phrase, index_reel l'index de toute la phrase, phrase la phrase à transformer en phrase réflexive, iter le nombre d'itérations,  fin un booléen qui indique si l'on a trouvé ou non, le_nombre le nombre de lettre à inclure dans la phrase réflexive, début le début de phrase générée aléatoirement, concat_index_fixe la fonction de création de la phrase fixe et concat_index_mobile la fonction de création de la phrase mobile.
@return au bout de 100 itérations maximum, retourne un booléen indiquant si on a obtenu une phrase réflexive et l'index de la phrase.

val hofstadter :  arbre ->  arbre ->  string ->  int ->  bool ->  int ->  string ->  (char * int -> string -> string) ->  (char * int -> string -> string) -> bool * arbre = <fun>

*)
val hofstadter :  arbre ->  arbre ->  string ->  int ->  bool ->  int ->  string ->  (char * int -> string -> string) ->  (char * int -> string -> string) -> bool * arbre


(** boucle_hofstadter

@param nb_essai le nombre d'essai sur un même corps de phrase, le_nombre le nombre de lettre à inclure dans la phrase à transformer en phrase réflexive, debut le début de phrase générée aléatoirement, concat_index_entier la fonction de création de la phrase toute entière, concat_index_mobile la fonction de création de la phrase mobile, concat_index_fixe la fonction de création de la phrase fixe et phrase_mobile la phrase mobile
@return au bout d'un maximum de 100 situations initiales générées aléatoirement, retourne un booléen indiquant si la phrase trouvée est réflexive ou non, la phrase trouvée ainsi que le nombre de situations initiales générées.
 
val boucle_hofstadter :  int ->  int ->  string ->  (char * int -> string -> string) ->  (char * int -> string -> string) ->  (char * int -> string -> string) -> 'a -> bool * int * string = <fun>

*)
val boucle_hofstadter :  int ->  int ->  string ->  (char * int -> string -> string) ->  (char * int -> string -> string) ->  (char * int -> string -> string) -> 'a -> bool * int * string



(** initialise_pitrat

@param le_nombre le nombre de lettres à inclure dans la phrase réflexive, corps_phrase le corps de la phrase généré aléatoirement, arbre_entier l'arbre de recherche binaire qui va comptabiliser l'ensemble des lettres dans la phrase, iter_int le nombre d'itération, phrase_totale l'ensemble de la phrase et concat_index_entier la fonction de création de la phrase toute entière
@return l'arbre qui rend compte du nombre de lettre dans la phrase
 
val initialise_pitrat : int -> string -> arbre -> int -> string -> (char * int -> string -> string) -> arbre = <fun>
*)
val initialise_pitrat : int -> string -> arbre -> int -> string -> (char * int -> string -> string) -> arbre 



(** corps_pitrat

@param lettre la lettre courante modifiée, le_nombre le nombre de lettre à inclure dans la phrase réflexive, index_fixe_infixe l'index de la phrase fixe sous forme de liste, index_fixe l'index de la phrase fixe, index_courant l'index de la phrase courante, phrase_mobile la phrase mobile et concat_index_mobile la fonction de création de la phrase mobile.
@return réalise une modification complète de la phrase en la modifiant lettre par lettre. A chaque itération, on regarde si la phrase obtenue n'est pas réflexive auquel cas on stoppe le processus
 
val corps_pitrat : char -> int -> (char * int) list -> arbre -> arbre -> string -> (char * int -> string -> string) -> bool * arbre * string = <fun>

*)
val corps_pitrat : char -> int -> (char * int) list -> arbre -> arbre -> string -> (char * int -> string -> string) -> bool * arbre * string

(** pitrat

@param le_nombre le nombre de lettres à inclure dans la phrase, index_fixe l'index de la partie fixe de la phrase, index_courant l'index de toute la phrase, phrase_mobile la phrase mobile, iter le nombre d'itérations, fin un booléen qui indique si l'on a trouvé ou non et concat_index_mobile la fonction de création de la phrase mobile.
@return au bout de 100 itérations maximum, retourne un booléen indiquant si on a obtenu une phrase réflexive et l'index de la phrase.

val pitrat :  int -> arbre ->  arbre ->  string ->  int -> bool -> (char * int -> string -> string) -> arbre * string = <fun>
*)
val pitrat : int -> arbre -> arbre -> string -> int -> bool -> (char * int -> string -> string) -> bool * arbre


(** boucle_pitrat

@param nb_essai le nombre d'essai sur un même corps de phrase, le_nombre le nombre de lettres à inclure dans la phrase réflexive, début le début de phrase généré aléatoirement, concat_index_entier la fonction de création de la phrase toute entière, concat_index_mobile la fonction de création de la phrase mobile, concat_index_fixe la fonction de création de la phrase fixe et phrase_mobile la phrase mobile. 
@return au bout d'un maximum de 100 situations initiales générées aléatoirement, retourne un booléen indiquant si la phrase trouvée est réflexive ou non, la phrase trouvée ainsi que le nombre de situations initiales générées.
 
val boucle_pitrat : int -> int -> string -> (char * int -> string -> string) -> (char * int -> string -> string) -> (char * int -> string -> string) -> string list -> bool * int * string = <fun>
*)
val boucle_pitrat : int -> int -> string -> (char * int -> string -> string) -> (char * int -> string -> string) -> (char * int -> string -> string) -> string list -> bool * int * string



(** generateur

@param boucle_f une fonction de génération de phrases réflexives (boucle_pitrat ou boucle_hofstadter), le_nombre le nombre de lettres à inclure dans la phrase réflexive, langue la langue de traduction des chiffres littéraux, iter_corps le nombre de corps différents aléatoires sur lesquels on veut tester la réflexivité des phrases et list_solutions le tableau des phrases réflexives obtenues et nombre_phrases_trouvees le nombre de phrases reflexives trouvées
@return retourne une liste constituée de toutes les phrases réflexives trouvées ainsi que du nombre_essai pour les trouver.
 
val generateur : (int -> 'a -> string ->(char * int -> string -> string) ->(char * int -> string -> string) -> (char * int -> string -> string) -> 'b list -> bool * 'c * string) ->'a -> char -> int -> ('c * string) list -> int -> ('c * string) list * int = <fun>
*)
val generateur : (int -> 'a -> string ->(char * int -> string -> string) ->(char * int -> string -> string) -> (char * int -> string -> string) -> 'b list -> bool * 'c * string) ->'a -> char -> int -> ('c * string) list -> int -> ('c * string) list * int
