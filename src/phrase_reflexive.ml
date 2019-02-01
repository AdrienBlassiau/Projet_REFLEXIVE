open Printf

(* print_list_int_string

   @param l une liste de couple (int,chaine de caractere)
   @return affiche les résultats du générateur

   val print_list_string : (int * string) list * int -> unit = <fun>
*)

(** Tests : 

    Voir les derniers test unitaires ...
*)
let rec print_list_string l= match l with
  |[],t->print_string "\n";print_string "Le nombre de phrase trouvée est : ";print_int t;print_string "\n"
  |(compt,s)::q,t-> print_string "(";print_int compt;print_string ";";print_string " ";print_string s;print_string ")";print_string "\n";print_string "\n";print_list_string (q,t) ;;



(** chaine_vers_liste_de_chaine

    @param s une chaîne de caractères, n un compteur pour itérer sur cette chaîne, buf un buffer qui récupère chacun des mots afin de les placer dans la liste et l la liste de chaînes de caractères renvoyée
    @return transforme une chaîne de caractères en une liste de ses mots en coupant au niveau des espaces.
    Équivalent de String.split_on_char " " s. 

    val chaine_vers_liste_de_chaine : string -> int -> string -> string list -> string list = <fun>
*)

(** Tests : 
    chaine_vers_liste_de_chaine "Bonjour le monde" 0 "" [];; -> ["Bonjour"; "le"; "monde"]
    chaine_vers_liste_de_chaine "" 0 "" [];; -> [""]
*)



let rec chaine_vers_liste_de_chaine s n buf l = 
  let lg = String.length s in
    if n = lg then buf::l 
    else if s.[lg-1-n] = ' ' then chaine_vers_liste_de_chaine s (n+1) "" (if (buf="") then l else buf::l)
         else chaine_vers_liste_de_chaine s (n+1) ((String.make 1 (s.[lg-1-n]))^buf) l;;


(** tradliste

    @param s une chaîne de caractères
    @return utilise la fonction ci-dessus, donc transforme une chaîne de caractères en une liste de ses mots en coupant au niveau des espaces.

    val tradliste : string -> string list = <fun>
*)

(** Tests : 
    tradliste "Bonjour  le    monde" ;; -> ["Bonjour"; "le"; "monde"]
    tradliste "";; -> [""]

*)
let tradliste s = chaine_vers_liste_de_chaine s 0 "" [] ;;




(** tradmot

    @param l une liste de chaînes de caractères
    @return la chaîne de caractères associée à la liste de chaînes de caractères entrée en concatènent chacune des chaînes 

    val tradmot : string list -> string = <fun>
*)

(** Tests : 
    tradmot ["";"Bonjour";"le";"monde"];; -> "Bonjour le monde"
    tradmot ["Bonjour";"le";"";"monde"];; -> "Bonjour le monde"
    tradmot ["Bonjour";"le";"monde";"";"";"";""];; -> "Bonjour le monde"
    tradmot ["Bonjour";"";"le";"monde"];; -> "Bonjour le monde"
    tradmot ["";"Bonjour";"le";"monde"];; -> "Bonjour le monde"
    tradmot ["";"";"";"Bonjour";"";"le";"monde";""];; -> "Bonjour le monde"
*)
let tradmot l = List.fold_left (fun x y -> if y="" || x="" then x^y else x^" "^y) "" l ;;


(******************************************************************************************************************************)

(** arbre
    Type d'arbre binaire de recherche qui sera adaptée au parcours des lettres de l'alphabet.
*)
type arbre = Vide | Noeud of ((char*int)*arbre*arbre);;


(** abr
    Voir le rapport pour plus d'informations au sujet de cet arbre, notamment sur sa construction.
*)
let abr = Noeud
    (('n', 0),
     Noeud
       (('f', 0),
        Noeud
          (('c', 0), Noeud (('a', 0), Vide, Noeud (('b', 0), Vide, Vide)),
           Noeud (('e', 0), Noeud (('d', 0), Vide, Vide), Vide)),
        Noeud
          (('j', 0),
           Noeud
             (('h', 0), Noeud (('g', 0), Vide, Vide), Noeud (('i', 0), Vide, Vide)),
           Noeud
             (('l', 0), Noeud (('k', 0), Vide, Vide), Noeud (('m', 0), Vide, Vide)))),
     Noeud
       (('t', 0),
        Noeud
          (('q', 0), Noeud (('p', 0), Noeud (('o', 0), Vide, Vide), Vide),
           Noeud (('r', 0), Vide, Noeud (('s', 0), Vide, Vide))),
        Noeud
          (('x', 0),
           Noeud
             (('v', 0), Noeud (('u', 0), Vide, Vide), Noeud (('w', 0), Vide, Vide)),
           Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide))));;



(** gabarits_debut et gabarits_fin
    Gabarits de phrases
*)
let gabarits_debut = 
  [["cher lecteur,";"ami lecteur,";"chers lecteurs,";""];
   ["vous avez"];
   ["bien";"vraiment";"parfaitement";""];
   ["raison de compter"];
   ["et de recompter";""];
   ["soigneusement";"avec soin";""];
   ["toutes";""];
   ["les lettres de cette"];
   ["longue";"curieuse";"bizarre";""];
   ["phrase avant de la croire quand elle"];
   ["vous";""];
   ["affirme qu'elle contient"];
   ["exactement";""]] ;;

let gabarits_fin = [["et finalement ";"et pour finir ";"et enfin ";"et pour terminer ";"et "]];;


(** infixe 

    @param abr un arbre binaire de recherche 
    @return l'index affixe associé à l'arbre binaire de recherche entré

    val infixe : arbre -> (char * int) list = <fun>

*)

(** Tests : 
    infixe abr;; -> [('a', 0); ('b', 0); ('c', 0); ('d', 0); ('e', 0); ('f', 0); ('g', 0); ('h', 0); ('i', 0); ('j', 0); ('k', 0); ('l', 0); ('m', 0); ('n', 0); ('o', 0); ('p', 0); ('q', 0); ('r', 0); ('s', 0); ('t', 0); ('u', 0); ('v', 0); ('w', 0); ('x', 0); ('y', 0); ('z', 0)]
    infixe Vide;; -> []

    où abr est l'arbre binaire de recherche défini plus haut.
*)
let rec infixe abr = match abr with
  |Vide -> [] 
  |Noeud(r,ag,ad) -> (infixe ag) @ (r::(infixe ad));;


(** creation_index 

    @param alea un booléen qui vaut true si on veut un remplissage de l'arbre par des nombres aléatoires et false pour un remplissage avec des 0, abr un arbre à initialiser 
    @return l'arbre initialisé

    val creation_index : arbre -> bool -> arbre = <fun>

*)

(** Tests : 
    creation_index abr true ;; ->
    Noeud
    (('n', 45),
    Noeud
    (('f', 97),
    Noeud
     (('c', 60), Noeud (('a', 55), Vide, Noeud (('b', 19), Vide, Vide)),
      Noeud (('e', 71), Noeud (('d', 44), Vide, Vide), Vide)),
    Noeud
     (('j', 4),
      Noeud
       (('h', 98), Noeud (('g', 64), Vide, Vide),
        Noeud (('i', 98), Vide, Vide)),
      Noeud
       (('l', 1), Noeud (('k', 56), Vide, Vide),
        Noeud (('m', 71), Vide, Vide)))),
    Noeud
    (('t', 86),
    Noeud
     (('q', 13), Noeud (('p', 94), Noeud (('o', 24), Vide, Vide), Vide),
      Noeud (('r', 2), Vide, Noeud (('s', 7), Vide, Vide))),
    Noeud
     (('x', 83),
      Noeud
       (('v', 22), Noeud (('u', 50), Vide, Vide),
        Noeud (('w', 71), Vide, Vide)),
      Noeud (('z', 42), Noeud (('y', 1), Vide, Vide), Vide))))

    creation_index abr false ;;->
    Noeud
    (('n', 0),
    Noeud
    (('f', 0),
    Noeud
     (('c', 0), Noeud (('a', 0), Vide, Noeud (('b', 0), Vide, Vide)),
      Noeud (('e', 0), Noeud (('d', 0), Vide, Vide), Vide)),
    Noeud
     (('j', 0),
      Noeud
       (('h', 0), Noeud (('g', 0), Vide, Vide), Noeud (('i', 0), Vide, Vide)),
      Noeud
       (('l', 0), Noeud (('k', 0), Vide, Vide), Noeud (('m', 0), Vide, Vide)))),
    Noeud
    (('t', 0),
    Noeud
     (('q', 0), Noeud (('p', 0), Noeud (('o', 0), Vide, Vide), Vide),
      Noeud (('r', 0), Vide, Noeud (('s', 0), Vide, Vide))),
    Noeud
     (('x', 0),
      Noeud
       (('v', 0), Noeud (('u', 0), Vide, Vide), Noeud (('w', 0), Vide, Vide)),
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide))))


    creation_index Vide false ;; -> Vide
    creation_index Vide true ;; -> Vide

    où abr est l'arbre binaire de recherche défini plus haut.
*)


let rec creation_index abr alea = 
  let nombre = (if alea then (Random.int(50)+1) 
                              else 0) in match abr with
  |Vide -> Vide
  |Noeud((c,i),ag,ad) -> Noeud((c,nombre),(creation_index ag alea),(creation_index ad alea));;



(** compte_lettre_arbre 

    @param abr un arbre binaire de recherche et lettre la lettre mise à jour dans l'arbre
    @return l'arbre avec la lettre voulue incrémentée de 1

    val compte_lettre_arbre : arbre -> char -> arbre = <fun>

*)

(** Tests : 
    compte_lettre_arbre Vide 'a';; -> Vide
    compte_lettre_arbre abr 'a';; -> 
    Noeud
    (('n', 0),
    Noeud
    (('f', 0),
    Noeud
     (('c', 0), Noeud (('a', 1), Vide, Noeud (('b', 0), Vide, Vide)),
      Noeud (('e', 0), Noeud (('d', 0), Vide, Vide), Vide)),
    Noeud
     (('j', 0),
      Noeud
       (('h', 0), Noeud (('g', 0), Vide, Vide), Noeud (('i', 0), Vide, Vide)),
      Noeud
       (('l', 0), Noeud (('k', 0), Vide, Vide), Noeud (('m', 0), Vide, Vide)))),
    Noeud
    (('t', 0),
    Noeud
     (('q', 0), Noeud (('p', 0), Noeud (('o', 0), Vide, Vide), Vide),
      Noeud (('r', 0), Vide, Noeud (('s', 0), Vide, Vide))),
    Noeud
     (('x', 0),
      Noeud
       (('v', 0), Noeud (('u', 0), Vide, Vide), Noeud (('w', 0), Vide, Vide)),
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide))))
*)
let rec compte_lettre_arbre abr lettre = match abr with
  |Vide -> abr
  |Noeud((c,i),ag,ad) -> if c >= lettre then (if c=lettre then Noeud((c,i+1),ag,ad) 
                                                                                          else Noeud((c,i),(compte_lettre_arbre ag lettre),ad))
                           else Noeud((c,i),ag,(compte_lettre_arbre ad lettre));;


(** compte_mot_global 

    @param abr une arbre binaire de recherche, mot la chaîne de caractères courante dont on cherche à recenser les lettres, n un compteur pour itérer sur les lettres de mot
    @return l'arbre mis à jour après avoir compté les occurrences des différentes lettres de mot par le biais de la fonction compte_lettre_arbre (voir ci-dessus)

    val compte_mot_global : arbre -> string -> int -> arbre = <fun>
*)

(** Tests : 
    compte_mot_global abr "Bonjour" 0 ;; 
    -> Noeud
    (('n', 1),
    Noeud
    (('f', 0),
    Noeud
     (('c', 0), Noeud (('a', 0), Vide, Noeud (('b', 1), Vide, Vide)),
      Noeud (('e', 0), Noeud (('d', 0), Vide, Vide), Vide)),
    Noeud
     (('j', 1),
      Noeud
       (('h', 0), Noeud (('g', 0), Vide, Vide), Noeud (('i', 0), Vide, Vide)),
      Noeud
       (('l', 0), Noeud (('k', 0), Vide, Vide), Noeud (('m', 0), Vide, Vide)))),
    Noeud
    (('t', 0),
    Noeud
     (('q', 0), Noeud (('p', 0), Noeud (('o', 2), Vide, Vide), Vide),
      Noeud (('r', 1), Vide, Noeud (('s', 0), Vide, Vide))),
    Noeud
     (('x', 0),
      Noeud
       (('v', 0), Noeud (('u', 1), Vide, Vide), Noeud (('w', 0), Vide, Vide)),
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide))))
    compte_mot_global Vide "Bonjour" 0 ;; -> Vide

    où abr est l'arbre binaire de recherche définit plus haut.
*)
let rec compte_mot_global abr mot n= 
  let taille = String.length mot in 
    if n>=taille then abr 
    else compte_mot_global (compte_lettre_arbre abr (Char.lowercase mot.[n])) mot (n+1) ;;



(** compte_mot_h 

    Voir définition au dessus de compte_mot_global, ici on impose que le compteur d'itération soit à 0

    val compte_mot_h : arbre -> string -> arbre = <fun>
*)

let compte_mot_h index mot = compte_mot_global index mot 0 ;;


(** compte_lettre_dans_mot 

    @param mot le mot dont on veut compter le nombre d’occurrences de la lettre lettre_pitrat, compte qui récupère le nombre de lettre_pitrat et n un compteur pour initialiser le compte de départ
    @return le nombre d'occurrences de la lettre lettre_pitrat dans mot

    val compte_lettre_dans_mot : string -> int -> char -> int -> int = <fun>
*)

(** Tests : 
    compte_lettre_dans_mot "Bonjour" 0 'o' 0;; -> 2
    compte_lettre_dans_mot "Bonjour" 1 'o' 0;; -> 3
*)
let rec compte_lettre_dans_mot mot compte lettre_pitrat n= let taille = String.length mot in 
  if n>=taille then compte 
  else compte_lettre_dans_mot mot (compte + (if (lettre_pitrat=(Char.lowercase mot.[n])) then 1 
                                                                                       else 0)) lettre_pitrat (n+1);;


(** compte_mot_h 

    Voir définition au dessus de compte_lettre_dans_mot, ici on impose que le compteur d'itération soit à 0

    val compte_lettre : string -> int -> char -> int = <fun>
*)
let compte_lettre mot compte lettre_pitrat = compte_lettre_dans_mot mot compte lettre_pitrat 0;;



(** compte_lettre_dans_mot 

    @param abr l'arbre binaire de recherche que l'on souhaite mettre à jour, lettre la lettre dont le compte est mis à jour et nombre son nouveau compte
    @return l'arbre mis à jour
    val maj_index_courant : arbre -> char -> int -> arbre = <fun>
*)

(** Tests : 
    maj_index_courant abr 'a' 32;; ->
    Noeud
    (('n', 0),
    Noeud
    (('f', 0),
    Noeud
     (('c', 0), Noeud (('a', 32), Vide, Noeud (('b', 0), Vide, Vide)),
      Noeud (('e', 0), Noeud (('d', 0), Vide, Vide), Vide)),
    Noeud
     (('j', 0),
      Noeud
       (('h', 0), Noeud (('g', 0), Vide, Vide), Noeud (('i', 0), Vide, Vide)),
      Noeud
       (('l', 0), Noeud (('k', 0), Vide, Vide), Noeud (('m', 0), Vide, Vide)))),
    Noeud
    (('t', 0),
    Noeud
     (('q', 0), Noeud (('p', 0), Noeud (('o', 0), Vide, Vide), Vide),
      Noeud (('r', 0), Vide, Noeud (('s', 0), Vide, Vide))),
    Noeud
     (('x', 0),
      Noeud
       (('v', 0), Noeud (('u', 0), Vide, Vide), Noeud (('w', 0), Vide, Vide)),
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide))))


    où abr est l'arbre binaire de recherche définit plus haut.
*)
let rec maj_index_courant abr lettre nombre =  match abr with
  |Vide -> abr
  |Noeud((c,i),ag,ad) -> if c >= lettre then (if c=lettre then Noeud((c,nombre),ag,ad) 
                                                                                          else Noeud((c,i),(maj_index_courant ag lettre nombre),ad))
                           else Noeud((c,i),ag,(maj_index_courant ad lettre nombre));;


(******************************************************************************************************************************)

(** litt 

    @param nombre un entier à convertir et langue la langue de conversion ('f' pour français, 'b' pour belge et 's' pour suisse)
    @pre: 0<nombre<=100
    @return l'écriture littérale d'un nombre dans la langue choisie
    @raises Failure "erreur nombre rentré" si le nombre est entre entre -1 et -9 et 
    Failure "Votre nombre doit etre compris entre 0 exclus et 100 inclus" si le nombre n'est pas compris entre 0 et 100


    val litt : int -> char -> string = <fun>
*)

(** Tests : 
    litt (-110) 's;; -> Exception:Failure "Votre nombre doit etre compris entre 0 exclus et 100 inclus".
    litt 101 'b';; -> Exception:Failure "Votre nombre doit etre compris entre 0 exclus et 100 inclus".
    litt 0 'f';; -> ""
    litt (-1) 'f';; -> Exception:Failure "erreur nombre rentre"
    litt 80 'f';; -> "quatre-vingts"
    litt 80 's';; -> "huitante"
    litt 78 'b';; -> "septante-huit"
    litt 13 'f';; -> "treize"
*)
let rec litt nombre langue= 
  let dizaine = nombre / 10 in 
  let unite = nombre mod 10 in match (dizaine,unite) with
  |(0,_) | (1,_) -> (match (dizaine,unite) with 
                        |(0,0)-> ""|(0,1) ->"un"|(0,2)->"deux"|(0,3)->"trois"|(0,4)->"quatre"|(0,5)->"cinq"|(0,6)->"six"|(0,7)->"sept"|(0,8)->"huit"|(0,9)->"neuf"|(1,0)->"dix"|(1,1)->"onze"|(1,2)->"douze"|(1,3)->"treize"|(1,4)->"quatorze"|(1,5)->"quinze"|(1,6)->"seize"|(1,7)->"dix-sept"|(1,8)->"dix-huit"|(1,9)->"dix-neuf"
                        |_->failwith "erreur nombre rentre")
  |(8,0) -> if langue!='s' then "quatre-vingts" 
              else "huitante"
  |(2,_) | (3,_) | (4,_) | (5,_) | (6,_) | (8,_) -> (match dizaine with 
                                                         |2 ->"vingt"|3->"trente"|4->"quarante"|5->"cinquante"|6->"soixante"
                                                         |8->if langue='s'then "huitante" 
                                                       else "quatre-vingt"
                                                   |_->failwith "erreur dizaine")
                                                  ^(if (unite = 1) && ((langue='s') || dizaine <> 8) then "-et-"
                                                      else if unite <> 0 then "-"
                                                           else "")
                                                  ^(litt unite langue)
|(7,_) | (9,_)->(match dizaine with 
                     |7->if langue='f' then "soixante" 
                           else "septante"
                     |9->if langue='f' then "quatre-vingt" 
                           else "nonante"
                     |_->failwith "erreur dizaine")
                                ^(if langue !='f' && unite=0 then " " 
                                    else if (langue !='f' && unite=1) || (langue ='f' && nombre=71) then "-et-" 
                                         else "-")
                                ^(litt (if langue = 'f' then (unite+10) 
                                                else unite) langue)
|(10,0) -> "cent"
|_ -> failwith "Votre nombre doit etre compris entre 0 exclus et 100 inclus" ;;


(** pre_choix_alea_gabarits 

    @param l une liste d'éléments, n un compteur qui permet de faire défiler les éléments de la liste et choix le choix d'éléments de la liste
    @return un élément pris au hasard dans cette liste
    @raises: Failure "liste vide" si l est vide

    val pre_choix_alea_gabarits : 'a list -> int -> int -> 'a = <fun>
*)

(** Tests : 
    pre_choix_alea_gabarits ["cher lecteur,";"ami lecteur,";"chers lecteurs,";""] 0 2;; -> "chers lecteurs,"
    pre_choix_alea_gabarits ["vous avez"] 0 0;; -> "vous avez" 
    pre_choix_alea_gabarits ["toutes";""] 0 1;; -> ""  
*)
let rec pre_choix_alea_gabarits l n choix = match l with
  |[] -> failwith "liste vide"
  |t::q -> if choix = n then t 
              else pre_choix_alea_gabarits q (n+1) choix ;;


(** choix_alea_gabarits

    Voir définition au dessus de pre_choix_alea_gabarits, ici on impose que le compteur d'itérations soit à 0 et que le choix soit fait aléatoirement parmi un des éléments de la liste.

    val choix_alea_gabarits : 'a list -> 'a = <fun>
*)

let choix_alea_gabarits l = pre_choix_alea_gabarits l 0 (Random.int(List.length(l)));;


(** initiale_maj

    @param s une chaîne de caractères
    @return La même chaîne de caractère s avec une majuscule sur la première lettre

    val initiale_maj : string -> string = <fun>
*)

(** Tests : 
    initiale_maj "";; -> "" 
    initiale_maj "ceci est une chaine de caracteres";; -> "Ceci est une chaine de caracteres"
*)

let initiale_maj s = if String.length s = 0 then "" 
                       else (String.make 1 (Char.uppercase s.[0]))^(String.sub s 1 (String.length(s)-1));;


(** compare_arbre

    @param arbre1 et arbre2 les deux arbres à comparer et le_nombre le nombre de lettres à comparer en partant de 'a'
    @return true si les deux arbres sont les mêmes et false sinon

    val compare_arbre : arbre -> arbre -> int -> bool = <fun>
*)

(** Tests : 
    compare_arbre abr1 abr2 10 ;; -> false
    compare_arbre abr1 abr2 9 ;; -> true


    avec :
    let abr1 = Noeud
    (('n', 0),
    Noeud
    (('f', 0),
    Noeud
     (('c', 0), Noeud (('a', 0), Vide, Noeud (('b', 0), Vide, Vide)),
      Noeud (('e', 0), Noeud (('d', 0), Vide, Vide), Vide)),
    Noeud
     (('j', 0),
      Noeud
       (('h', 0), Noeud (('g', 0), Vide, Vide), Noeud (('i', 0), Vide, Vide)),
      Noeud
       (('l', 0), Noeud (('k', 0), Vide, Vide), Noeud (('m', 0), Vide, Vide)))),
    Noeud
    (('t', 0),
    Noeud
     (('q', 0), Noeud (('p', 0), Noeud (('o', 0), Vide, Vide), Vide),
      Noeud (('r', 0), Vide, Noeud (('s', 0), Vide, Vide))),
    Noeud
     (('x', 0),
      Noeud
       (('v', 0), Noeud (('u', 0), Vide, Vide), Noeud (('w', 0), Vide, Vide)),
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide))));;

    let abr2 = Noeud
    (('n', 0),
    Noeud
    (('f', 0),
    Noeud
     (('c', 0), Noeud (('a', 0), Vide, Noeud (('b', 0), Vide, Vide)),
      Noeud (('e', 0), Noeud (('d', 0), Vide, Vide), Vide)),
    Noeud
     (('j', 1),
      Noeud
       (('h', 0), Noeud (('g', 0), Vide, Vide), Noeud (('i', 0), Vide, Vide)),
      Noeud
       (('l', 0), Noeud (('k', 0), Vide, Vide), Noeud (('m', 0), Vide, Vide)))),
    Noeud
    (('t', 0),
    Noeud
     (('q', 0), Noeud (('p', 0), Noeud (('o', 0), Vide, Vide), Vide),
      Noeud (('r', 0), Vide, Noeud (('s', 0), Vide, Vide))),
    Noeud
     (('x', 0),
      Noeud
       (('v', 0), Noeud (('u', 0), Vide, Vide), Noeud (('w', 0), Vide, Vide)),
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide))));;
*)
let rec compare_arbre arbre1 arbre2 le_nombre = match arbre1, arbre2 with
  |Vide,Vide -> true 
  |_,Vide -> false 
  |Vide,_ -> false 
  |Noeud((a,b),ag1,ad1),Noeud((c,d),ag2,ad2)-> if a>(char_of_int (le_nombre+96)) || b=d then compare_arbre ag1 ag2 le_nombre && compare_arbre ad1 ad2 le_nombre 
                                                 else false;;



(** decoupe_liste

    @param n la taille de la liste souhaitée et l la liste à réduire de taille
    @return la liste réduite

    val decoupe_liste : int -> 'a list -> 'a list = <fun>
*)

(** Tests : 
    decoupe_liste 5 tab;; -> [('a', 0); ('b', 0); ('c', 0); ('d', 0); ('e', 0)]
    decoupe_liste (-1) tab;; -> []

    avec :
    let tab = 
    [('a', 0); ('b', 0); ('c', 0); ('d', 0); ('e', 0); ('f', 0); ('g', 0);('h', 0); ('i', 0); ('j', 0); ('k', 0); ('l', 0); ('m', 0); ('n', 0);('o', 0); ('p', 0); ('q', 0); ('r', 0); ('s', 0); ('t', 0); ('u', 0);('v', 0); ('w', 0); ('x', 0); ('y', 0); ('z', 0)] ;;

*)
let rec decoupe_liste n l = match l with
  |[] -> []
  |t::q -> if n<=0 then [] 
             else t::(decoupe_liste (n-1) q);;


(** est_fixe

    @param l une lettre de l'alphabet
    @return true si la lettre est une lettre dite fixe c'est à dire une lettre dont le nombre ne varie plus après la phase d'initialisation de hofstadter ou pitrat. En d'autre terme, cette lettre ne figure dans aucune écriture littérale des nombres compris entre 0 et 100.

    val est_fixe : char -> bool = <fun>
*)

(** Tests : 
    est_fixe 'a' ;; -> false
    est_fixe 'l' ;; -> true

*)
let est_fixe l = match l with 
  |'b'|'j'|'k'|'l'|'m'|'w'|'y' -> true 
  |_ -> false;;


(** fragment_phrase

    @param arb un arbre binaire de recherche, f une fonction qui à un couple (lettre,nombre d’occurrence) et à une chaîne de caractère associe une nouvelle chaîne de caractère et le_nombre le nombre de lettres de l'alphabet sélectionné
    @return une chaîne de caractères particulière


    val fragment_phrase : arbre -> (char * int -> string -> string) -> int -> string = <fun>
*)

(* Tests :
   fragment_phrase abr concat_index_fixe 12 ;; -> "adix-neufbcdefghiquatrejcinquante-sixkunl"

   fragment_phrase abr concat_index_entier 14 ;; -> "cinquante-cinq a, dix-neuf b, soixante c, quarante-quatre d, soixante-et-onze e, quatre-vingt-dix-sept f, soixante-quatre g, quatre-vingt-dix-huit h, quatre-vingt-dix-huit i, quatre j, cinquante-six k, un l, soixante-et-onze m, quarante-cinq n"

   fragment_phrase abr concat_index_mobile 13 ;; -> "cinquante-cinq  soixante quarante-quatre soixante-et-onze quatre-vingt-dix-sept soixante-quatre quatre-vingt-dix-huit quatre-vingt-dix-huit     "

   avec : 

   let debut = tradmot(List.map choix_alea_gabarits gabarits_debut) ;;
   let fin = tradmot(List.map choix_alea_gabarits gabarits_fin) ;;

   et avec f :

   let concat_index_entier (a,b) reste = (litt b 'f')^" "^(String.make 1 a)^(if reste="" then "" else ","^" "^(if a='y' then fin else "")^reste) ;; 
   let concat_index_mobile (a,b) reste = (if not(est_fixe a) then (litt b 'f') else "")^" "^reste ;;
   let concat_index_fixe (a,b) reste = (if (est_fixe a) then (litt b 'f') else "")^(String.make 1 a)^(if a='y' then fin else "")^reste ;;

   et encore :

   let abr = 
   Noeud
   (('n', 45),
   Noeud
   (('f', 97),
    Noeud
     (('c', 60), Noeud (('a', 55), Vide, Noeud (('b', 19), Vide, Vide)),
      Noeud (('e', 71), Noeud (('d', 44), Vide, Vide), Vide)),
    Noeud
     (('j', 4),
      Noeud
       (('h', 98), Noeud (('g', 64), Vide, Vide),
        Noeud (('i', 98), Vide, Vide)),
      Noeud
       (('l', 1), Noeud (('k', 56), Vide, Vide),
        Noeud (('m', 71), Vide, Vide)))),
   Noeud
   (('t', 86),
    Noeud
     (('q', 13), Noeud (('p', 94), Noeud (('o', 24), Vide, Vide), Vide),
      Noeud (('r', 2), Vide, Noeud (('s', 7), Vide, Vide))),
    Noeud
     (('x', 83),
      Noeud
       (('v', 22), Noeud (('u', 50), Vide, Vide),
        Noeud (('w', 71), Vide, Vide)),
      Noeud (('z', 42), Noeud (('y', 1), Vide, Vide), Vide)))) ;;


*)
let fragment_phrase arb f le_nombre= List.fold_right f (decoupe_liste le_nombre (infixe arb) ) "" ;;





(** hofstadter

    @param index_fixe l'index de la partie fixe de la phrase, index_reel l'index de toute la phrase, phrase la phrase à transformer en phrase réflexive, iter le nombre d'itérations,  fin un booléen qui indique si l'on a trouvé ou non, le_nombre le nombre de lettre à inclure dans la phrase réflexive, début le début de phrase générée aléatoirement, concat_index_fixe la fonction de création de la phrase fixe et concat_index_mobile la fonction de création de la phrase mobile.
    @return au bout de 100 itérations maximum, retourne un booléen indiquant si on a obtenu une phrase réflexive et l'index de la phrase.

    val hofstadter :  arbre ->  arbre ->  string ->  int ->  bool ->  int ->  string ->  (char * int -> string -> string) ->  (char * int -> string -> string) -> bool * arbre = <fun>

*)

(** Tests : 
    hofstadter Vide index_reel phrase_entiere 0 false 26 debut concat_index_fixe concat_index_mobile ;; -> 
    (false,
    Noeud
    (('n', 22),
    Noeud
    (('f', 5),
     Noeud
      (('c', 6), Noeud (('a', 15), Vide, Noeud (('b', 3), Vide, Vide)),
       Noeud (('e', 39), Noeud (('d', 8), Vide, Vide), Vide)),
     Noeud
      (('j', 1),
       Noeud
        (('h', 4), Noeud (('g', 4), Vide, Vide),
         Noeud (('i', 23), Vide, Vide)),
       Noeud
        (('l', 8), Noeud (('k', 1), Vide, Vide),
         Noeud (('m', 3), Vide, Vide)))),
    Noeud
    (('t', 30),
     Noeud
      (('q', 8), Noeud (('p', 7), Noeud (('o', 13), Vide, Vide), Vide),
       Noeud (('r', 21), Vide, Noeud (('s', 16), Vide, Vide))),
     Noeud
      (('x', 7),
       Noeud
        (('v', 8), Noeud (('u', 21), Vide, Vide),
         Noeud (('w', 1), Vide, Vide)),
       Noeud (('z', 6), Noeud (('y', 1), Vide, Vide), Vide))))))


    avec :
    let index_affiche = creation_index abr true ;;
    let debut = "Vous avez bien raison de compter les lettres de cette phrase bizarre avant de la croire quand elle vous affirme qu'elle contient" ;;
    let fin = "et pour finir " ;;
    let milieu_1 = fragment_phrase (index_affiche) concat_index_entier 26 ;; 
    let phrase_entiere = debut^" "^milieu_1^"." ;;
    let index_reel = compte_mot_h (creation_index abr false) phrase_entiere ;;
*)
let rec hofstadter index_fixe index_reel phrase iter fin le_nombre debut concat_index_fixe concat_index_mobile= 
  let new_index_reel = compte_mot_h index_fixe phrase in 
  if (iter>100 || fin) then fin,index_reel 
  else if iter=0 then hofstadter (compte_mot_h (creation_index abr false) (debut^(fragment_phrase (index_reel) concat_index_fixe le_nombre))) index_reel (fragment_phrase (index_reel) concat_index_mobile le_nombre ) (iter+1) false le_nombre debut concat_index_fixe concat_index_mobile 
       else hofstadter index_fixe new_index_reel (fragment_phrase (new_index_reel) concat_index_mobile le_nombre) (iter+1) (compare_arbre index_reel new_index_reel le_nombre) le_nombre debut concat_index_fixe concat_index_mobile;;


(** boucle_hofstadter

    @param nb_essai le nombre d'essai sur un même corps de phrase, le_nombre le nombre de lettre à inclure dans la phrase à transformer en phrase réflexive, debut le début de phrase générée aléatoirement, concat_index_entier la fonction de création de la phrase toute entière, concat_index_mobile la fonction de création de la phrase mobile, concat_index_fixe la fonction de création de la phrase fixe et phrase_mobile la phrase mobile
    @return au bout d'un maximum de 100 situations initiales générées aléatoirement, retourne un booléen indiquant si la phrase trouvée est réflexive ou non, la phrase trouvée ainsi que le nombre de situations initiales générées.

    val boucle_hofstadter :  int ->  int ->  string ->  (char * int -> string -> string) ->  (char * int -> string -> string) ->  (char * int -> string -> string) -> 'a -> bool * int * string = <fun>

*)

(** Tests : 
    boucle_hofstadter 0 26 debut concat_index_entier concat_index_mobile concat_index_fixe [] ;; -> 
    (true,56, "Vous avez bien raison de compter les lettres de cette phrase bizarre avant de la croire quand elle vous affirme qu'elle contient onze a, trois b, sept c, huit d, trente-cinq e, cinq f, quatre g, sept h, vingt-sept i, un j, un k, huit l, trois m, vingt-et-un n, douze o, huit p, six q, seize r, dix-sept s, vingt-huit t, dix-neuf u, huit v, un w, six x, un y, et pour finir six z.")

*)
let rec boucle_hofstadter nb_essai le_nombre debut concat_index_entier concat_index_mobile concat_index_fixe phrase_mobile= 
  let index_affiche = creation_index abr true in
  let milieu_1 = fragment_phrase (index_affiche) concat_index_entier le_nombre in 
  let phrase_entiere = debut^" "^milieu_1^"." in
  let index_reel = compte_mot_h (creation_index abr false) phrase_entiere in
  match (hofstadter Vide index_reel phrase_entiere 0 false le_nombre debut concat_index_fixe concat_index_mobile) with 
  |(b,ir) -> if b || nb_essai > 100 then b,nb_essai,debut^" "^(fragment_phrase (ir) concat_index_entier le_nombre)^"."
               else boucle_hofstadter (nb_essai+1) le_nombre debut concat_index_entier concat_index_mobile concat_index_fixe phrase_mobile;;





(** initialise_pitrat

    @param le_nombre le nombre de lettres à inclure dans la phrase réflexive, corps_phrase le corps de la phrase généré aléatoirement, arbre_entier l'arbre de recherche binaire qui va comptabiliser l'ensemble des lettres dans la phrase, iter_int le nombre d'itération, phrase_totale l'ensemble de la phrase et concat_index_entier la fonction de création de la phrase toute entière
    @return l'arbre qui rend compte du nombre de lettre dans la phrase

    val initialise_pitrat : int -> string -> arbre -> int -> string -> (char * int -> string -> string) -> arbre = <fun>
*)

(** Tests : 
    initialise_pitrat 26 debut index_aleatoire 97 "" concat_index_entier ;; ->
    Noeud
    (('n', 35),
    Noeud
    (('f', 8),
    Noeud
     (('c', 12), Noeud (('a', 28), Vide, Noeud (('b', 3), Vide, Vide)),
      Noeud (('e', 62), Noeud (('d', 10), Vide, Vide), Vide)),
    Noeud
     (('j', 1),
      Noeud
       (('h', 5), Noeud (('g', 6), Vide, Vide),
        Noeud (('i', 35), Vide, Vide)),
      Noeud
       (('l', 8), Noeud (('k', 1), Vide, Vide), Noeud (('m', 4), Vide, Vide)))),
    Noeud
    (('t', 37),
    Noeud
     (('q', 14), Noeud (('p', 6), Noeud (('o', 13), Vide, Vide), Vide),
      Noeud (('r', 23), Vide, Noeud (('s', 16), Vide, Vide))),
    Noeud
     (('x', 7),
      Noeud
       (('v', 9), Noeud (('u', 19), Vide, Vide),
        Noeud (('w', 1), Vide, Vide)),
      Noeud (('z', 7), Noeud (('y', 1), Vide, Vide), Vide))))

    avec :

    let debut = "Vous avez bien raison de compter les lettres de cette phrase bizarre avant de la croire quand elle vous affirme qu'elle contient" ;;

    let index_aleatoire = (creation_index abr true) ;;


*)
let rec initialise_pitrat le_nombre corps_phrase arbre_entier iter_int phrase_totale concat_index_entier=
  let phrase_totale = corps_phrase^" "^(fragment_phrase (arbre_entier) concat_index_entier le_nombre) in 
  if iter_int >(le_nombre+96)then arbre_entier 
  else initialise_pitrat le_nombre corps_phrase (maj_index_courant arbre_entier (char_of_int iter_int) (compte_lettre phrase_totale 0 (char_of_int iter_int))) (iter_int+1) phrase_totale concat_index_entier;;




(** corps_pitrat

    @param lettre la lettre courante modifiée, le_nombre le nombre de lettre à inclure dans la phrase réflexive, index_fixe_infixe l'index de la phrase fixe sous forme de liste, index_fixe l'index de la phrase fixe, index_courant l'index de la phrase courante, phrase_mobile la phrase mobile et concat_index_mobile la fonction de création de la phrase mobile.
    @return réalise une modification complète de la phrase en la modifiant lettre par lettre. A chaque itération, on regarde si la phrase obtenue n'est pas réflexive auquel cas on stoppe le processus

    val corps_pitrat : char -> int -> (char * int) list -> arbre -> arbre -> string -> (char * int -> string -> string) -> bool * arbre * string = <fun>

*)

(** Tests : 
    corps_pitrat ' ' 26 (infixe index_fixe) index_fixe index_courant phrase_mobile concat_index_mobile ;; ->('z', false,
    Noeud
    (('n', 96),
    Noeud
    (('f', 47),
     Noeud
      (('c', 29), Noeud (('a', 53), Vide, Noeud (('b', 46), Vide, Vide)),
       Noeud (('e', 36), Noeud (('d', 24), Vide, Vide), Vide)),
     Noeud
      (('j', 58),
       Noeud
        (('h', 70), Noeud (('g', 32), Vide, Vide),
         Noeud (('i', 66), Vide, Vide)),
       Noeud
        (('l', 4), Noeud (('k', 14), Vide, Vide),
         Noeud (('m', 10), Vide, Vide)))),
    Noeud
    (('t', 35),
     Noeud
      (('q', 36), Noeud (('p', 89), Noeud (('o', 7), Vide, Vide), Vide),
       Noeud (('r', 22), Vide, Noeud (('s', 53), Vide, Vide))),
     Noeud
      (('x', 64),
       Noeud
        (('v', 94), Noeud (('u', 65), Vide, Vide),
         Noeud (('w', 56), Vide, Vide)),
       Noeud (('z', 38), Noeud (('y', 45), Vide, Vide), Vide)))),
    "cinquante-trois  vingt-neuf vingt-quatre trente-six quarante-sept trente-deux soixante-dix soixante-six     quatre-vingt-seize sept quatre-vingt-neuf trente-six vingt-deux cinquante-trois trente-cinq soixante-cinq quatre-vingt-quatorze  soixante-quatre  trente-huit ")

    avec :

    let index_fixe = (creation_index abr true) ;;
    let index_courant = (creation_index abr true) ;;
    let phrase_mobile = "dix  cinq dix vingt-huit cinq cinq six vingt-huit     vingt douze six trois dix-huit dix-neuf vingt-six dix-sept huit  onze  six ";;

*)

let rec corps_pitrat lettre le_nombre index_fixe_infixe index_fixe index_courant phrase_mobile concat_index_mobile = 
  let index_reel = compte_mot_h index_fixe phrase_mobile in 
  if (compare_arbre index_reel index_courant le_nombre) then true,index_courant, phrase_mobile 
  else match index_fixe_infixe with
    |[]->false,index_courant,phrase_mobile
       |(l,n)::s -> if l > (char_of_int (le_nombre+96)) then false,index_courant,phrase_mobile 
                      else if (est_fixe l) then corps_pitrat l le_nombre s index_fixe index_courant phrase_mobile concat_index_mobile 
                           else let nombre_l = compte_lettre phrase_mobile n l in 
                                  let index_courant = maj_index_courant index_courant l (nombre_l) in 
                                  let phrase_mobile = fragment_phrase (index_courant) concat_index_mobile le_nombre in 
                                  corps_pitrat l le_nombre s index_fixe index_courant phrase_mobile concat_index_mobile;;

(** pitrat

    @param le_nombre le nombre de lettres à inclure dans la phrase, index_fixe l'index de la partie fixe de la phrase, index_courant l'index de toute la phrase, phrase_mobile la phrase mobile, iter le nombre d'itérations, fin un booléen qui indique si l'on a trouvé ou non et concat_index_mobile la fonction de création de la phrase mobile.
    @return au bout de 100 itérations maximum, retourne un booléen indiquant si on a obtenu une phrase réflexive et l'index de la phrase.

    val pitrat : int -> arbre -> arbre -> string -> int -> bool -> (char * int -> string -> string) -> bool * arbre =
    <fun>


*)

(** Tests : 
    pitrat 2 index_fixe index_courant phrase_mobile 0 false concat_index_mobile ;; -> 
    (true,
    Noeud
    (('n', 84),
    Noeud
    (('f', 17),
     Noeud
      (('c', 63), Noeud (('a', 10), Vide, Noeud (('b', 3), Vide, Vide)),
       Noeud (('e', 85), Noeud (('d', 27), Vide, Vide), Vide)),
     Noeud
      (('j', 28),
       Noeud
        (('h', 70), Noeud (('g', 31), Vide, Vide),
         Noeud (('i', 70), Vide, Vide)),
       Noeud
        (('l', 8), Noeud (('k', 79), Vide, Vide),
         Noeud (('m', 88), Vide, Vide)))),
    Noeud
    (('t', 37),
     Noeud
      (('q', 58), Noeud (('p', 35), Noeud (('o', 95), Vide, Vide), Vide),
       Noeud (('r', 80), Vide, Noeud (('s', 32), Vide, Vide))),
     Noeud
      (('x', 95),
       Noeud
        (('v', 51), Noeud (('u', 19), Vide, Vide),
         Noeud (('w', 19), Vide, Vide)),
       Noeud (('z', 79), Noeud (('y', 42), Vide, Vide), Vide)))))

    avec :
    let index_aleatoire = (creation_index abr true) ;;
    let index_courant = initialise_pitrat 2 debut index_aleatoire 97 "" concat_index_entier ;;
    let milieu_2 = fragment_phrase (index_courant) concat_index_fixe 2 ;;
    let index_fixe = compte_mot_h (creation_index abr false) (debut^milieu_2) ;;
    let phrase_mobile = fragment_phrase (index_courant) concat_index_mobile 2 ;;

*)
let rec pitrat le_nombre index_fixe index_courant phrase_mobile iter fin concat_index_mobile=
  if iter>100 || fin then fin,index_courant 
  else match (corps_pitrat ' ' le_nombre (infixe index_fixe) index_fixe index_courant phrase_mobile concat_index_mobile) with
         |(indic,index_courant, phrase_mobile) -> pitrat le_nombre index_fixe index_courant phrase_mobile (iter+1) indic concat_index_mobile;;



(** boucle_pitrat

    @param nb_essai le nombre d'essai sur un même corps de phrase, le_nombre le nombre de lettres à inclure dans la phrase réflexive, début le début de phrase généré aléatoirement, concat_index_entier la fonction de création de la phrase toute entière, concat_index_mobile la fonction de création de la phrase mobile, concat_index_fixe la fonction de création de la phrase fixe et phrase_mobile la phrase mobile. 
    @return au bout d'un maximum de 100 situations initiales générées aléatoirement, retourne un booléen indiquant si la phrase trouvée est réflexive ou non, la phrase trouvée ainsi que le nombre de situations initiales générées.

    val boucle_pitrat : int -> int -> string -> (char * int -> string -> string) -> (char * int -> string -> string) -> (char * int -> string -> string) -> string list -> bool * int * string = <fun>
*)


(** Tests : 
    boucle_pitrat 0 26 debut concat_index_entier concat_index_mobile concat_index_fixe [] ;; -> (95, "Vous avez bien raison de compter les lettres de cette phrase bizarre avant de la croire quand elle vous affirme qu'elle contient onze a, trois b, sept c, huit d, trente-cinq e, cinq f, quatre g, sept h, vingt-sept i, un j, un k, huit l, trois m, vingt-et-un n, douze o, huit p, six q, seize r, dix-sept s, vingt-huit t, dix-neuf u, huit v, un w, six x, un y, et pour finir six z.")

*)
let rec boucle_pitrat nb_essai le_nombre debut concat_index_entier concat_index_mobile concat_index_fixe phrase_mobile = 
  let index_aleatoire = (creation_index abr true) in
  let index_courant = initialise_pitrat le_nombre debut index_aleatoire 97 "" concat_index_entier in
  let milieu_2 = fragment_phrase (index_courant) concat_index_fixe le_nombre in
  let index_fixe = compte_mot_h (creation_index abr false) (debut^milieu_2) in
  let phrase_mobile = fragment_phrase (index_courant) concat_index_mobile le_nombre in 
  match pitrat le_nombre index_fixe index_courant phrase_mobile 0 false concat_index_mobile with 
  |(b,index_courant) -> if (b || nb_essai > 100) then b,nb_essai,debut^" "^(fragment_phrase (index_courant) concat_index_entier le_nombre)^"." 
                          else boucle_pitrat (nb_essai+1) le_nombre debut concat_index_entier concat_index_mobile concat_index_fixe (tradliste phrase_mobile) ;;






(** generateur

    @param boucle_f une fonction de génération de phrases réflexives (boucle_pitrat ou boucle_hofstadter), le_nombre le nombre de lettres à inclure dans la phrase réflexive, langue la langue de traduction des chiffres littéraux, iter_corps le nombre de corps différents aléatoires sur lesquels on veut tester la réflexivité des phrases et list_solutions la liste des phrases réflexives obtenues et nombre_phrases_trouvees le nombre de phrases reflexives trouvées
    @return retourne une liste constituée de toutes les phrases réflexives trouvées ainsi que du nombre_essai pour les trouver.

    val generateur : (int -> 'a -> string ->(char * int -> string -> string) ->(char * int -> string -> string) -> (char * int -> string -> string) -> 'b list -> bool * 'c * string) ->'a -> char -> int -> ('c * string) list -> int -> ('c * string) list * int = <fun>
*)

(** Tests : 

    generateur boucle_pitrat 10 'f' 5 [] 0;; -> ([(0,
    "Ami lecteur, vous avez bien raison de compter avec soin toutes les lettres de cette bizarre phrase avant de la croire quand elle vous affirme qu'elle contient douze 'a', trois 'b', sept 'c', sept 'd', trente-trois 'e', trois 'f', un 'g', deux 'h', treize 'i', un 'j'.");
    (7,
    "Ami lecteur, vous avez vraiment raison de compter et de recompter avec soin toutes les lettres de cette phrase avant de la croire quand elle affirme qu'elle contient quatorze 'a', un 'b', huit 'c', six 'd', trente-quatre 'e', trois 'f', un 'g', trois 'h', treize 'i', un 'j'.");
    (0,
    "Vous avez bien raison de compter et de recompter toutes les lettres de cette bizarre phrase avant de la croire quand elle affirme qu'elle contient dix 'a', trois 'b', six 'c', dix 'd', trente-et-un 'e', trois 'f', un 'g', deux 'h', douze 'i', un 'j'.")],
    3)

    generateur boucle_hofstadter 10 'f' 5 [] 0;; ->([(57,
    "Cher lecteur, vous avez raison de compter et de recompter soigneusement toutes les lettres de cette bizarre phrase avant de la croire quand elle vous affirme qu'elle contient exactement quatorze 'a', deux 'b', neuf 'c', neuf 'd', quarante-deux 'e', six 'f', deux 'g', trois 'h', neuf 'i', un 'j'.");
    (6,
    "Chers lecteurs, vous avez raison de compter et de recompter toutes les lettres de cette bizarre phrase avant de la croire quand elle vous affirme qu'elle contient exactement douze 'a', deux 'b', dix 'c', neuf 'd', trente-huit 'e', cinq 'f', un 'g', quatre 'h', neuf 'i', un 'j'.");
    (23,
    "Cher lecteur, vous avez vraiment raison de compter toutes les lettres de cette bizarre phrase avant de la croire quand elle affirme qu'elle contient exactement quatorze 'a', deux 'b', neuf 'c', sept 'd', trente-cinq 'e', quatre 'f', un 'g', trois 'h', dix 'i', un 'j'.")],
    3)
*)
let rec generateur boucle_f le_nombre langue iter_corps list_solutions nombre_phrases_trouvees=
  let debut = tradmot(List.map choix_alea_gabarits gabarits_debut) in
  let fin = tradmot(List.map choix_alea_gabarits gabarits_fin) in
  let concat_index_entier (a,b) reste = (litt b langue)^" "^"'"^(String.make 1 a)^"'"^(if reste="" then "" else ","^" "^(if a='y' then fin else "")^reste) in 
  let concat_index_mobile (a,b) reste = (if not(est_fixe a) then (litt b langue) else "")^" "^reste in 
  let concat_index_fixe (a,b) reste = (if (est_fixe a) then (litt b langue) else "")^(String.make 1 a)^(if a='y' then fin else "")^reste in 
  match (boucle_f 0 le_nombre debut concat_index_entier concat_index_mobile concat_index_fixe []) with 
  |b,fin,phrase -> if iter_corps = 0 then list_solutions,nombre_phrases_trouvees 
    else generateur boucle_f le_nombre langue (iter_corps-1) 
        (if b then ((fin,initiale_maj(phrase))::list_solutions) 
         else  list_solutions)
        (if b then (nombre_phrases_trouvees+1) 
         else nombre_phrases_trouvees);;
          
