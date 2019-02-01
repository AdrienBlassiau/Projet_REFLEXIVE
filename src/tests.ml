open Printf ;;
open Phrase_reflexive ;;

(*Données pour les test*)

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

let abr3 = 
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


let tab = 
[('a', 0); ('b', 0); ('c', 0); ('d', 0); ('e', 0); ('f', 0); ('g', 0);('h', 0); ('i', 0); ('j', 0); ('k', 0); ('l', 0); ('m', 0); ('n', 0);('o', 0); ('p', 0); ('q', 0); ('r', 0); ('s', 0); ('t', 0); ('u', 0);('v', 0); ('w', 0); ('x', 0); ('y', 0); ('z', 0)] ;;

let debut = "Vous avez bien raison de compter les lettres de cette phrase bizarre avant de la croire quand elle vous affirme qu'elle contient" ;;
let fin = "et pour finir " ;;

let concat_index_entier (a,b) reste = (litt b 'f')^" "^(String.make 1 a)^(if reste="" then "" else ","^" "^(if a='y' then fin else "")^reste) ;; 
let concat_index_mobile (a,b) reste = (if not(est_fixe a) then (litt b 'f') else "")^" "^reste ;;
let concat_index_fixe (a,b) reste = (if (est_fixe a) then (litt b 'f') else "")^(String.make 1 a)^(if a='y' then fin else "")^reste ;;

let index_affiche = creation_index abr true ;;
let milieu_1 = fragment_phrase (index_affiche) concat_index_entier 26 ;; 
let phrase_entiere = debut^" "^milieu_1^"." ;;
let index_reel = compte_mot_h (creation_index abr false) phrase_entiere ;;
let index_aleatoire = (creation_index abr true) ;;
let index_fixe = (creation_index abr true) ;;
let index_courant = (creation_index abr true) ;;

let phrase_mobile = "dix  cinq dix vingt-huit cinq cinq six vingt-huit     vingt douze six trois dix-huit dix-neuf vingt-six dix-sept huit  onze  six ";;








printf "*************************************************\n" ;;
printf "*****Tests des diverses fonctions du projet******\n" ;;
printf "*************************************************\n" ;;



printf "Tests chaine_vers_liste_de_chaine\n" ;;
printf "...\n" ;;
assert( chaine_vers_liste_de_chaine "Bonjour le monde" 0 "" [] = ["Bonjour"; "le"; "monde"]);;
assert( chaine_vers_liste_de_chaine "" 0 "" [] = [""]);;

printf "Tests réussis\n\n\n" ;;



printf "Tests tradliste\n" ;;
printf "...\n" ;;

assert(tradliste "Bonjour  le    monde" = ["Bonjour"; "le"; "monde"]);;
assert(tradliste ""= [""]);;

printf "Tests réussis\n\n\n" ;;


printf "Tests tradmot\n" ;;
printf "...\n" ;;

assert(tradmot ["";"Bonjour";"le";"monde"]= "Bonjour le monde");;
assert(tradmot ["Bonjour";"le";"";"monde"]= "Bonjour le monde");;
assert(tradmot ["Bonjour";"le";"monde";"";"";"";""]= "Bonjour le monde");;
assert(tradmot ["Bonjour";"";"le";"monde"]= "Bonjour le monde");;
assert(tradmot ["";"Bonjour";"le";"monde"]= "Bonjour le monde");;
assert(tradmot ["";"";"";"Bonjour";"";"le";"monde";""]= "Bonjour le monde");;

printf "Tests réussis\n\n\n" ;;

 

printf "Tests infixe\n" ;;
printf "...\n" ;;


assert(infixe abr= [('a', 0); ('b', 0); ('c', 0); ('d', 0); ('e', 0); ('f', 0); ('g', 0); ('h', 0); ('i', 0); ('j', 0); ('k', 0); ('l', 0); ('m', 0); ('n', 0); ('o', 0); ('p', 0); ('q', 0); ('r', 0); ('s', 0); ('t', 0); ('u', 0); ('v', 0); ('w', 0); ('x', 0); ('y', 0); ('z', 0)]);;
assert(infixe Vide= []);;

printf "Tests réussis\n\n\n" ;;



printf "Tests creation_index\n" ;;
printf "...\n" ;;
 

assert(creation_index abr false =
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
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide)))));;

printf "Tests réussis\n\n\n" ;;



printf "Tests compte_lettre_arbre\n" ;;
printf "...\n" ;;
 


assert(compte_lettre_arbre Vide 'a'= Vide);;
assert(compte_lettre_arbre abr 'a'= 
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
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide)))));;

printf "Tests réussis\n\n\n" ;;


printf "Tests compte_mot_global\n" ;;
printf "...\n" ;;

assert( compte_mot_global abr "Bonjour" 0 = Noeud
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
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide)))));;

assert(compte_mot_global Vide "Bonjour" 0 = Vide );;

printf "Tests réussis\n\n\n" ;;



printf "Tests compte_lettre_dans_mot\n" ;;
printf "...\n" ;;


assert(compte_lettre_dans_mot "Bonjour" 0 'o' 0= 2);;
assert(compte_lettre_dans_mot "Bonjour" 1 'o' 0= 3);;

printf "Tests réussis\n\n\n" ;;


printf "Tests maj_index_courant\n" ;;
printf "...\n" ;;


assert(maj_index_courant abr 'a' 32=
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
      Noeud (('z', 0), Noeud (('y', 0), Vide, Vide), Vide)))));;

printf "Tests réussis\n\n\n" ;;


printf "Tests litt\n" ;;
printf "...\n" ;;

assert(litt 0 'f'= "");;
assert(litt 80 'f'= "quatre-vingts");;
assert(litt 80 's'= "huitante");;
assert(litt 78 'b'= "septante-huit");;
assert(litt 13 'f'= "treize");;

printf "Tests réussis\n\n\n" ;;



printf "Tests pre_choix_alea_gabarits\n" ;;
printf "...\n" ;;

assert(pre_choix_alea_gabarits ["cher lecteur,";"ami lecteur,";"chers lecteurs,";""] 0 2= "chers lecteurs,");;
assert(pre_choix_alea_gabarits ["vous avez"] 0 0= "vous avez" );;
assert(pre_choix_alea_gabarits ["toutes";""] 0 1= "" );; 

printf "Tests réussis\n\n\n" ;;


printf "Tests initiale_maj\n" ;;
printf "...\n" ;;

assert(initiale_maj ""= "" );;
assert(initiale_maj "ceci est une chaine de caracteres"= "Ceci est une chaine de caracteres");;

printf "Tests réussis\n\n\n" ;;



printf "Tests compare_arbre\n" ;;
printf "...\n" ;;

assert(compare_arbre abr1 abr2 10 = false);;
assert(compare_arbre abr1 abr2 9 = true);;
printf "Tests réussis\n\n\n" ;;


printf "Tests decoupe_liste\n" ;;
printf "...\n" ;;

 
assert(decoupe_liste 5 tab= [('a', 0); ('b', 0); ('c', 0); ('d', 0); ('e', 0)]);;
assert(decoupe_liste (-1) tab= []);;
printf "Tests réussis\n\n\n" ;;


printf "Tests est_fixe\n" ;;
printf "...\n" ;;

assert(est_fixe 'a' = false);;
assert(est_fixe 'l' = true);;
printf "Tests réussis\n\n\n" ;;


printf "Tests fragment_phrase\n" ;;
printf "...\n" ;;
assert(fragment_phrase abr3 concat_index_fixe 12 = "adix-neufbcdefghiquatrejcinquante-sixkunl");;

assert(fragment_phrase abr3 concat_index_entier 14 = "cinquante-cinq a, dix-neuf b, soixante c, quarante-quatre d, soixante-et-onze e, quatre-vingt-dix-sept f, soixante-quatre g, quatre-vingt-dix-huit h, quatre-vingt-dix-huit i, quatre j, cinquante-six k, un l, soixante-et-onze m, quarante-cinq n");;

assert(fragment_phrase abr3 concat_index_mobile 13 = "cinquante-cinq  soixante quarante-quatre soixante-et-onze quatre-vingt-dix-sept soixante-quatre quatre-vingt-dix-huit quatre-vingt-dix-huit     ");;

printf "Tests réussis\n\n\n" ;;


printf "Tests generateur\n\n" ;;
printf "...\n" ;;

let res1 = generateur boucle_hofstadter 10 'f' 5 [] 0;;
let res2 = generateur boucle_pitrat 10 'f' 3 [] 0;;


printf "Hofstadter :\n" ;;
print_list_string res1 ;;



printf "\n\nPitrat :\n" ;;
print_list_string res2 ;;



printf "\n\nSucces de tous les tests\n" ;;
