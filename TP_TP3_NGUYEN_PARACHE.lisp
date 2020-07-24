;; SYSTEME EXPERT POUR CLASSER UN FILM

;;-----------------------------------------------------------------------;;
;;------------------------BASE D'INFORMATION-----------------------------;;
;;----------------------- -----------------------------------------------;;
;; BASE DE RÈGLES
(defvar *BdR*
    '((R1 ((TECHNOLOGIE inexistante)) (MONDE imaginaire))
    (R2 ((TECHNOLOGIE existante)) (MONDE reel))

    (R3 ((THEME espace) (MONDE imaginaire)) (DECOR artificiel))
    (R4 ((THEME evenement-ayant-eu-lieu))(CARACTERISTIQUE-PERSO reel))
    (R5 ((THEME espace) (MONDE reel)) (GENRE documentaire))
    (R6 ((THEME nature) (BUT instruire))(GENRE documentaire))
    (R7 ((THEME vie)) (PERSO-PRINC humain))
    (R8 ((THEME science) (EPOQUE future)) (GENRE science-fiction))
    (R9 ((THEME science) (EPOQUE passe)) (GENRE documentaire))
    (R10 ((THEME evenement) (EPOQUE passe)) (THEME evenement-ayant-eu-lieu))
    (R11 ((THEME crime) (PERSO-PRINC criminel))(GENRE thriller))
    (R12 ((THEME ses-defauts) (PERSO-PRINC criminel)) (GENRE thriller))
    (R13 ((THEME ses-defauts) (PERSO-PRINC personne-ordinaire)) (GENRE comedie))

    (R14 ((MONDE imaginaire) (BUT divertir)) (GENRE science-fiction))
    (R15 ((DECOR foret) (PERSO-PRINC animal)) (THEME nature))
    (R16 ((DECOR ocean) (PERSO-PRINC animal)) (THEME nature))

    (R17 ((MONDE reel) (BUT instruire)) (GENRE documentaire))
    (R18 ((MONDE imaginaire) (THEME sauver-des-gens)) (GENRE super-heros))
    
    (R19 ((PERSO-PRINC humain) (GENRE documentaire)) (GENRE biographie))
    (R20 ((PERSO-PRINC humain) (CARACTERISTIQUE-PERSO casse-des-choses)) (THEME ses-defauts))
    (R21 ((PERSO-PRINC humain) (CARACTERISTIQUE-PERSO tuer-qqn)) (THEME ses-defauts))
    (R22 ((PERSO-PRINC inhumain) (MONDE imaginaire)) (DECOR artificiel))
    (R23 ((PERSO-PRINC heros)) (GENRE super-heros))

    (R24 ((CARACTERISTIQUE-PERSO tuer-qqn)) (PERSO-PRINC criminel))
    (R25 ((CARACTERISTIQUE-PERSO comportement-humain) (PERSO-PRINC robot)) (MONDE imaginaire))
    (R29 ((CARACTERISTIQUE-PERSO comportement-humain) (PERSO-PRINC inhumain)) (MONDE imaginaire))
    (R26 ((CARACTERISTIQUE-PERSO pouvoirs)) (MONDE imaginaire))
    (R27 (CARACTERISTIQUE-PERSO reel) (MONDE reel))
    (R28 ((CARACTERISTIQUE-PERSO pouvoirs) (PERSO-PRINC humain)) (PERSO-PRINC heros))
    ))

;; BASE D'ATTRIBUTS
(defvar *BdA*
  '((TECHNOLOGIE (inexistante existante))
  (MONDE (imaginaire reel))
  (THEME (espace evenement-ayant-eu-lieu nature vie science evenement crime ses-defauts sauver-des-gens))
  (DECOR (artificiel foret ocean))
  (CARACTERISTIQUE-PERSO (reel casse-des-choses tuer-qqn pouvoirs comportement-humain))
  (EPOQUE (passe future))
  (PERSO-PRINC (humain criminel animal robot inhumain))
  (BUT (divertir instruire))
  )
)

;; BASE DE BUT
(defvar *BdB*
  '(science-fiction biographie documentaire super-heros comedie thriller)
)

;; ------------------------FONCTIONS DE SERVICE------------------------;;
(defun premisses (regle)
  (cadr (assoc regle *BdR*)) ; retourne les prémisses d'une règle
)


;; Retourne la liste de tous les ID des règles applicables à partir de la base de faits
(defun regles-applicables (bdf reglesUtilisees)
  (let ((res) (regles-non-utilisees))
    ;;creer la liste de regle-non-utilisees
    (dolist (r *bdr*)
      (if (null (member (car r) reglesUtilisees :test #'equal))
        (push r regles-non-utilisees)
      )
    )

    (dolist (r regles-non-utilisees) 
      (if (equal (regle-favorable (car r) bdf) 'ok)
          (push (car r) res)
      )
    )
  res    
  )
)



;;vérifier si la règle est applicable. Ses prémisses doivent se trouver dans la base de faits
(defun regle-favorable (r bdf)
  (let ((premisses (premisses r)) (compter 0))
    ;; compter nombre de prémisses de la règle présentant dans la base de faits
    (dolist (p premisses)
      (dolist (f bdf)
        (if (equal p f)
          (setq compter (+ 1 compter))
        )
      )
    )
    ;; retourne ok si tous les prémisses de la règles se trouver dans la base de faits
    (if (eq compter (length premisses))
      (return-from regle-favorable 'ok)
    )
  )
)


;;--------------------------------------------------------------------------------;;
;;------------------------CHAINAGE AVANT EN PROFONDEUR-----------------------------;;
;;--------------------------------------------------------------------------------;;
(defun chainage-avant-profondeur(but bdf reglesUtilisees)
  ;;si le but se trouve dans la bdf, on s'arrête et affiche les règles utilisées
  (when (member but bdf :test #'equal)
    (if (equal 0 (length reglesUtilisees))
      (format t "Le but est déjà dans la base de faits")
      (progn
        (format t "~%*************** ~s VÉRIFIÉ ! ***************~%Des règles utilisées : ~s~%" but (reverse reglesUtilisees))
        (dolist (r (reverse reglesUtilisees))
          (format t " ~s : ~s ~%" r (assoc r *bdr*))
        )
      )
    )
    (return-from chainage-avant-profondeur (reverse reglesUtilisees))
  )
  ;;//-----------------------------------TRAITEMENT-----------------------------------//
  ;; récupérer des règles applicables, les tester
  (format t "~%Chemin parcouru : ~s ~%Base de faits : ~s" (reverse reglesUtilisees) bdf)
  (setq regles (regles-applicables bdf reglesUtilisees))
  (if (equal 0 (length regles))
    (format t "~%_________ ~s N'EST PAS PROUVÉ _________~%" but)
    (progn
      (when regles (format t "~%Règles candidates: ~s~%" regles)
          (dolist (regle regles)
            ;; ajouter la conclusion de regle à la base de faits
            ;; appeller la fonction chainage-avant-profondeur pour tester si le but est prouvé 
            (pushnew (caddr(assoc regle *bdr*)) bdf) ;; modification de la bdf
            (unless (member regle reglesUtilisees)
              (chainage-avant-profondeur but bdf (cons regle reglesUtilisees))) 
        )
      )
    )  
  )
)

;;--------------------------------------------------------------------------------;;
;;------------------------CHAINAGE AVANT EN LARGEUR-----------------------------;;
;;--------------------------------------------------------------------------------;;

(defun chainage-avant-largeur(but bdf)
  (let ((parcours) (reglesChoisies) (bdr *bdr*) tmp)
    (loop
      (when (member but bdf :test #'equal)
          (format t "~%*************** ~s VÉRIFIÉ ! ***************~%Des règles utilisées : ~s~%" but (reverse parcours))
          (dolist (r (reverse parcours))
            (format t " ~s : ~s ~%" r (assoc r *bdr*))
          )
          (return 'T)
      )
      ;; chercher une liste des règles applicables, les retirer dans la bdr
      (format t "~%Chemin parcouru : ~s ~%Base de faits : ~s" parcours bdf)
      (dolist (r bdr)
        (when (equal (regle-favorable (car r) bdf) 'ok)
          (pushnew (car r) reglesChoisies)
          (setq bdr (remove r bdr))
        )
      )
      (format t "~%Règles candidates: ~s~%" reglesChoisies)
      (if (equal 0 (length reglesChoisies))
        (progn
          (format t "~%_________ ~s N'EST PAS PROUVÉ _________~%" but)
          (return NIL)
        )
        (progn
          (setq tmp (pop reglesChoisies))
          (pushnew (caddr(assoc tmp *bdr*)) bdf)
          (push tmp parcours)
        )     
      )
    )
  )
)


;;------------------------------------------------------------------------;;
;;------------------------FONCTION PRINCIPALE-----------------------------;;
;;------------------------------------------------------------------------;;
(defun getBut()
  (let ((res))
    (format t "~% Des genres de film : ~%")
    (dolist (b *BdB*)
      (format t " - ~s ~%" b)
    )
    (format t "~% Veuillez entrer le genre de film à vérifier (ex : (GENRE documentaire)) : ")
    (setq res (read))
    (format t "~% But défini : ~s ~%" res)
    res
  )
) 

(defun getChainage()
  (let ((res))
    (format t "~% Veuillez entrer le chainage à traiter : ~%")
    (format t " 0. Chainage avant en profondeur ~%")
    (format t " 1. Chainage avant en largeur ~%")
    (format t " 2. Les deux ~%")
    (setq res (read))
    res
  )
) 

(defun checkFilm()
(let (bdf (nbA 0) attr but choix)
  (format t "~%//------------------------------------------------------------------------//~%")
  (format t "~%//----------------------------FILM CLASSIFICATION-------------------------//~%")
  (format t "~%//------------------------------------------------------------------------//~%")
  (format t "~%Des caracteristiques au choix : ~%~%")
  (dotimes (a  (length *BdA*))
    (setq attr (nth a *BdA*))
    (format t "~s. ~s : " a (car attr))
    (dolist (v (cadr attr))
      (if (equal v (car (last (cadr attr))))
        (format t "~s ~%" v)
        (format t "~s, " v)
      )
    )
  )
  (format t "~% Veuillez entrer les caracteristiques (ex : ((MONDE reel))) : ")
  (setq bdf (read))
  (format t "~% Base de faits établie : ~s ~%" bdf)
  (setq but (getBut))
  (setq choix (getChainage))
  (cond
    ((eq choix 0) 
      (format t "~%~%//------------ VÉRIFICATION EN CHAINAGE EN PROFONDEUR ------------//~%~%")
      (chainage-avant-profondeur but bdf nil)
      (format t "~%~%//------------ FIN DE VÉRIFICATIOIN ------------//~%~%")
    )

    ((eq choix 1) 
      (format t "~%~%//------------ VÉRIFICATION EN CHAINAGE EN LARGEUR ------------//~%~%")
      (chainage-avant-largeur but bdf)
      (format t "~%~%//------------ FIN DE VÉRIFICATIOIN ------------//~%~%")
    )

    ((eq choix 2) 
      (format t "~%~%//------------ VÉRIFICATION EN CHAINAGE EN PROFONDEUR ------------//~%~%")
      (chainage-avant-profondeur but bdf nil)
        (format t "~%~%//------------ VÉRIFICATION EN CHAINAGE EN LARGEUR ------------//~%~%")
      (chainage-avant-largeur but bdf)
      (format t "~%~%//------------ FIN DE VÉRIFICATIOIN ------------//~%~%")
    )
  )
)
)
(checkFilm)


