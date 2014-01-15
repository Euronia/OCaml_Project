
													(*Projet*)
													
								(*Structure de données*)				
					(*Question 1 :*) 
					
				(* Pour la représentation graphique : voir feuille annexe *) 

				type elt = int ;;

				type skip_list = 
						| Nil 
						| Level of skip_list * cell 
				and cell = 
					{ lvl : int ; l_elt : ( elt * skip_list) list } 
				;;

				let liste_un : skip_list = 
						Level 
						( Level ( Nil,
						  { lvl = 2; l_elt = [( 2, Level ( Nil ,
																			{ lvl = 0; l_elt = [(3 , Nil); (5, Nil)]}))]}),
					
						{lvl = 3; l_elt = [(8, Level (Level (Nil, {lvl = 0; l_elt = [(13 , Nil)]}),
							{lvl = 1; l_elt = [(21, Level ( Nil , 
								{lvl = 0 ; l_elt = [(34, Nil)]}))]}))]})
																			 
				let liste_un_ajout : skip_list = 
						Level 
						( Level ( Nil,
						  { lvl = 2; l_elt = [( 2, Level ( Nil ,
																			{ lvl = 0; l_elt = [(3 , Nil); (5, Nil)]}))]}),
					
						{lvl = 3; l_elt = [(6 , Nil);(8, Level (Nil , {lvl = 2 ; l_elt = [(12 ,Level ( Level ( Nil , 
																													{ lvl = 0 ; l_elt = [( 13, Nil )]}) , 
																												{lvl = 1 ; l_elt = [( 15 ,Nil);(21, Level ( Nil , 
																												{lvl = 0 ; l_elt = [(34,Nil)]}))]}))]}))]});; 

																								
					(*Question 2 :*)
					
				(*
					Oui ,  cela représente une liste à sauts à une seule valeur.
					Je ne trouve pas de valeurs du type skip_lisdt qui ne représentent pas une liste à sauts .
				*)
					
					(*Question 3 :*)

				let level ( l : skip_list ) = 
					match l with 
					| Nil -> failwith "Function level error : list = Nil " ; 
					| Level (m , c) -> c.lvl ;;

				let key ( l : skip_list) = 
					match l with
					| Nil -> failwith "Function key error : list = Nil " ; 
					| Level (m, c) -> let (e ,_) = List.hd c.l_elt in e 
				;;
				
				let right (l : skip_list) =
						match l with
						| Nil -> failwith "Function right error : list = Nil "
						| Level (l2, c) ->
							match c.l_elt with
							| [] -> failwith "Function right error : list.l_elt = [] "
							| e::[] -> let (_, lelt) = e in lelt 
							| e::m -> Level (Nil, {lvl=c.lvl; l_elt=m})
					;;
				

				let left (l : skip_list) = 
					match l with
					| Nil -> failwith "Function left error : list = Nil "; 
					| Level (m, c) -> m
				;;
									(* Recherche d'un élément :*)
									
						(*Question 4 :*)
						
				let rec find_list (l :skip_list) (e: elt) = 
					if l = Nil then 
						false 
					else 
						let k = key l 
						in 
							if e < k then 
								find_list ( left l ) e 
							else if e > k then
								find_list ( right l ) e 
							else 
								true ;;

						(*Question 5 :*)
				
				(* 	Le coup d'une recherche d'un élément dans une liste à sauts dont les éléments sont tous de même niveau est de O(n) 
					C'est le même que celui d'une recherche d'un élément dans un tableau  *) 
						
						(*Question 6 :*)
						
						
				(* 
					a) Voir feuille annexe
					
					b) Le coût d'une recherche d'un élément dans une list à sauts dont les éléments
						Similaire a une recherche dichotomique dans un arbre binaire de recherche 
						
				*)
									(* Insertion et suppression :*)
									
									
						(*Question 7 :*)
						
					let rec split_list (x : elt) (l : skip_list) =
						match l with
						| Nil -> (Nil, false, Nil)
						| Level (l2, c) ->
							let k = key l
							in
								if x < k then
									let (res_l, res_p, res_r) = split_list x (left l)
									in
										(res_l, res_p, Level (res_r, c))
								else if x > k then
									let (res_l, res_p, res_r) = split_list x (right l)
									in
										(Level (l2, {lvl=c.lvl; l_elt=[(k, res_l)]}), res_p, res_r)
								else
									(left l, true, right l)
					;;
						
						(*Question 8 :*)
						
						
					(*
					let rec insert lvlx elt liste_saut =
					  match liste_saut with
						|Nil -> Level (Nil, {lvl = lvlx; l_elt = [elt,Nil]})
						|Level (skiplist, cell) -> let lvla = level liste_saut
									   in
							if(lvlx > lvla) then
								let (liste_gauche, bool, liste_droite) = split elt liste_saut
								in
									if (bool = true) then
									   liste_saut
									else
									   Level(liste_gauche,{lvl= lvlx; l_elt = [(elt, liste_droite)]})
							else if(lvlx < lvla)
								if((key (right liste_saut)) = elt) then
									liste_saut
								else if ((key (right liste_saut)) < elt) then
									Level((insert lvlx elt (ddown liste_saut)),{lvl = cell.lvl; l_elt = cell.l_elt})
								else
									   Level(skiplist, {lvl = cell.lvl; l_elt = [key (right liste_saut),(insert lvlx elt (right liste_saut))]})    
							else
								if((key right liste_saut) > elt) then
									Level(skiplist, {lvl = cell.lvl; l_elt = [(elt, Nil), (key (right liste_saut), cell.l_elt)]})
								else if ((key right liste_saut) < elt) then
								
							Notre fonction insert ne fonctionne pas , nous avons essayé de nous baser sur la fonction insert des arbres binaires de P.Corbineau 
						*)
						
						(*Question 9 :*)
						
						
						
									(* Mise en place d'un type abstrait et utilisation de celui_ci *) 
									
						(*Question 10 :*)
						
				module type List_Skip =
					sig 
					type elt = int ;;

					type skip_list = 
							| Nil 
							| Level of skip_list * cell 
					and cell = 
						{ lvl : int ; l_elt : ( elt * skip_list) list } 
					;;
					
					val level : skip_list -> int 
					val key : skip_list -> int 
					val right :skip_list -> skip_list 
					val left : skip_list -> skip_list 
					val find_list : skip_list -> elt -> bool
					val split : elt -> skip_list ->(skip_list * bool * skip_list) 
					(*val insert : int -> elt -> skip_list -> skip_list *)
				end
				
				module monImplantation : List_Skip = 
					struct 
						type elt = int ;;

					type skip_list = 
							| Nil 
							| Level of skip_list * cell 
					and cell = 
						{ lvl : int ; l_elt : ( elt * skip_list) list } 
					;;
					
					let level ( l : skip_list ) = 
					match l with 
					| Nil -> failwith "Function level error : list = Nil " ; 
					| Level (m , c) -> c.lvl ;;

					let key ( l : skip_list) = 
						match l with
						| Nil -> failwith "Function key error : list = Nil " ; 
						| Level (m, c) -> let (e ,_) = List.hd c.l_elt in e 
					;;
					
					let right (l : skip_list) =
							match l with
							| Nil -> failwith "Function right error : list = Nil "
							| Level (l2, c) ->
								match c.l_elt with
								| [] -> failwith "Function right error : list.l_elt = [] "
								| e::[] -> let (_, lelt) = e in lelt 
								| e::m -> Level (Nil, {lvl=c.lvl; l_elt=m})
						;;
					

					let left (l : skip_list) = 
						match l with
						| Nil -> failwith "Function left error : list = Nil "; 
						| Level (m, c) -> m
					;;
					
					let rec find_list (l :skip_list) (e: elt) = 
					if l = Nil then 
						false 
					else 
						let k = key l 
						in 
							if e < k then 
								find_list ( left l ) e 
							else if e > k then
								find_list ( right l ) e 
							else 
								true ;;
								
					let rec split_list (x : elt) (l : skip_list) =
						match l with
						| Nil -> (Nil, false, Nil)
						| Level (l2, c) ->
							let k = key l
							in
								if x < k then
									let (res_l, res_p, res_r) = split_list x (left l)
									in
										(res_l, res_p, Level (res_r, c))
								else if x > k then
									let (res_l, res_p, res_r) = split_list x (right l)
									in
										(Level (l2, {lvl=c.lvl; l_elt=[(k, res_l)]}), res_p, res_r)
								else
									(left l, true, right l)
					;;
					end;;
									
					
						(*Question 11 :*)
						
				let rec gen p =
					if Random.float 1.0 >= p then 0 else 1 + (gen p) ;;		
						
						
				let rec gen_lvls p n = 
					if n = 0 then []  
					else 
					List.append (gen_lvls p (n-1)) [gen p] 
					;;
					
						(*     
								Lorsque p est proche de 0 , la fonction gen renvoie presque tout le temps 0 :
								# gen_lvls 0.1 5;;
						        - : int list = [0; 0; 0; 0; 0]
								=> Le skip_list crée sera vide ou presque vide .
								
								Lorsque p est proche de 1 , la fonction gen renvoie des valeurs fortes :
								# gen_lvls 0.99 5;;
								- : int list = [186; 20; 87; 120; 47]
								=> Le skip_list crée aura beaucoup d'éléments.
						*)
								
								
								
								
						(*Question 12 :*)	
						
						
				let rec gen_skp n l p =
					if l = 0 then Nil
					else 
					insert (List.nth (gen_lvls p 1) 1) (Random.int n) (gen_skp n (l-1) p) ;;
					
				
				let rec gen_lists n l p m = 
					if m = 0 then []  
					else 
					List.append (gen_lists n l p (m-1)) [gen_skp n l p] 
					;;	
					
						
						(*Question 13 :*) 
				
					


			
				
				