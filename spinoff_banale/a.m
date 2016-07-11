(*Quiet[]*)


(*   Funzioni ausiliarie  *)

RandomChoice[list_List]:=
	Module[{},
	       pos=Random[Integer,{1,Length[list]}];
	       Return[list[[pos]] ];
		];




(*Definisco i pezzi base del robottino*)

target=Characters["universal"];


RandomState[]:= Module[{tar, stack, table},
	tar=target;
	stack={};
	table={};
	While[Length[tar]!=0,
	      pos=Random[Integer,{1,Length[tar]}];
	      st=Random[Integer,{1,2}];
	      If[st===1,
		 stack=Append[ stack,tar[[pos]] ];
		 tar=Delete[tar, pos];];
	      If[st===2,
		 table=Append[ table,tar[[pos]] ];
		 tar=Delete[tar, pos];
	      ];

	];
	Return[{stack, table}];
];


(*
stack=GenerateRandomState[target][[1]];   
table=GenerateRandomState[target][[2]];
*)


(*stackref={"l","a","v"};
tableref={"i","s","c"};*)

stackref={"l","u","a", "r"};  (*Riscrivere il maniera simile a target*)
tableref={"i","v","n", "e","s"};

stack=stackref;
table=tableref;

(*Inizializzo le configurazioni usate per testare il programma*)
stacks={};
tables={};
numconf=100;
For[i=0, i<numconf-2, i++,
    tmp=RandomState[];
    stacks=Append[ stacks, tmp[[1]] ];
    tables=Append[ tables, tmp[[2]] ]
];


stacks=Append[ stacks, {"l","a","s","r","e","v","i","n","u"} ];
tables=Append[ tables, {} ];

stacks=Append[ stacks, {} ];
tables=Append[ tables, {"l","a","s","r","e","v","i","n","u"} ];



passi=0;
maxpassi=20* Length[target]^2;


			(* *)
Cs[]:=Module[{return, n},
	If[passi>maxpassi, Return["X"]];
	passi=passi+1;
	If[Length[stack]>0,  Return[stack[[-1]]], Return["NIL"]];
      ];
	
Tb[]:=Module[{n},
	     If[passi>maxpassi, Return["X"]];
	     passi=passi+1;
	     
	     If[Length[stack]===0,  Return["NIL"]];
	     If[stack[[1]]!=target[[-1]],  Return["NIL"]];
	     If[Reverse[stack]===target, Return["NIL"]];
	     n=2;
	     return=target[[1]];  (*Sono molto triste di dovere usare questa variabile e non potere usare solo l'indice n*)
	     While[n<Length[stack]+1 && stack[[n]]==target[[Length[target]+1-n]] , return=stack[[n]]; n++];
	     Return[stack[[n-1]]];
	];

Nn[]:=Module[{tmp},
     	     If[passi>maxpassi, Return["X"]];    
	 
	     
	     If[Tb[]!="NIL", Return[target[[	Flatten[Position[target,Tb[]]][[1]]   -1	]]	], 
		If[Reverse[stack]===target,
		   Return["NIL"],
		   Return[target[[Length[target]]]]
		];
	     ];

	     passi=passi+1;  (*NOTA: l'incremento di passi va messo alla fine se vengono chiamate altre funzioni*)
      ];
	
		(* Functions*)
Ms[x_]:=Module[{},   (*C'e' il rischio che venga scritto "NIL" sui dati? Secondo me no*)
	If[passi>maxpassi, Return["X"]];
	If[x==="X", Return["X"]];
	passi=passi+1;
	
	If[MemberQ[table,x],
	   stack=Append[stack, x];
	   table=Delete[table, Position[table,x]];
	   Return[x],
	   Return["NIL"]];
	];


Mt[x_]:=Module[{},
	If[passi>maxpassi, Return["X"]];
	If[x==="X", Return["X"]];
	passi=passi+1;
	
	If[MemberQ[stack,x],
		table=Append[table, stack[[-1]]];		
		(*stack=Delete[stack, Length[stack] ];*)
		stack=Drop[stack, -1 ];
		Return[x],
		Return["NIL"]
		];
	];

Du[exp1_,exp2_]:=Module[{tmp},	
	If[passi>maxpassi, Return["X"]]; passi=passi+1;
	If[exp1==="X" || exp2==="X", Return["X"]];
	
	tmp="NIL";
	While[ exp2==="TRUE",  tmp=exp1]; 
	passi=passi+1;
	
	Return[tmp]; (*WARNING: QUESTO POTREBBE FARE UN CICLO IN PIU*)
	             (*UPDATE: invece no*)
	];

SetAttributes[Du, HoldAllComplete];
(*SetAttributes[Mt, HoldAllComplete];*)
(*SetAttributes[Not, HoldAllComplete];*)

NOt[exp_]:=Module[{},   (*ATTENZIONE: "Not" esiste gia' *)
	If[passi>maxpassi, Return["X"]];
	If[exp==="X", Return["X"]];
	passi=passi+1;
	If[exp==="NIL", Return["TRUE"], Return["NIL"]];
	];
	
Eq[exp1_,exp2_]:=Module[{},
	If[passi>maxpassi, Return["X"]];
	If[exp1==="X" || exp2==="X", Return["X"]];
	passi=passi+1;
	If[exp1===exp2, Return["TRUE"], Return["NIL"]];
	];

sensors={CS[]& , TB[] &, NN[]&};
functions={MS[#]&, MT[#] &};
operators1={NOT[#]&};
operators2={DU[#,#2]&, EQ[#,#2] &};

(*sensors={CS,TB,NN};
functions={MS,MT};
operators1={NOT};
operators2={DU,EQ};*)


commands=Flatten[{sensors,functions,operators1,operators2}];
predicates={NOT,EQ};
(******************************************)


RandomIns[tree_, ins_]:=  (*Doppia definizione, in nome della chiarezza del codice*)
	Module[{elem, pos},


	       elem=RandomChoice[Level[tree,-1]];
	       pos=RandomChoice[Position[tree,elem,-1]];	       
	       ReplacePart[tree, ins, pos]

	];






(*Promemoria:   ricordarsi di creare RandomSensor --- O forse basta aggiungere un case a FunzCas*)

CreateExpression[]:=Module[{i},  (*Forse si puo' mettere Return prima di Which?*)
	i=Random[Integer,{1,14}];                                (*RandomInteger[{1,8}];*)
	Which[
		i<4,	Return[commands[[i]] ],  
		i<6,	Return[{commands[[i]] , CreateExpression[]} ],			(*Posso evitare i lower bounds per come funziona which*)
		i<7,	Return[{commands[[i]] , CreateExpression[]} ],			(*Ridondante ma tenuto per estensibilita'*)
		i<8,	Return[{commands[[i]] , CreateExpression[], CreatePredicate[]}  ],
		i<9,  	Return[{commands[[i]] , CreateExpression[], CreateExpression[]} ],
		i<15,    Return[RandomChoice[sensors]]  (*Non il modo piu' raffinato per cambiare le proabilita'*)
	];                                             (*Ma modifica poco il codice*)
];

CreatePredicate[]:=Module[{i},
	i=Random[Integer,{1,2}];
	Which[
		i===1, Return[{predicates[[i]] , CreateExpression[]}  ],
		i===2, Return[{predicates[[i]] , CreateExpression[], CreateExpression[]}  ]
	];

];

GenerateRandomProgram[]:=Module[{},
				nnodes=1;
	
	program={EQ[#1,#2] &, CreateExpression[], CreateExpression[]};

	
	program=TreeToExp[program];
	(*Opzionale: Controllo della lunghezza*)
	(*tramite potatura*)
	
	While[Length[Level[program, -1]]>11,
	      program=RandomIns[program, TreeToExp[RandomChoice[sensors]]  ];		    
	];

	
	Return[program];
	];

 



es={EQ[#1, #2] & , NN[] & , {MS[#1] & , CS[] & }}  

(*es={eq[#,#2]&,a,{f[#]&,b}}*)

(*es={Plus[#,#2]&,6,{Sqrt[#]&,10}}*)
(*t={DU[#1, #2] & , {MT[#1] & , CS[] & } , {NOT[#] &,   {NOT[#] &, CS[]& }}}  *)


sol1={DU[#1, #2] & , {MT[#1] & , CS[] & } , {NOT[#] &,   {NOT[#] &, CS[]& }}};
sol2={DU[#1, #2] & , {MS[#1] & , NN[] & } , {NOT[#] &,   {NOT[#] &, NN[]& }}};

sol={EQ[#1,#2] &, sol1, sol2};


TreeToExp[tree_]:=Module[{tmp}, 
	(*If[Depth[tree]===1, Return[tree]];
	If[Length[tree]===2, 
		tree[[1]][EvaluateTree[tree[[2]]]],
		tree[[1]][EvaluateTree[tree[[2]]],EvaluateTree[tree[[3]]]]
	];*)
			    
	Which[
		Depth[tree]<4, (*3*Length[target]^2*) Return[tree [] ], 
		Length[tree]===2,   Return[
			tree[[1]]  [  TreeToExp[tree[[2]]]   ]   
			  ],
		Length[tree]===3, 	Return[  
			tree[[1]][  TreeToExp[tree[[2]]]  ,
				    TreeToExp[tree[[3]]]    ]   
				        ]
	];
			    
			    
];


EvaluateExp[exp_]:=Module[{tmp},
			  passi=0;
			  exp /. {CS->Cs, TB->Tb, NN->Nn, MS->Ms, MT->Mt, DU->Du, NOT->NOt, EQ->Eq}
			 
		   ];
			 
		   

EvaluateTree[tree_]:=Module[{p},
			    
			    p=TreeToExp[tree];
			    EvaluateExp[p]
		     ];


RunP[exp_, n_]:=Module[{tmps,tmpt},
		       tmps=stack;
		       tmpt=table;
		       stack=stacks[[n]];
		       table=tables[[n]];
		       Print[stack];
		       Print[table];
		       Print["-------------------------Evaluation---------------------------"];
		       EvaluateExp[exp];
		       Print[stack];
		       Print[table];
		       Print[passi, " passi"];

		       stack=tmps;
		       table=tmpt;
		];





Nindividui = 100;   (*   numero pari *)
Lmax=20;  (*Eventuale lunghezza massima del programma*) 
pc=0.7;
pm=0.005;


(**************************************************)
PopIniziale = Table[ GenerateRandomProgram[],  {Nindividui}];
po=PopIniziale;

CountEqual[stack_]:=Module[{i},
			   i=0; passi=0;
			   If[Length[stack]===0, Return[0]];
			   If[Reverse[stack]===target, Return[15]];
			   If[stack[[1]] != target[[Length[target]]], Return[0]];
			   (*Qui posso segnalare i programmi che eccedono il numero di passi massimo*)
			   Return[Position[stack, Tb[]][[1]][[1]]];
		    ];



(***************** fitness *************************)
(*fitness[x_] :=
	Module[{fit},
	       fit=0;
	       For[i=1, i<numconf+1, i++,
		   stack=stacks[[i]];
		   table=tables[[i]];
		   EvaluateTree[x];
		   fit=fit+CountEqual[stack];
	       ];
	       fit
	];

*)




fitness[x_] :=
	Module[{fit, tmp},
	       fit=0;
	       For[i=1, i<numconf+1, i++,
		   stack=stacks[[i]];
		   table=tables[[i]];
		  
		   tmp=EvaluateExp[x];
		   If[tmp==="X", Print["fermato alla ", i, " con passi ", passi]; Return[0]];
		   
		   If[Reverse[stack]===target, Print[i, " ", stack]; fit++];
	       ];
	       fit
	];

fitnesspars[x_] :=
	Module[{fit, tmp, passitot=0},
	       fit=0;
	       For[i=1, i<numconf+1, i++,
		   stack=stacks[[i]];
		   table=tables[[i]];
		  
		   tmp=EvaluateExp[x];
		   If[tmp==="X", Print["fermato alla ", i, " con passi ", passi]; Return[0]];
		   
		   If[Reverse[stack]===target, Print[i, " ", stack]; passitot+=passi; fit++];
	       ];

	       passitot=passitot/(fit * maxpassi);
	       fit + (20/passitot)
	];



(*
Test:=Module[{t,i},
	     t=0;
	     i=0;
	     stack=stacks[[1]];
	     table=tables[[1]];
	     oldstack=stack;
	     oldtable=table;
	     While[stack===oldstack,
		   stack=stacks[[1]];
		   table=tables[[1]];
		   p=GenerateRandomProgram[];
		   EvaluateTree[p];
		   i++;
		   Print[i];
	     ];
	     Print[oldstack];
	     Print[stack];
	     p
      ];*)
progs={};

Test:=Module[{t,i},
	     t=0;
	     i=1;
	     stack=stacks[[1]];
	     table=tables[[1]];
	     oldstack=stack;
	     oldtable=table;
	     While[t<2,
		   stack=stacks[[1]];
		   table=tables[[1]];
		   p=GenerateRandomProgram[];
		   t=fitness[p];
		   Print[i];
		   progs=Append[progs,p];
		   i++;
	     ];
	     p
      ];




  
(***************************************************)
selezione[voti_List, criterio_] := Module[ {larghezza,intervallo},

	  Which[ criterio === FitnessProportionate,

		     larghezze=voti/Apply[Plus, voti];
 		     intervallo= Table[ Sum[larghezze[[i]], {i,1,j}],
                                                 {j,1,Nindividui}
					],

				       
					    True,
					    Print["criterio non implementato"];
					    Abort[];
					    
		        		    ];

      intervallo
					   
					   ];


SceltaGenitori[intervallo_] :=
Module[{selezionati,subint},
	selezionati = Table[
	      r = Random[];
	      subint=Length[ Select[ intervallo, (#<r) &]]+1;
              subint,
 	                                                              {Nindividui-2}
							             ];
	      selezionati
				      ];

(*Anche:  Random level, random element*)
(*
RandomCut[tree_  /; Depth[tree]<5, ex_, n_]:=
	Module[ { branch},
		branch=ex;
		If [Random[Integer, {1,n}]===n,
		    branch=tree;
		];
		
		Return[{branch,n+1}];
	];
 *)

RandomCut[tree_ ]:=
	Module[{},
	    
	       RandomChoice[Level[tree,-1]]
				 
	];




(*
RandomIns[tree_, ins_]:=  (*Caso particolare, si già che l'elemento "radice" e' EQ[,]*)
	Module[{elem, pos},


	       elem=RandomChoice[Level[tree,-1]];
	       pos=RandomChoice[Position[tree,elem,-1]];
		       
	       ReplacePart[tree, ins, pos]

	];*)

		   
Crossover[pop_,selezionati_] := Module[{newpop, g1, g2, genitori,
					figli, taglio, t11, t12, t21, t22},

  newpop = Table[
		      g1=selezionati[[2 i-1]];
		      g2=selezionati[[2 i]];
		      genitori={pop[[g1]], pop[[g2]]};

		      If[ Random[] > pc,
			  figli = genitori,

			  cut1=RandomCut[genitori[[1]]];
			  cut2=RandomCut[genitori[[2]]];

			  figlio1=RandomIns[genitori[[1]], cut2];
			  figlio2=RandomIns[genitori[[2]], cut1];
			  
			  figli = { figlio1, figlio2    };
			  	  
			  ];

		      figli,
						      
                      {i,1,(Nindividui-2)/2}
		      ];

	       newpop = Flatten[ newpop, 1];

	       newpop
				];


bitflip[ind_] :=
	Module[{tmp},
	       tmp=TreeToExp[CreateExpression[]];
	       r=Random[];
	       If[r<pm,
		  Return[RandomIns[ind, tmp ]],
		  Return[ind]];
	];

	       
Mutazione[pop_] :=  Table[ bitflip[  pop[[i]]   ], {i,1,Nindividui}];

Fittest[pop_, voti_]:=
	Module[{n,m, l},
	       m=Max[voti];
	       l=Position[voti,Max[voti]];
	       (*l'indice fortunato e' della forma
		{ind}, ma sembra che questo non causi problemi, speriamo in bene*)	       
	       Return[pop[[RandomChoice[l][[1]] ]]  ];
	       
	];



GeneraFigli[ pop_, criterio_, voti_] := Module[ {intervallo,
					                         genitori, figli, tmp, pop2},

						(*voti=Map[fitness,pop];*)
			      intervallo=selezione[voti,criterio];
			      genitori=SceltaGenitori[intervallo];
			      figli=Crossover[pop,genitori];

			      Print["ciao1"];
			      tmp=Fittest[pop,voti];
			      figli=Append[figli, tmp];
			      pop2=DeleteCases[pop, tmp];
			      voti=Delete[voti, RandomChoice[Position[voti, Max[voti]]]];			      
			      figli=Append[figli, Fittest[pop2,voti]];

			      Print["ciao2"];
			      
			      figli=Mutazione[figli];

			      figli

			      

			      ];


tmpgen={};

SingoloRun[file_] :=
	Module[{gencount, popin, voti},

(*	       pc = ppcc;
	       pm=ppmm;*)
	       
	       gencount =1;
	       popin = PopIniziale;
	       voti = Map[ fitness, popin];
	       If[ Max[voti]===100, Return[gencount]];
	       
	       While[ Max[voti] < 100 && gencount<100 ,
		      
		      gencount+=1;
		      popin = GeneraFigli[ popin, FitnessProportionate, voti];
		      Print["figli generati"];
		      voti = Map[ fitness, popin];
		      Print["--------------Generazione: ",gencount,"---Max voto: ",Max[voti]];
(*		      fin=popin[[Position[voti,Max[voti]]]];
		      Print[fin];*)
		      
		      tmpgen=popin;
		      
	       ];

	       Write[file, "Ultima generazione: ", gencount];	      	       
	       Write[file, "Posizione delle soluzioni: ", Position[voti, Max[voti]]];
	       Write[file, "--------------------Soluzione-------------------[con fitness=",fitness[Fittest[popin]],"]"];  (*Computazione inutile ma in realta' utile*)
	       Write[file, Fittest[popin]];
	       Write[file, "----------Segue per completezza l'intera generazione ultima: --------------", popin];
	       
	       (*Return[{popin, voti, gencount, Fittest[popin,voti]}];*)
	       
	];



(*stack=sback;
table=tback;*)

c=FitnessProportionate;
