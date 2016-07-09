(*Definisco i pezzi base del robottino*)
target=Characters["universal"];

(*
GenerateRandomState[target_]:= Module[{Target, stack,table},
	Target=target;
	stack={};
	table={};
	NestWhile[ # &,  Target, Length[Target]<1];
	{stack,table}
	];
*)

(*
stack=GenerateRandomState[target][[1]];   
table=GenerateRandomState[target][[2]];
*)

stack={"l","a","v"};
table={"i","s","c"};



passi=0;

			(* *)
CS[]:=Module[{},
	passi=passi+1;
	If[Length[stack]>0,  Return[stack[[-1]]], Return["NIL"]];
	];
	
TB[]:=Module[{n},

	passi=passi+1;
	If[stack[[1]]!=target[[-1]],  Return["NIL"]];
	n=1;
	While[stack[[n]]==target[[Length[target]+1-n]] (*&& n<Length[stack]*), n++];
	Return[stack[[n-1]]];


	];

NN[]:=Module[{},
	passi=passi+1;
	If[TB[]!="NIL", Return[target[[	Flatten[Position[target,TB[]]][[1]]   -1	]]	], 
		Return["NIL"]];
	];
	
		(* Functions*)
MS[x_]:=Module[{},   (*C'e' il rischio che venga scritto "NIL" sui dati? Secondo me no*)

	x=ReleaseHold[x];
	passi=passi+1;
	If[MemberQ[table,x],
	stack=Append[stack, x];
	table=Delete[table, Position[table,x]];
	Return[x],
	Return["NIL"]];
	];


MT[x_]:=Module[{},
	x=ReleaseHold[x];
	passi=passi+1;
	If[MemberQ[stack,x],
		table=Append[table, stack[[-1]]];		
		stack=Delete[stack, Length[stack] ];
		Return[x],
		Return["NIL"]
		];
	];

DU[exp1_,exp2_]:=Module[{tmp},	
	tmp="NIL";
	While[ ReleaseHold[exp2]==="TRUE",  passi=passi+1;  tmp=ReleaseHold[exp1]]; 
	Return[tmp]; (*WARNING: QUESTO POTREBBE FARE UN CICLO IN PIU*)
	];

NOT[exp_]:=Module[{},
	exp=ReleaseHold[exp];
	passi=passi+1;
	If[exp==="NIL", Return["TRUE"], Return["NIL"]];
	];
	
EQ[exp1_,exp2_]:=Module[{},
	exp1=ReleaseHold[exp1];
	exp2=ReleaseHold[exp2];
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


(*Promemoria:   ricordarsi di creare RandomSensor --- O forse basta aggiungere un case a FunzCas*)

CreateExpression[]:=Module[{i},  (*Forse si puo' mettere Return prima di Which?*)
	i=Random[Integer,{1,8}];                                (*RandomInteger[{1,8}];*)
	Which[
		i<4,	Return[commands[[i]] ],  
		i<6,	Return[{commands[[i]] , CreateExpression[]} ],			(*Posso evitare i lower bounds per come funziona which*)
		i<7,	Return[{commands[[i]] , CreateExpression[]} ],			(*Ridondante ma tenuto per estensibilita'*)
		i<8,	Return[{commands[[i]] , CreateExpression[], CreatePredicate[]}  ],
		i<9,  	Return[{commands[[i]] , CreateExpression[], CreateExpression[]} ]
	];
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
	program={EQ[#1,#2] &, CreateExpression[], CreateExpression[]}
	];

 



es={EQ[#1, #2] & , NN[] & , {MS[#1] & , CS[] & }}  

(*es={eq[#,#2]&,a,{f[#]&,b}}*)

(*es={Plus[#,#2]&,6,{Sqrt[#]&,10}}*)

(*t={DU[#1, #2] & , {MT[#1] & , CS[] & } , {NOT[#] &,   {NOT[#] &, CS[] }*)


EvaluateTree[tree_]:=Module[{tmp1, tmp2}, 
	(*If[Depth[tree]===1, Return[tree]];
	If[Length[tree]===2, 
		tree[[1]][EvaluateTree[tree[[2]]]],
		tree[[1]][EvaluateTree[tree[[2]]],EvaluateTree[tree[[3]]]]
	];*)

	Which[
	      Depth[tree]===3, 	If[passi> 10  (*3*Length[target]^2*) , Return ["NIL"], Print[tree];  Return[tree]], 
	      Length[tree]===2, Print[tree];   Return[ 
				       tree[[1]][  EvaluateTree[tree[[2]]] ]     ]  (* Qui potrebbe stari bene un ToString*)
					  ,
	      Length[tree]===3, Print[tree];	Return[  
				       tree[[1]][  EvaluateTree[tree[[2]]]  ,
						   EvaluateTree[tree[[3]]]    ]   
					 ]
	      ];
			    
			    
];









(*

Nindividui = 100;   (*   numero pari *)
Nbits=20;
pc=0.7;
pm=0.001;


(**************************************************)
PopIniziale := Table[ Random[ Integer],  {Nindividui}, {Nbits}];


(***************** fitness *************************)
fitness[x_] := Apply[ Plus, x]  ;

  
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


SceltaGenitori[intervallo_] := Module[{selezionati,subint},

				      selezionati = Table[
	      r = Random[];
	      subint=Length[ Select[ intervallo, (#<r) &]]+1;
              subint,
 	                                                              {Nindividui}
							             ];
	      selezionati
				      ];


Crossover[pop_,selezionati_] := Module[{newpop, g1, g2, genitori,
					figli, taglio, t11, t12, t21, t22},

  newpop = Table[
		      g1=selezionati[[2 i-1]];
		      g2=selezionati[[2 i]];
		      genitori={pop[[g1]], pop[[g2]]};

		      If[ Random[] > pc,
			  figli = genitori,

			  taglio = Random[Integer, {1,Nbits-1}];
			  t11 = Take[genitori[[1]], {1,taglio}];
			  t12 = Take[genitori[[1]], {taglio+1,Nbits}];
			  t21 = Take[genitori[[2]], {1,taglio}];
			  t22 = Take[genitori[[2]], {taglio+1,Nbits}];
			  figli = { Join[ t11,t22], Join[ t21,t12]    };
			  	  
			  ];

		      figli,
						      
                       {i,1,Nindividui/2}
		      ];

	       newpop = Flatten[ newpop, 1];

	       newpop
				       ];


bitflip[ind_] := Map[ ( r=Random[]; If[r<pm,  1-# , #  ]    )&,     ind];

Mutazione[pop_] :=  Table[ bitflip[  pop[[i]]   ], {i,1,Nindividui}];

GeneraFigli[ pop_, criterio_] := Module[ {voti, intervallo,
					                         genitori, figli},

			      voti=Map[fitness,pop];
			      intervallo=selezione[voti,criterio];
			      genitori=SceltaGenitori[intervallo];
			      figli=Crossover[pop,genitori];
			      figli=Mutazione[figli];

			      figli

			      

			      ];




SingoloRun[ppcc_, ppmm_] := Module[{gencount, popin, voti},

				   pc = ppcc;
				   pm=ppmm;
				   
gencount =1;
popin = PopIniziale;
voti = Map[ fitness, popin];
If[ Max[voti]===Nbits, Return[gencount]];

		     While[ Max[voti] < Nbits,

			    gencount+=1;
			    popin = GeneraFigli[ popin, FitnessProportionate];
			    voti = Map[ fitness, popin];

			    ];

		     gencount
		     
		     ];
*)
