%{
open Syntax
%}

%token LPAREN RPAREN SEMI SEMISEMI
%token PLUS MINUS DIV MULT LT
%token IF THEN ELSE TRUE FALSE
%token AND OR
%token LET REC EQ IN ANDLET
%token FUN RARROW
%token EXCLA COLONEQ
%token COMMA
%token LBOXBRA RBOXBRA COLOCOLO APPEND
%token BAR
%token MATCH WITH
%token TYPE OF

%token <int> INTV
%token <Syntax.id> ID
%token <Syntax.upperId> UPPERCASE

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET bs=LetBindings SEMISEMI { Decl bs }
  | LET REC bs=LetBindings SEMISEMI { RecDecl bs }
  | defs=TypeDefinition SEMISEMI { TyDef defs }

LetBindings :
  | p=Pattern EQ e=Expr { [(p, e)] }
  | x=ID p=Pattern ps=list(Pattern) EQ e=Expr { [(PVar x, make_fun_exp e (p :: ps))] }
  | p=Pattern EQ e=Expr ANDLET bs=LetBindings { (p, e) :: bs }
  | x=ID p=Pattern ps=list(Pattern) EQ e=Expr ANDLET bs=LetBindings { (PVar x, make_fun_exp e (p :: ps)) :: bs }

Expr :
    e=SeqExpr { e }
  | e=LetExpr { e }
  | e=FunExpr { e }
  | e=MatchExpr { e }

LetExpr :
    LET bs=LetBindings IN e2=Expr { LetExp (bs, e2) }
  | LET REC bs=LetBindings IN e2=Expr { LetRecExp (bs, e2) }

SeqExpr :
    l=IfExpr SEMI r=SeqExpr { UnitSeqExp (l, r) }
  | e=IfExpr { e }

IfExpr :
    IF c=Expr THEN t=AssignExpr ELSE e=AssignExpr { IfExp (c, t, e) }
  | e=AssignExpr { e }

AssignExpr :
    l=TupleExpr COLONEQ r=AssignExpr { BinOp (Assign, l, r) }
  | e=TupleExpr { e }

TupleExpr :
    e=OrExpr { e }
  | l=OrExpr COMMA r=TupleExprR { match r with
                                    TupleExp exps -> TupleExp (l :: exps)
                                  | _ -> TupleExp [l; r] }

TupleExprR :
    e=OrExpr { TupleExp [e] }
  | l=OrExpr COMMA r=TupleExprR { match r with
                                    TupleExp exps -> TupleExp (l :: exps)
                                  | _ -> TupleExp [l; r] }

OrExpr :
    l=AndExpr OR r=OrExpr { BinOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=LTExpr AND r=AndExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=LTExpr LT r=AppendExpr { BinOp (Lt, l, r) }
  | l=LTExpr EQ r=AppendExpr { BinOp (Eq, l, r) }
  | e=AppendExpr { e }

AppendExpr :
    l=ConsExpr APPEND r=AppendExpr { BinOp (Append, l, r) }
  | e=ConsExpr { e }

ConsExpr :
    l=PExpr COLOCOLO r=ConsExpr { BinOp (Cons, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | l=PExpr MINUS r=MExpr { BinOp (Minus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | l=MExpr DIV r=AppExpr { BinOp (Div, l, r) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=DerefExpr { AppExp (e1, e2) }
  | constr=UpperCase e2=DerefExpr { ConstrExp (constr, e2) }
  | e=DerefExpr { e }

DerefExpr :
    EXCLA e=AExpr { DerefExp e }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | LPAREN RPAREN { UnitLit }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
  | e=InfixFunExpr { e }
  | e=ListExpr { e }

ListExpr :
    LBOXBRA RBOXBRA { ListExp [] }
  | LBOXBRA e=ListSeqExpr RBOXBRA { ListExp e }

ListSeqExpr : (* not include SeqExpr directly *)
    e=LetExpr { [e] }
  | e=FunExpr { [e] }
  | e=IfExpr { [e] }
  | l=LetExpr SEMI r=ListSeqExpr { l :: r }
  | l=FunExpr SEMI r=ListSeqExpr { l :: r }
  | l=IfExpr SEMI r=ListSeqExpr { l :: r }

InfixFunExpr :
    LPAREN PLUS RPAREN { FunExp (PVar "x", FunExp (PVar "y", BinOp (Plus, Var "x", Var "y"))) }
  | LPAREN MULT RPAREN { FunExp (PVar "x", FunExp (PVar "y", BinOp (Mult, Var "x", Var "y"))) }
  | LPAREN LT RPAREN { FunExp (PVar "x", FunExp (PVar "y", BinOp (Lt, Var "x", Var "y"))) }
  | LPAREN AND RPAREN { FunExp (PVar "x", FunExp (PVar "y", BinOp (And, Var "x", Var "y"))) }
  | LPAREN OR RPAREN { FunExp (PVar "x", FunExp (PVar "y", BinOp (Or, Var "x", Var "y"))) }

FunExpr :
    FUN e=FunArgsAndBody { e }

FunArgsAndBody :
    p=Pattern RARROW e=Expr { FunExp (p, e) }
  | p=Pattern e=FunArgsAndBody { FunExp (p, e) }


(* Pattern *)
Pattern :
    p=TuplePattern { p }
  | p=TuplePattern BAR ps=CombinedPattern { PCombineExp (p :: ps) }
CombinedPattern :
    p=TuplePattern { [p] }
  | p=TuplePattern BAR ps=CombinedPattern { p :: ps }

TuplePattern :
    p=ConsPattern { p }
  | p=ConsPattern COMMA ps=TuplePatternSeq { PTupleExp (p :: ps) }
TuplePatternSeq :
    p=ConsPattern { [p] }
  | p=ConsPattern COMMA ps=TuplePatternSeq { p :: ps }

ConsPattern :
    p=ConstrPattern { p }
  | p=ConstrPattern COLOCOLO ps=ConsPatternSeq { PConsExp (p :: ps) }
ConsPatternSeq :
    p=ConstrPattern { [p] }
  | p=ConstrPattern COLOCOLO ps=ConsPatternSeq { p :: ps }

ConstrPattern :
    constr=UpperCase p=APattern { PConstrExp (constr, p) }
  | p=APattern { p }

APattern :
    i=ID { PVar i }
  | TRUE { PBLit true }
  | FALSE { PBLit false }
  | i=INTV { PILit i }
  | LPAREN RPAREN { PUnitLit }
  | LPAREN p=Pattern RPAREN { p }
  | p=ListPattern { p }

ListPattern :
    LBOXBRA RBOXBRA { PListExp [] }
  | LBOXBRA ps=ListPatternSeq RBOXBRA { PListExp ps }
ListPatternSeq :
    p=APattern { [p] }
  | p=APattern SEMI ps=ListPatternSeq { p :: ps }

MatchExpr :
    MATCH e=Expr WITH m=PatternMatching { MatchExp (e, m) }

PatternMatching :
    p=Pattern RARROW e=Expr { [p, e] }
  | BAR p=Pattern RARROW e=Expr { [p, e] }
  | p=Pattern RARROW e=Expr ms=PatternMatchingSeq { (p, e) :: ms }
  | BAR p=Pattern RARROW e=Expr ms=PatternMatchingSeq { (p, e) :: ms }

PatternMatchingSeq :
    BAR p=Pattern RARROW e=Expr { [p, e] }
  | BAR p=Pattern RARROW e=Expr ms=PatternMatchingSeq { (p, e) :: ms }


(* Type expressions and type definitions *)
TypeExpr :
    e=FunTypeExpr { e }

FunTypeExpr :
    l=FunTypeExpr RARROW r=TupleTypeExpr { TEFun (l, r) }
  | e=TupleTypeExpr { e }

TupleTypeExpr :
    e=ATypeExpr MULT es=TupleTypeExprSeq { TETuple (e :: es) }
  | e=ATypeExpr { e }
TupleTypeExprSeq :
    e=ATypeExpr { [e] }
  | e=ATypeExpr MULT es=TupleTypeExprSeq { e :: es }

ATypeExpr :
    LPAREN e=TypeExpr RPAREN { e }
  | id=ID { TEVar id }

TypeDefinition :
    TYPE id=ID EQ decl=ConstrDecl { [id, decl] }
  | TYPE id=ID EQ decl=ConstrDecl defs=AndTypeDefinition { (id, decl) :: defs }
AndTypeDefinition :
    AND id=ID EQ decl=ConstrDecl { [id, decl] }
  | AND id=ID EQ decl=ConstrDecl defs=AndTypeDefinition { (id, decl) :: defs }

ConstrDecl :
    id=UpperCase OF e=TypeExpr { [id, e] }
  | BAR id=UpperCase OF e=TypeExpr { [id, e] }
  | id=UpperCase OF e=TypeExpr decls=ConstrDeclSeq { (id, e) :: decls }
  | BAR id=UpperCase OF e=TypeExpr decls=ConstrDeclSeq { (id, e) :: decls }
ConstrDeclSeq :
    BAR id=UpperCase OF e=TypeExpr { [id, e] }
  | BAR id=UpperCase OF e=TypeExpr decls=ConstrDeclSeq { (id, e) :: decls }

UpperCase :
    id=UPPERCASE { match id with UpperId id -> id }
