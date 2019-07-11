%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
%token AND OR
%token LET EQ IN ANDLET
%token FUN RARROW

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET bs=LetBindings SEMISEMI { Decl bs }

LetBindings :
    x=ID EQ e=Expr { [(x, e)] }
  | x=ID EQ e=Expr ANDLET bs=LetBindings { (x, e) :: bs }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=OrExpr { e }
  | e=FunExpr { e }

LetExpr :
    LET bs=LetBindings IN e2=Expr { LetExp (bs, e2) }

OrExpr :
    l=AndExpr OR r=OrExpr { BinOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=LTExpr AND r=AndExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }
