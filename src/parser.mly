%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS DIV MULT LT
%token IF THEN ELSE TRUE FALSE
%token AND OR
%token LET REC EQ IN ANDLET
%token FUN RARROW

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET bs=LetBindings SEMISEMI { Decl bs }
  | LET REC bs=LetBindings SEMISEMI { RecDecl bs }

LetBindings :
    x=ID EQ e=Expr { [(x, e)] }
  | x=ID ps=Parameters EQ e=Expr { [(x, make_fun_exp e ps)] }
  | x=ID EQ e=Expr ANDLET bs=LetBindings { (x, e) :: bs }
  | x=ID ps=Parameters EQ e=Expr ANDLET bs=LetBindings { (x, make_fun_exp e ps) :: bs }

Parameters :
    x=ID { [x] }
  | x=ID ps=Parameters { x :: ps }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=OrExpr { e }
  | e=FunExpr { e }

LetExpr :
    LET bs=LetBindings IN e2=Expr { LetExp (bs, e2) }
  | LET REC bs=LetBindings IN e2=Expr { LetRecExp (bs, e2) }

OrExpr :
    l=AndExpr OR r=OrExpr { BinOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=LTExpr AND r=AndExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=LTExpr { BinOp (Lt, l, r) }
  | l=PExpr EQ r=LTExpr { BinOp (Eq, l, r) }
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
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
  | e=InfixFunExpr { e }

InfixFunExpr :
    LPAREN PLUS RPAREN { FunExp ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y"))) }
  | LPAREN MULT RPAREN { FunExp ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y"))) }
  | LPAREN LT RPAREN { FunExp ("x", FunExp ("y", BinOp (Lt, Var "x", Var "y"))) }
  | LPAREN AND RPAREN { FunExp ("x", FunExp ("y", BinOp (And, Var "x", Var "y"))) }
  | LPAREN OR RPAREN { FunExp ("x", FunExp ("y", BinOp (Or, Var "x", Var "y"))) }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

FunExpr :
    FUN e=FunArgsAndBody { e }

FunArgsAndBody :
    x=ID RARROW e=Expr { FunExp (x, e) }
  | x=ID e=FunArgsAndBody { FunExp (x, e) }
