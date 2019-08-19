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
  | p=Pattern EQ e=Expr { [(p, e)] }
  | x=ID ps=Parameters EQ e=Expr { [(PVar x, make_fun_exp e ps)] }
  | p=Pattern EQ e=Expr ANDLET bs=LetBindings { (p, e) :: bs }
  | x=ID ps=Parameters EQ e=Expr ANDLET bs=LetBindings { (PVar x, make_fun_exp e ps) :: bs }

Parameters : (* todo: replace with Pattern *)
    x=ID { [x] }
  | x=ID ps=Parameters { x :: ps }

Expr :
    e=SeqExpr { e }
  | e=LetExpr { e }
  | e=FunExpr { e }

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
    LPAREN PLUS RPAREN { FunExp ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y"))) }
  | LPAREN MULT RPAREN { FunExp ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y"))) }
  | LPAREN LT RPAREN { FunExp ("x", FunExp ("y", BinOp (Lt, Var "x", Var "y"))) }
  | LPAREN AND RPAREN { FunExp ("x", FunExp ("y", BinOp (And, Var "x", Var "y"))) }
  | LPAREN OR RPAREN { FunExp ("x", FunExp ("y", BinOp (Or, Var "x", Var "y"))) }

FunExpr :
    FUN e=FunArgsAndBody { e }

FunArgsAndBody :
    x=ID RARROW e=Expr { FunExp (x, e) }
  | x=ID e=FunArgsAndBody { FunExp (x, e) }

Pattern : (* todo *)
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
    p=APattern { p }
  | p=APattern COLOCOLO ps=ConsPatternSeq { PConsExp (p :: ps) }
ConsPatternSeq :
    p=APattern { [p] }
  | p=APattern COLOCOLO ps=ConsPatternSeq { p :: ps }

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

                         (*
PatternMatching :
    p=Pattern RARROW e=Expr { p }
  | BAR p=Pattern RARROW e=Expr { p }
  | p=Pattern RARROW e=Expr {}
                          *)
