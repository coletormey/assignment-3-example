package parser;

import java.util.*;
import lexer.*;
import ast.*;

/**
 * The Parser class performs recursive-descent parsing; as a by-product it will
 * build the <b>Abstract Syntax Tree</b> representation for the source
 * program
 * Following is the Grammar we are using:
 * <pre>
 *  PROGRAM -> 'program' BLOCK             ==> program
 *
 *  BLOCK -> '{' D* S* '}'                 ==> block
 *
 *  D -> TYPE NAME                         ==> decl
 *    -> TYPE NAME FUNHEAD BLOCK           ==> functionDecl
 *
 *  TYPE  ->  'int'
 *        ->  'boolean'
 *
 *  FUNHEAD  -> '(' (D list ',')? ')'      ==> formals
 *
 *  S  -> 'if' E 'then' BLOCK 'else' BLOCK ==> if
 *     -> 'while' E BLOCK                  ==> while
 *     -> 'return' E                       ==> return
 *     -> BLOCK
 *     -> NAME '=' E                       ==> assign
 *
 *  E  -> SE
 *     -> SE '==' SE                       ==> =
 *     -> SE '!=' SE                       ==> !=
 *     -> SE '<'  SE                       ==> <
 *     -> SE '<=' SE                       ==> <=
 *
 *  SE ->  T
 *     ->  SE '+' T                        ==> +
 *     ->  SE '-' T                        ==> -
 *     ->  SE '|' T                        ==> or
 *
 *  T  -> F
 *     -> T '*' F                          ==> *
 *     -> T '/' F                          ==> /
 *     -> T '&' F                          ==> and
 *
 *  F  -> '(' E ')'
 *     -> NAME
 *     -> <int>
 *     -> NAME '(' (E list ',')? ')'       ==> call<br>
 *
 *  NAME  -> <id>
 */
public class Parser {

  private Token currentToken;
  private Lexer lex;
  private EnumSet<Tokens> relationalOps =
    EnumSet.of( Tokens.Equal, Tokens.NotEqual, Tokens.Less, Tokens.LessEqual );
  private EnumSet<Tokens> addingOps =
    EnumSet.of( Tokens.Plus, Tokens.Minus, Tokens.Or );
  private EnumSet<Tokens> multiplyingOps =
    EnumSet.of( Tokens.Multiply, Tokens.Divide, Tokens.And );

  /**
   * Construct a new Parser;
   *
   * @param sourceProgram - source file name
   * @exception Exception - thrown for any problems at startup (e.g. I/O)
   */
  public Parser( String sourceProgram ) throws Exception {
    try {
      lex = new Lexer( sourceProgram );
      scan();
    } catch( Exception e ) {
      System.out.println( "********exception*******" + e.toString() );
      throw e;
    };
  }

  public Lexer getLex() {
    return lex;
  }

  /**
   * Execute the parse command
   *
   * @return the AST for the source program
   * @exception Exception - pass on any type of exception raised
   */
  public AST execute() throws Exception {
    try {
      return rProgram();
    } catch( SyntaxError e ) {
      e.print();
      throw e;
    }
  }

  /**
   * Program -> 'program' block ==> program
   *
   * @return the program tree
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rProgram() throws SyntaxError {
    // note that rProgram actually returns a ProgramTree; we use the
    // principle of substitutability to indicate it returns an AST
    AST programTree = new ProgramTree();

    expect( Tokens.Program );
    programTree.addKid( rBlock() );

    return programTree;
  }

  /**
   * block -> '{' d* s* '}' ==> block
   *
   * @return block tree
   * @exception SyntaxError - thrown for any syntax error e.g. an expected
   * left brace isn't found
   */
  public AST rBlock() throws SyntaxError {
    expect( Tokens.LeftBrace );
    AST blockTree = new BlockTree();

    // get decls
    while( startingDecl() ) {
      blockTree.addKid( rDecl() );
    }

    // get statements
    while( startingStatement() ) {
      blockTree.addKid( rStatement() );
    }

    expect( Tokens.RightBrace );
    return blockTree;
  }

  boolean startingDecl() {
    return isNextTok( Tokens.Int ) || isNextTok( Tokens.BOOLean );
  }

  boolean startingStatement() {
    return isNextTok( Tokens.If ) ||
      isNextTok( Tokens.While ) ||
      isNextTok( Tokens.Return ) ||
      isNextTok( Tokens.LeftBrace ) ||
      isNextTok( Tokens.Identifier );
  }

  /**
   * d -> type name ==> decl -> type name funcHead block ==> functionDecl
   *
   * @return either the decl tree or the functionDecl tree
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rDecl() throws SyntaxError {
    AST type, identifier;
    type = rType();
    identifier = rName();

    // function
    if( isNextTok( Tokens.LeftParen )) {
      type = ( new FunctionDeclTree() ).addKid( type ).addKid( identifier );
      type.addKid( rFunHead() );
      type.addKid( rBlock() );

      return type;
    }

    type = ( new DeclTree() ).addKid( type ).addKid( identifier );

    return type;
  }

  /**
   * type -> 'int' type -> 'bool'
   *
   * @return either the intType or boolType tree
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rType() throws SyntaxError {
    AST type;

    if( isNextTok( Tokens.Int )) {
      type = new IntTypeTree();
      scan();
    } else {
      expect( Tokens.BOOLean );
      type = new BoolTypeTree();
    }

    return type;
  }

  /**
   * funHead -> '(' (decl list ',')? ')' ==> formals note a funhead is a list
   * of zero or more decl's separated by commas, all in parens
   *
   * @return the formals tree describing this list of formals
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rFunHead() throws SyntaxError {
    AST formals = new FormalsTree();

    expect( Tokens.LeftParen );

    if( !isNextTok( Tokens.RightParen )) {
      do {
        formals.addKid( rDecl() );
        if( isNextTok( Tokens.Comma )) {
          scan();
        } else {
          break;
        }
      } while( true );
    }

    expect( Tokens.RightParen );

    return formals;
  }

  /**
   * S -> 'if' e 'then' block 'else' block ==> if -> 'while' e block ==> while
   * -> 'return' e ==> return -> block -> name '=' e ==> assign
   *
   * @return the tree corresponding to the statement found
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rStatement() throws SyntaxError {
    AST statement;

    if( isNextTok( Tokens.If )) {
      scan();

      statement = new IfTree();
      statement.addKid( rExpr() );
      expect(Tokens.Then);
      statement.addKid( rBlock() );
      expect(Tokens.Else);
      statement.addKid( rBlock() );

      return statement;
    }
    if( isNextTok( Tokens.While )) {
      scan();

      statement = new WhileTree();
      statement.addKid( rExpr() );
      statement.addKid( rBlock() );

      return statement;
    }
    if( isNextTok( Tokens.Return )) {
      scan();

      statement = new ReturnTree();
      statement.addKid( rExpr() );

      return statement;
    }
    if( isNextTok( Tokens.LeftBrace )) {
      return rBlock();
    }

    statement = rName();
    statement = ( new AssignTree() ).addKid( statement );

    expect( Tokens.Assign );

    statement.addKid( rExpr() );

    return statement;
  }

  /**
   * e -> se -> se '==' se ==> = -> se '!=' se ==> != -> se '<' se ==> < -> se
   * '<=' se ==> <= </pre> @return the tree corresponding to the expression
   *
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rExpr() throws SyntaxError {
    AST relationTree, kid = rSimpleExpr();

    relationTree = getRelationTree();

    if( relationTree == null ) {
      return kid;
    }

    relationTree.addKid( kid );
    relationTree.addKid( rSimpleExpr() );

    return relationTree;
  }

  /**
   * se -> t -> se '+' t ==> + -> se '-' t ==> - -> se '|' t ==> or This rule
   * indicates we should pick up as many <i>t</i>'s as possible; the
   * <i>t</i>'s will be left associative
   *
   * @return the tree corresponding to the adding expression
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rSimpleExpr() throws SyntaxError {
    AST simpleExpression, kid = rTerm();

    while(( simpleExpression = getAddOperTree() ) != null ) {
      simpleExpression.addKid( kid );
      simpleExpression.addKid( rTerm() );
      kid = simpleExpression;
    }

    return kid;
  }

  /**
   * t -> f -> t '*' f ==> * -> t '/' f ==> / -> t '&' f ==> and This rule
   * indicates we should pick up as many <i>f</i>'s as possible; the
   * <i>f</i>'s will be left associative
   *
   * @return the tree corresponding to the multiplying expression
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rTerm() throws SyntaxError {
    AST term, kid = rFactor();

    while(( term = getMultOperTree() ) != null ) {
      term.addKid( kid );
      term.addKid( rFactor() );
      kid = term;
    }

    return kid;
  }

  /**
   * f -> '(' e ')' -> name -> <int>
   *   -> name '(' (e list ',')? ')' ==> call
   *
   * @return the tree corresponding to the factor expression
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rFactor() throws SyntaxError {
    AST expression;

    // -> (e)
    if( isNextTok( Tokens.LeftParen )) {
      scan();

      expression = rExpr();
      expect( Tokens.RightParen );

      return expression;
    }

    //  -> <int>
    if( isNextTok( Tokens.INTeger )) {
      expression = new IntTree( currentToken );
      scan();

      return expression;
    }

    expression = rName();

    //  -> name
    if( ! isNextTok( Tokens.LeftParen )) {
      return expression;
    }

    // -> name '(' (e list ',')? ) ==> call
    scan();

    expression = ( new CallTree() ).addKid( expression );
    if( ! isNextTok( Tokens.RightParen )) {
      do {
        expression.addKid( rExpr() );
        if( isNextTok( Tokens.Comma )) {
          scan();
        } else {
          break;
        }
      } while( true );
    }

    expect( Tokens.RightParen );

    return expression;
  }

  /**
   * name -> <id>
   *
   * @return the id tree
   * @exception SyntaxError - thrown for any syntax error
   */
  public AST rName() throws SyntaxError {
    AST name;

    if( isNextTok( Tokens.Identifier )) {
      name = new IdTree( currentToken );
      scan();

      return name;
    }

    throw new SyntaxError( currentToken, Tokens.Identifier );
  }

  // build tree with current token's relation
  AST getRelationTree() {
    Tokens kind = currentToken.getKind();

    if( relationalOps.contains( kind )) {
      AST relOpTree = new RelOpTree( currentToken );
      scan();

      return relOpTree;
    } else {
      return null;
    }
  }

  private AST getAddOperTree() {
    Tokens kind = currentToken.getKind();

    if( addingOps.contains( kind )) {
      AST addOpTree = new AddOpTree( currentToken );
      scan();

      return addOpTree;
    } else {
      return null;
    }
  }

  private AST getMultOperTree() {
    Tokens kind = currentToken.getKind();

    if( multiplyingOps.contains( kind )) {
      AST multOpTree = new MultOpTree( currentToken );
      scan();

      return multOpTree;
    } else {
      return null;
    }
  }

  private boolean isNextTok( Tokens kind ) {
    if(( currentToken == null ) || ( currentToken.getKind() != kind )) {
      return false;
    }

    return true;
  }

  private void expect( Tokens kind ) throws SyntaxError {
    if( isNextTok( kind )) {
      scan();
      return;
    }

    throw new SyntaxError( currentToken, kind );
  }

  private void scan() {
    currentToken = lex.nextToken();

    if( currentToken != null ) {
      // debug printout
      currentToken.print();
    }

    return;
  }
}

class SyntaxError extends Exception {

  private static final long serialVersionUID = 1L;
  private Token tokenFound;
  private Tokens kindExpected;

  /**
   * record the syntax error just encountered
   *
   * @param tokenFound is the token just found by the parser
   * @param kindExpected is the token we expected to find based on the current
   * context
   */
  public SyntaxError( Token tokenFound, Tokens kindExpected ) {
    this.tokenFound = tokenFound;
    this.kindExpected = kindExpected;
  }

  void print() {
    System.out.println( "Expected: " + kindExpected );

    return;
  }
}
