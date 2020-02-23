import ply.yacc as yacc
import sys
import os
from lexer import Lex as mylex
from lexer import *

tokens=mylex.tokens

# production from 15

def p_Primary(p):
	'''Primary : PrimaryNoNewArray
			| ArrayCreationExpression'''

def p_PrimaryNoNewArray(p):
	'''PrimaryNoNewArray : Literal
			| ClassLiteral
			| THIS
			| TypeName DOT THIS
			| LEFT_PAREN Expression RIGHT_PAREN
			| ClassInstanceCreationExpression
			| FieldAccess
			| ArrayAccess
			| MethodInvocation
			| MethodReference'''

def p_ClassLiteral(p):
	'''ClassLiteral : TypeName SquareBracket DOT CLASS
			| NumericType SquareBracket DOT CLASS
			| boolean SquareBracket DOT CLASS
			| void DOT CLASS'''

def p_SquareBracket(p):
	'''SquareBracket : LEFT_SQUARE RIGHT_SQUARE SquareBracket
			| epsilon'''

def p_ClassInstanceCreationExpression(p):
	'''ClassInstanceCreationExpression : UnqualifiedClassInstanceCreationExpression
			| ExpressionName DOT UnqualifiedClassInstanceCreationExpression
			| Primary DOT UnqualifiedClassInstanceCreationExpression'''

def p_UnqualifiedClassInstanceCreationExpression(p):
	'''UnqualifiedClassInstanceCreationExpression : NEW TypeArgumentsZeroOne ClassOrInterfaceTypeToInstantiate LEFT_PAREN ArgumentListZeroOne RIGHT_PAREN ClassBodyZeroOne'''

def p_TypeArgumentsZeroOne(p):
	'''TypeArgumentsZeroOne : TypeArguments
			| epsilon'''

def p_ArgumentListZeroOne(p):
	'''ArgumentListZeroOne : ArgumentList
			| epsilon'''

def p_ClassBodyZeroOne(p):
	'''ClassBodyZeroOne : ClassBody
			|  epsilon'''

def p_ClassOrInterfaceTypeToInstantiate(p):
	'''ClassOrInterfaceTypeToInstantiate : AnnotationMany Identifier AnnotationIdentifierMany TypeArgumentsOrDiamondZeroOne'''

def p_AnnotationMany(p):
	'''AnnotationMany : Annotation
			| epsilon'''

def p_AnnotationIdentifierMany(p):
	'''AnnotationIdentifierMany : DOT AnnotationMany Identifier AnnotationIdentifierMany
			| epsilon'''

def p_TypeArgumentsOrDiamondZeroOne(p):
	'''TypeArgumentsOrDiamondZeroOne : TypeArgumentsOrDiamond
			| epsilon'''

def p_TypeArgumentsOrDiamond(p):
	'''TypeArgumentsOrDiamond : TypeArguments
			| DIAMOND'''

def p_FieldAccess(p):
	'''FieldAccess : Primary DOT Identifier
			| SUPER DOT Identifier
			| TypeName DOT SUPER DOT Identifier'''

def p_ArrayAccess(p):
	'''ArrayAccess : ExpressionName LEFT_SQUARE Expression RIGHT_SQUARE
			| PrimaryNoNewArray LEFT_SQUARE Expression RIGHT_SQUARE'''

def p_MethodInvocation(p):
	'''MethodInvocation : MethodName LEFT_PAREN ArgumentListZeroOne RIGHT_PAREN
			| TypeName DOT TypeArgumentsZeroOne Identifier LEFT_PAREN ArgumentListZeroOne RIGHT_PAREN
			| ExpressionName DOT TypeArgumentsZeroOne Identifier LEFT_PAREN ArgumentListZeroOne RIGHT_PAREN
			| Primary DOT TypeArgumentsZeroOne Identifier LEFT_PAREN ArgumentListZeroOne RIGHT_PAREN
			| SUPER DOT TypeArgumentsZeroOne Identifier LEFT_PAREN ArgumentListZeroOne RIGHT_PAREN
			| TypeName DOT SUPER DOT TypeArgumentsZeroOne Identifier LEFT_PAREN ArgumentListZeroOne RIGHT_PAREN'''

def p_ArgumentList(p):
	'''ArgumentList : Expression COMMAExpressionMany'''

def p_COMMAExpressionMany(p):
	'''COMMAExpressionMany : COMMA Expression COMMAExpressionMany
			| epsilon'''

def p_MethodReference(p):
	'''MethodReference : ExpressionName DOUBLECOLON TypeArgumentsZeroOne Identifier
			| ReferenceType DOUBLECOLON TypeArgumentsZeroOne Identifier
			| Primary DOUBLECOLON TypeArgumentsZeroOne Identifier
			| SUPER DOUBLECOLON TypeArgumentsZeroOne Identifier
			| TypeName DOT SUPER DOUBLECOLON TypeArgumentsZeroOne Identifier
			| ClassType DOUBLECOLON TypeArgumentsZeroOne NEW
			| ArrayType DOUBLECOLON NEW'''

def p_ArrayCreationExpression(p):
	'''ArrayCreationExpression : NEW PrimitiveType DimExprs DimsZeroOne
			| NEW ClassOrInterfaceType DimExprs DimsZeroOne
			| NEW PrimitiveType Dims ArrayInitializer
			| NEW ClassOrInterfaceType Dims ArrayInitializer'''

def p_DimsZeroOne(p):
	'''DimsZeroOne : Dims
			| epsilon'''

def p_DimExprs(p):
	'''DimExprs : DimExpr DimExprMany'''

def p_DimExprMany(p):
	'''DimExprMany : DimExpr DimExprMany
			| epsilon'''

def p_DimExpr(p):
	'''DimExpr : AnnotationMany LEFT_PAREN Expression RIGHT_PAREN'''

def p_Expression(p):
	'''Expression : LambdaExpression
			| AssignmentExpression'''

def p_LambdaExpression(p):
	'''LambdaExpression : LambdaParameters LEFTARROW LambdaBody'''

def p_LambdaParameters(p):
	'''LambdaParameters : Identifier
			| LEFT_PAREN FormalParameterListZeroOne RIGHT_PAREN
			| LEFT_PAREN InferredFormalParameterList RIGHT_PAREN'''

def p_FormalParameterListZeroOne(p):
	'''FormalParameterListZeroOne : FormalParameterListZeroOne
			| epsilon'''

def p_InferredFormalParameterList(p):
	'''InferredFormalParameterList : Identifier COMMAIdentifierMany'''

def p_COMMAIdentifierMany(p):
	'''COMMAIdentifierMany : COMMA Identifier COMMAIdentifierMany 
			| epsilon'''

def p_LambdaBody(p):
	'''LambdaBody : Expression
			| Block'''

def p_AssignmentExpression(p):
	'''AssignmentExpression : ConditionalExpression
			| Assignment'''

def p_Assignment(p):
	'''Assignment : LeftHandSide AssignmentOperator Expression'''

def p_LeftHandSide(p):
	'''LeftHandSide : ExpressionName
			| FieldAccess
			| ArrayAccess'''

def p_AssignmentOperator(p):
	'''AssignmentOperator : ASSIGN
			| STAR_ASSIGN
			| DIVIDE_ASSIGN
			| MOD_ASSIGN
			| PLUS_ASSIGN
			| MINUS_ASSIGN
			| LSHIFT_ASSIGN
			| RSHIFT_ASSIGN
			| UNSIGN_RSHIFT_ASSIGN
			| AND_ASSIGN
			| XOR_ASSIGN
			| OR_ASSIGN'''

def p_ConditionalExpression(p):
	'''ConditionalExpression : ConditionalOrExpression
			| ConditionalOrExpression QUESTIONMARK Expression COLON ConditionalExpression
			| ConditionalOrExpression QUESTIONMARK Expression COLON LambdaExpression'''

def p_ConditionalOrExpression(p):
	'''ConditionalOrExpression : ConditionalAndExpression
			| ConditionalOrExpression LOGICAL_OR ConditionalAndExpression'''

def p_ConditionalAndExpression(p):
	'''ConditionalAndExpression : InclusiveOrExpression
			| ConditionalAndExpression LOGICAL_AND InclusiveOrExpression'''

def p_InclusiveOrExpression(p):
	'''InclusiveOrExpression : ExclusiveOrExpression
			| InclusiveOrExpression OR ExclusiveOrExpression'''

def p_ExclusiveOrExpression(p):
	'''ExclusiveOrExpression : AndExpression
			| ExclusiveOrExpression XOR AndExpression'''

def p_AndExpression(p):
	'''AndExpression : EqualityExpression
			| AndExpression AND EqualityExpression'''

def p_EqualityExpression(p):
	'''EqualityExpression : RelationalExpression
			| EqualityExpression EQUALS RelationalExpression
			| EqualityExpression NOT_ASSIGN RelationalExpression'''

def p_RelationalExpression(p):
	'''RelationalExpression : ShiftExpression
			| RelationalExpression LESSER ShiftExpression
			| RelationalExpression GREATER ShiftExpression
			| RelationalExpression LESS_EQUALS ShiftExpression
			| RelationalExpression GREATER_EQUALS ShiftExpression
			| RelationalExpression INSTANCEOF ReferenceType'''

def p_ShiftExpression(p):
	'''ShiftExpression : AdditiveExpression
			| ShiftExpression LSHIFT AdditiveExpression
			| ShiftExpression RSHIFT AdditiveExpression
			| ShiftExpression UNRSHIFT AdditiveExpression'''

def p_AdditiveExpression(p):
	'''AdditiveExpression : MultiplicativeExpression
			| AdditiveExpression PLUS MultiplicativeExpression
			| AdditiveExpression MINUS MultiplicativeExpression'''

def p_MultiplicativeExpression(p):
	'''MultiplicativeExpression : UnaryExpression
			| MultiplicativeExpression STAR UnaryExpression
			| MultiplicativeExpression DIVIDE UnaryExpression
			| MultiplicativeExpression MOD UnaryExpression'''

def p_UnaryExpression(p):
	'''UnaryExpression : PreIncrementExpression
			| PreDecrementExpression
			| PLUS UnaryExpression
			| MINUS UnaryExpression
			| UnaryExpressionNotPlusMinus'''

def p_PreIncrementExpression(p):
	'''PreIncrementExpression : INC UnaryExpression'''

def p_PreDecrementExpression(p):
	'''PreDecrementExpression : DEC UnaryExpression'''

def p_UnaryExpressionNotPlusMinus(p):
	'''UnaryExpressionNotPlusMinus : PostfixExpression
			| BIT_WISE_NOT UnaryExpression
			| NOT UnaryExpression
			| CastExpression'''

def p_PostfixExpression(p):
	'''PostfixExpression : Primary
			| ExpressionName
			| PostIncrementExpression
			| PostDecrementExpression'''

def p_PostIncrementExpression(p):
	'''PostIncrementExpression : PostfixExpression INC'''

def p_PostDecrementExpression(p):
	'''PostDecrementExpression : PostfixExpression DEC'''

def p_CastExpression(p):
	'''CastExpression : LEFT_PAREN PrimitiveType RIGHT_PAREN UnaryExpression
			| LEFT_PAREN ReferenceType AdditionalBoundMany RIGHT_PAREN UnaryExpressionNotPlusMinus
			| LEFT_PAREN ReferenceType AdditionalBoundMany RIGHT_PAREN LambdaExpression'''

def p_AdditionalBoundMany(p):
	'''AdditionalBoundMany : AdditionalBound AdditionalBoundMany
			| epsilon'''

def p_ConstantExpression(p):
	'''ConstantExpression : Expression'''


def p_epsilon(p):
	'''epsilon : '''
	p[0] = ""

def p_error(p):
	print("Syntax error in input!")
	exit()


parser = yacc.yacc()
s = data
result = parser.parse(s)