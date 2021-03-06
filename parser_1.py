import ply.yacc as yacc
import sys
import os
from lexer import Lex as mylex
from lexer import *
# from lexer import lexer

tokens=Lex.tokens
# print(tokens)

#prod from 3
def p_start(p):
	'''start : CompilationUnit'''
	print("Starting .............")

def p_Identifier(p):
	'''Identifier : IDENTIFIER'''

def p_Literal(p):
	'''Literal : INTEGER
				| FLOATING
				| BOOLEAN
				| CHARACTER
				| STRING
				| NULL'''

#Products of 4

def p_Type(p):
	'''Type : PrimitiveType
		 | ReferenceType'''

def p_PrimitiveType(p):
	'''PrimitiveType : Annotation_1 NumericType
				  | NumericType
				  | BOOLEAN
				  | Annotation_1 BOOLEAN'''

def p_NumericType(p):
	'''NumericType : IntegralType
				| FloatingPointType'''

def p_IntegralType(p):
	'''IntegralType : BYTE 
				  | SHORT
				  | INT
				  | LONG
				  | CHAR'''

def p_FloatingPointType(p):
	'''FloatingPointType : FLOAT
					  | DOUBLE'''

def p_ReferenceType(p):
	'''ReferenceType : ClassOrInterfaceType
				  | TypeVariable
				  | ArrayType'''

def p_ClassOrInterfaceType(p):
	'''ClassOrInterfaceType : ClassType
						 | InterfaceType'''

def p_ClassType(p):
	'''ClassType : Annotation_1 Identifier TypeArguments
			  | Identifier TypeArguments
			  | Annotation_1 Identifier
			  | Identifier
			  | ClassOrInterfaceType DOT Annotation_1 Identifier TypeArguments
			  | ClassOrInterfaceType DOT Identifier TypeArguments
			  | ClassOrInterfaceType DOT Annotation_1 Identifier
			  | ClassOrInterfaceType DOT Identifier'''

def p_InterfaceType(p):
	'''InterfaceType : ClassType'''

def p_TypeVariable(p):
	'''TypeVariable : Annotation_1 Identifier
					| Identifier'''

def p_ArrayType(p):
	'''ArrayType : PrimitiveType Dims
			  | ClassOrInterfaceType Dims
			  | TypeVariable Dims'''

def p_Dims(p):
	'''Dims : Annotation_1 LEFT_SQUARE RIGHT_SQUARE Annotation_2
			| LEFT_SQUARE RIGHT_SQUARE Annotation_2
			| Annotation_1 LEFT_SQUARE RIGHT_SQUARE
			| LEFT_SQUARE RIGHT_SQUARE'''

def p_TypeParameter(p):
	'''TypeParameter : TypeParameterModifier_1 Identifier TypeBound
					| TypeParameterModifier_1 Identifier
					| Identifier TypeBound
					| Identifier'''

def p_TypeParameterModifier(p):
	'''TypeParameterModifier : Annotation'''

def p_TypeBound(p):
	'''TypeBound : EXTENDS TypeVariable
			  | EXTENDS ClassOrInterfaceType AdditionalBound_1
			  | EXTENDS ClassOrInterfaceType'''

def p_AdditionalBound(p):
	'''AdditionalBound : AND InterfaceType'''

def p_TypeArguments(p):
	'''TypeArguments : LESSER TypeArgumentList GREATER'''

def p_TypeArgumentList(p):
	'''TypeArgumentList : TypeArgument TypeArgument_1
						| TypeArgument'''

def p_TypeArgument(p):
	'''TypeArgument : ReferenceType
				 | Wildcard'''

def p_Wildcard(p):
	'''Wildcard : Annotation_1 QUESTIONMARK WildcardBounds
				| QUESTIONMARK WildcardBounds
				| Annotation_1 QUESTIONMARK
				| QUESTIONMARK'''

def p_WildcardBounds(p):
	'''WildcardBounds : EXTENDS ReferenceType 
				   | SUPER ReferenceType'''

#prods from 6

def p_TypeName(p):
 '''	TypeName : Identifier
 			| PackageOrTypeName DOT Identifier'''

def p_PackageOrTypeName(p):
 '''	PackageOrTypeName : Identifier
 			| PackageOrTypeName DOT Identifier'''

def p_ExpressionName(p):
	'''ExpressionName : Identifier
					| AmbiguousName DOT Identifier'''

def p_MethodName(p):
	'''MethodName : Identifier'''

def p_PackageName(p):
	'''PackageName : Identifier
				| PackageName DOT Identifier'''

def p_AmbiguousName(p):
	'''AmbiguousName : Identifier
					| AmbiguousName DOT Identifier'''

#Products of 7

def p_CompilationUnit(p):
	'''CompilationUnit : PackageDeclaration ImportDeclaration_1 TypeDeclaration_1
					| PackageDeclaration ImportDeclaration_1
					| PackageDeclaration TypeDeclaration_1
					| PackageDeclaration
					| ImportDeclaration_1 TypeDeclaration_1
					| ImportDeclaration_1
					| TypeDeclaration_1
					| epsilon'''

def p_PackageDeclaration(p):
	'''PackageDeclaration : PackageModifier_1 PACKAGE Identifier Identifier_1 SEMICOLON
						| PACKAGE Identifier Identifier_1 SEMICOLON
						| PackageModifier_1 PACKAGE Identifier SEMICOLON
						| PACKAGE Identifier SEMICOLON'''

def p_PackageModifier(p):
	'''PackageModifier : Annotation'''

def p_ImportDeclaration(p):
	'''ImportDeclaration : SingleTypeImportDeclaration
						| TypeImportOnDemandDeclaration
						| SingleStaticImportDeclaration
						| StaticImportOnDemandDeclaration'''

def p_SingleTypeImportDeclaration(p):
	'''SingleTypeImportDeclaration : IMPORT TypeName SEMICOLON'''

def p_TypeImportOnDemandDeclaration(p):
	'''TypeImportOnDemandDeclaration : IMPORT PackageOrTypeName DOT STAR SEMICOLON'''

def p_SingleStaticImportDeclaration(p):
	'''SingleStaticImportDeclaration : IMPORT STATIC TypeName DOT Identifier SEMICOLON'''

def p_StaticImportOnDemandDeclaration(p):
	'''StaticImportOnDemandDeclaration : IMPORT STATIC TypeName DOT STAR SEMICOLON'''

def p_TypeDeclaration(p):
	'''TypeDeclaration : ClassDeclaration
					| InterfaceDeclaration
					| SEMICOLON'''

#prod from 8

def p_ClassDeclaration(p):
	'''ClassDeclaration : NormalClassDeclaration
					 | EnumDeclaration'''

def p_NormalClassDeclaration(p):
	'''NormalClassDeclaration : ClassModifier_1 CLASS Identifier TypeParameters Superclass Superinterfaces ClassBody
							| ClassModifier_1 CLASS Identifier Superclass Superinterfaces ClassBody
							| CLASS Identifier TypeParameters Superclass Superinterfaces ClassBody
							| CLASS Identifier Superclass Superinterfaces ClassBody
							| ClassModifier_1 CLASS Identifier TypeParameters Superinterfaces ClassBody
							| ClassModifier_1 CLASS Identifier Superinterfaces ClassBody
							| CLASS Identifier TypeParameters Superinterfaces ClassBody
							| CLASS Identifier Superinterfaces ClassBody
							| ClassModifier_1 CLASS Identifier TypeParameters Superclass ClassBody
							| ClassModifier_1 CLASS Identifier Superclass ClassBody
							| CLASS Identifier TypeParameters Superclass ClassBody
							| CLASS Identifier Superclass ClassBody
							| ClassModifier_1 CLASS Identifier TypeParameters ClassBody
							| ClassModifier_1 CLASS Identifier ClassBody
							| CLASS Identifier TypeParameters ClassBody
							| CLASS Identifier ClassBody'''

def p_ClassModifier(p):
	'''ClassModifier : Annotation
				  | PUBLIC	 
				  | PROTECTED
				  | PRIVATE 
				  | ABSTRACT
				  | STATIC
				  | FINAL
				  | STRICTFP'''
	print("Class modi")

def p_TypeParameters(p):
	'''TypeParameters : LESSER TypeParameterList GREATER '''

def p_TypeParameterList(p):
	'''TypeParameterList : TypeParameter TypeParameter_1
						| TypeParameter'''

def p_Superclass(p):
	'''Superclass : EXTENDS ClassType'''

def p_Superinterfaces(p):
	'''Superinterfaces : IMPLEMENTS InterfaceTypeList	'''

def p_InterfaceTypeList(p):
	'''InterfaceTypeList : InterfaceType InterfaceType_1
						| InterfaceType'''

def p_ClassBody(p):
	'''ClassBody : LEFT_CURL ClassBodyDeclaration_1 RIGHT_CURL
				| LEFT_CURL RIGHT_CURL'''

def p_ClassBodyDeclaration(p):
	'''ClassBodyDeclaration : ClassMemberDeclaration
						 | InstanceInitializer
						 | StaticInitializer
						 | ConstructorDeclaration'''

def p_ClassMemberDeclaration(p):
	'''ClassMemberDeclaration : FieldDeclaration
						   | MethodDeclaration
						   | ClassDeclaration
						   | InterfaceDeclaration
						   | SEMICOLON'''

def p_FieldDeclaration(p):
	'''FieldDeclaration : FieldModifier_1 UnannType VariableDeclaratorList SEMICOLON
						| UnannType VariableDeclaratorList SEMICOLON'''

def p_FieldModifier(p):
	'''FieldModifier : Annotation 
				  | PUBLIC
				  |	PROTECTED 
				  |	PRIVATE
				  |	STATIC
				  |	FINAL
				  |	TRANSIENT 
				  |	VOLATILE'''

def p_VariableDeclaratorList(p):
	'''VariableDeclaratorList : VariableDeclarator VariableDeclarator_1
							| VariableDeclarator'''

def p_VariableDeclarator(p):
	'''VariableDeclarator : VariableDeclaratorId ASSIGN VariableInitializer
						| VariableDeclaratorId'''

def p_VariableDeclaratorId(p):
	'''VariableDeclaratorId : Identifier Dims 
						 | Identifier'''

def p_VariableInitializer(p):
	'''VariableInitializer : Expression 
						| ArrayInitializer'''

def p_UnannType(p):
	'''UnannType : UnannPrimitiveType
			  | UnannReferenceType'''

def p_UnannPrimitiveType(p):
	'''UnannPrimitiveType : NumericType
					   | BOOLEAN'''

def p_UnannReferenceType(p):
	'''UnannReferenceType : UnannClassOrInterfaceType
					   | UnannTypeVariable
					   | UnannArrayType'''

def p_UnannClassOrInterfaceType(p):
	'''UnannClassOrInterfaceType : UnannClassType
							  | UnannInterfaceType'''

def p_UnannClassType(p):
	'''UnannClassType : Identifier TypeArguments
				| UnannClassOrInterfaceType DOT Identifier TypeArguments
				| UnannClassOrInterfaceType DOT Annotation_1 Identifier TypeArguments
				| Identifier
				| UnannClassOrInterfaceType DOT Identifier
				| UnannClassOrInterfaceType DOT Annotation_1 Identifier'''

def p_UnannInterfaceType(p):
	'''UnannInterfaceType : UnannClassType'''

def p_UnannTypeVariable(p):
	'''UnannTypeVariable : Identifier'''

def p_UnannArrayType(p):
	'''UnannArrayType : UnannPrimitiveType Dims
				   | UnannClassOrInterfaceType Dims
				   | UnannTypeVariable Dims'''

def p_MethodDeclaration(p):
	'''MethodDeclaration : MethodModifier_1 MethodHeader MethodBody
						| MethodHeader MethodBody'''

def p_MethodModifier(p):
	'''MethodModifier : Annotation
				   | PUBLIC
				   | PROTECTED
				   | PRIVATE
				   | ABSTRACT 
				   | STATIC
				   | FINAL
				   | SYNCHRONIZED
				   | NATIVE
				   | STRICTFP'''
	print("from types ")
def p_MethodHeader(p):
	'''MethodHeader : Result MethodDeclarator Throws 
				 | Result MethodDeclarator
				 | TypeParameters Annotation_1 Result MethodDeclarator Throws
				 | TypeParameters Annotation_1 Result MethodDeclarator
				 | TypeParameters Result MethodDeclarator Throws
				 | TypeParameters Result MethodDeclarator'''


def p_Result(p):
	'''Result : UnannType
	       | VOID'''
	print("FRom void")

def p_MethodDeclarator(p):
	'''MethodDeclarator : Identifier LEFT_PAREN FormalParameterList RIGHT_PAREN Dims
				| Identifier LEFT_PAREN FormalParameterList RIGHT_PAREN
				| Identifier LEFT_PAREN RIGHT_PAREN Dims
				| Identifier LEFT_PAREN RIGHT_PAREN'''

def p_FormalParameterList(p):
	'''FormalParameterList : ReceiverParameter
						| FormalParameters COMMA LastFormalParameter
						| LastFormalParameter'''

def p_FormalParameters(p):
	'''FormalParameters : FormalParameter FormalParameter_1 
					 | ReceiverParameter FormalParameter_1
					 | FormalParameter 
					 | ReceiverParameter'''

def p_FormalParameter(p):
	'''FormalParameter : VariableModifier_1 UnannType VariableDeclaratorId
						| UnannType VariableDeclaratorId'''

def p_VariableModifier(p):
	'''VariableModifier : Annotation
					 | FINAL'''

def p_LastFormalParameter(p):
	'''LastFormalParameter : VariableModifier_1 UnannType Annotation_1 VARARGS VariableDeclaratorId
						| VariableModifier_1 UnannType VARARGS VariableDeclaratorId
						| UnannType Annotation_1 VARARGS VariableDeclaratorId
						| UnannType VARARGS VariableDeclaratorId
						| FormalParameter'''

def p_ReceiverParameter(p):
	'''ReceiverParameter : Annotation_1 UnannType  Identifier_1 THIS
						| UnannType  Identifier_1 THIS
						| Annotation_1 UnannType  THIS
						| UnannType  THIS'''

def p_Throws(p):
	'''Throws : THROWS ExceptionTypeList'''

def p_ExceptionTypeList(p):
	'''ExceptionTypeList : ExceptionType ExceptionType_1
						| ExceptionType'''

def p_ExceptionType(p):
	'''ExceptionType : ClassType
				  |	TypeVariable'''

def p_MethodBody(p):
	'''MethodBody : Block
			   | SEMICOLON'''

def p_InstanceInitializer(p):
	'''InstanceInitializer : Block'''

def p_StaticInitializer(p):
	'''StaticInitializer : STATIC Block'''

def p_ConstructorDeclaration(p):
	'''ConstructorDeclaration : ConstructorModifier_1 ConstructorDeclarator Throws ConstructorBody
							| ConstructorModifier_1 ConstructorDeclarator ConstructorBody
							| ConstructorDeclarator Throws ConstructorBody
							| ConstructorDeclarator ConstructorBody'''

def p_ConstructorModifier(p):
 '''	ConstructorModifier : Annotation
 						| PUBLIC
 						| PROTECTED
 						| PRIVATE'''

def p_ConstructorDeclarator(p):
	'''ConstructorDeclarator : TypeParameters SimpleTypeName LEFT_PAREN FormalParameterList RIGHT_PAREN
							| SimpleTypeName LEFT_PAREN FormalParameterList RIGHT_PAREN
							| TypeParameters SimpleTypeName LEFT_PAREN RIGHT_PAREN
							| SimpleTypeName LEFT_PAREN RIGHT_PAREN'''

def p_SimpleTypeName(p):
	'''SimpleTypeName : Identifier'''

def p_ConstructorBody(p):
	'''ConstructorBody : LEFT_CURL ExplicitConstructorInvocation BlockStatements RIGHT_CURL
						| LEFT_CURL BlockStatements RIGHT_CURL
						| LEFT_CURL ExplicitConstructorInvocation RIGHT_CURL
						| LEFT_CURL RIGHT_CURL'''

def p_ExplicitConstructorInvocation(p):
	'''ExplicitConstructorInvocation : TypeArguments THIS LEFT_PAREN ArgumentList RIGHT_PAREN SEMICOLON
								  | TypeArguments SUPER LEFT_PAREN ArgumentList RIGHT_PAREN SEMICOLON
								  | ExpressionName DOT TypeArguments SUPER LEFT_PAREN ArgumentList RIGHT_PAREN SEMICOLON
								  | Primary DOT TypeArguments SUPER LEFT_PAREN ArgumentList RIGHT_PAREN SEMICOLON
								  | THIS LEFT_PAREN ArgumentList RIGHT_PAREN SEMICOLON
								  | SUPER LEFT_PAREN ArgumentList RIGHT_PAREN SEMICOLON
								  | ExpressionName DOT SUPER LEFT_PAREN ArgumentList RIGHT_PAREN SEMICOLON
								  | Primary DOT SUPER LEFT_PAREN ArgumentList RIGHT_PAREN SEMICOLON
								  | TypeArguments THIS LEFT_PAREN RIGHT_PAREN SEMICOLON
								  | TypeArguments SUPER LEFT_PAREN RIGHT_PAREN SEMICOLON
								  | ExpressionName DOT TypeArguments SUPER LEFT_PAREN RIGHT_PAREN SEMICOLON
								  | Primary DOT TypeArguments SUPER LEFT_PAREN RIGHT_PAREN SEMICOLON
								  | THIS LEFT_PAREN RIGHT_PAREN SEMICOLON
								  | SUPER LEFT_PAREN RIGHT_PAREN SEMICOLON
								  | ExpressionName DOT SUPER LEFT_PAREN RIGHT_PAREN SEMICOLON
								  | Primary DOT SUPER LEFT_PAREN RIGHT_PAREN SEMICOLON'''

def p_EnumDeclaration(p):
	'''EnumDeclaration : ClassModifier_1 ENUM Identifier Superinterfaces EnumBody
						| ENUM Identifier Superinterfaces EnumBody
						| ClassModifier_1 ENUM Identifier EnumBody
						| ENUM Identifier EnumBody'''

def p_EnumBody(p):
	'''EnumBody : LEFT_CURL EnumConstantList COMMA EnumBodyDeclarations RIGHT_CURL
				| LEFT_CURL COMMA EnumBodyDeclarations RIGHT_CURL
				| LEFT_CURL EnumConstantList EnumBodyDeclarations RIGHT_CURL
				| LEFT_CURL EnumBodyDeclarations RIGHT_CURL
				| LEFT_CURL EnumConstantList COMMA RIGHT_CURL
				| LEFT_CURL COMMA RIGHT_CURL
				| LEFT_CURL EnumConstantList RIGHT_CURL
				| LEFT_CURL RIGHT_CURL'''

def p_EnumConstantList(p):
	'''EnumConstantList : EnumConstant EnumConstant_1
						| EnumConstant'''

def p_EnumConstant(p):
	'''EnumConstant : EnumConstantModifier_1 Identifier ClassBody
					| EnumConstantModifier_1 Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
					| EnumConstantModifier_1 Identifier LEFT_PAREN ArgumentList RIGHT_PAREN ClassBody
					| EnumConstantModifier_1 Identifier LEFT_PAREN RIGHT_PAREN
					| EnumConstantModifier_1 Identifier LEFT_PAREN RIGHT_PAREN ClassBody
					| EnumConstantModifier_1 Identifier
					| Identifier ClassBody
					| Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
					| Identifier LEFT_PAREN ArgumentList RIGHT_PAREN ClassBody
					| Identifier LEFT_PAREN RIGHT_PAREN
					| Identifier LEFT_PAREN RIGHT_PAREN ClassBody
					| Identifier'''

def p_EnumConstantModifier(p):
	'''EnumConstantModifier : Annotation'''

def p_EnumBodyDeclarations(p):
	'''EnumBodyDeclarations : SEMICOLON ClassBodyDeclaration_1
							| SEMICOLON'''

#Products of 9

def p_InterfaceDeclaration(p):
	'''InterfaceDeclaration : NormalInterfaceDeclaration
							| AnnotationTypeDeclaration'''

def p_NormalInterfaceDeclaration(p):
	'''NormalInterfaceDeclaration : InterfaceModifier_1 INTERFACE Identifier TypeParameters ExtendsInterfaces InterfaceBody
								| InterfaceModifier_1 INTERFACE Identifier ExtendsInterfaces InterfaceBody
								| InterfaceModifier_1 INTERFACE Identifier TypeParameters InterfaceBody
								| InterfaceModifier_1 INTERFACE Identifier InterfaceBody
								| INTERFACE Identifier TypeParameters ExtendsInterfaces InterfaceBody
								| INTERFACE Identifier ExtendsInterfaces InterfaceBody
								| INTERFACE Identifier TypeParameters InterfaceBody
								| INTERFACE Identifier InterfaceBody'''

def p_InterfaceModifier(p):
	'''InterfaceModifier : Annotation
						| PUBLIC
						| PROTECTED
						| PRIVATE
						| ABSTRACT
						| STATIC
						| STRICTFP'''

def p_ExtendsInterfaces(p):
	'''ExtendsInterfaces : EXTENDS InterfaceTypeList'''

def p_InterfaceBody(p):
	'''InterfaceBody : LEFT_CURL InterfaceMemberDeclaration_1 RIGHT_CURL
					| LEFT_CURL RIGHT_CURL'''

def p_InterfaceMemberDeclaration(p):
	'''InterfaceMemberDeclaration : ConstantDeclaration
								| InterfaceMethodDeclaration
								| ClassDeclaration
								| InterfaceDeclaration
								| SEMICOLON'''

def p_ConstantDeclaration(p):
	'''ConstantDeclaration : ConstantModifier_1 UnannType VariableDeclaratorList SEMICOLON
							| UnannType VariableDeclaratorList SEMICOLON'''

def p_ConstantModifier(p):
	'''ConstantModifier : Annotation
					| PUBLIC
					| STATIC
					| FINAL'''

def p_InterfaceMethodDeclaration(p):
	'''InterfaceMethodDeclaration : InterfaceMethodModifier_1 MethodHeader MethodBody
								| MethodHeader MethodBody'''

def p_InterfaceMethodModifier(p):
	'''InterfaceMethodModifier : Annotation
							| PUBLIC
							| ABSTRACT
							| DEFAULT
							| STATIC
							| STRICTFP'''

def p_AnnotationTypeDeclaration(p):
	'''AnnotationTypeDeclaration : InterfaceModifier_1 ATTHERATE INTERFACE Identifier AnnotationTypeBody
								| ATTHERATE INTERFACE Identifier AnnotationTypeBody'''

def p_AnnotationTypeBody(p):
	'''AnnotationTypeBody : LEFT_CURL AnnotationTypeMemberDeclaration_1 RIGHT_CURL
							| LEFT_CURL RIGHT_CURL'''

def p_AnnotationTypeMemberDeclaration(p):
	'''AnnotationTypeMemberDeclaration : AnnotationTypeElementDeclaration
									| ConstantDeclaration
									| ClassDeclaration
									| InterfaceDeclaration
									| SEMICOLON'''

def p_AnnotationTypeElementDeclaration(p):
	'''AnnotationTypeElementDeclaration : AnnotationTypeElementModifier_1 UnannType Identifier LEFT_PAREN RIGHT_PAREN Dims DefaultValue SEMICOLON
									 | AnnotationTypeElementModifier_1 UnannType Identifier LEFT_PAREN RIGHT_PAREN Dims SEMICOLON
									 | AnnotationTypeElementModifier_1 UnannType Identifier LEFT_PAREN RIGHT_PAREN DefaultValue SEMICOLON
									 | AnnotationTypeElementModifier_1 UnannType Identifier LEFT_PAREN RIGHT_PAREN SEMICOLON
									 | UnannType Identifier LEFT_PAREN RIGHT_PAREN Dims DefaultValue SEMICOLON
									 | UnannType Identifier LEFT_PAREN RIGHT_PAREN Dims SEMICOLON
									 | UnannType Identifier LEFT_PAREN RIGHT_PAREN DefaultValue SEMICOLON
									 | UnannType Identifier LEFT_PAREN RIGHT_PAREN SEMICOLON'''

def p_AnnotationTypeElementModifier(p):
	'''AnnotationTypeElementModifier : Annotation
								  | PUBLIC
								  | ABSTRACT'''

def p_DefaultValue(p):
	'''DefaultValue : DEFAULT ElementValue'''

def p_Annotation(p):
	'''Annotation : NormalAnnotation
			   | MarkerAnnotation
			   | SingleElementAnnotation'''

def p_NormalAnnotation(p):
	'''NormalAnnotation : ATTHERATE TypeName LEFT_PAREN ElementValuePairList RIGHT_PAREN
					 | ATTHERATE TypeName LEFT_PAREN RIGHT_PAREN'''

def p_ElementValuePairList(p):
	'''ElementValuePairList : ElementValuePair ElementValuePair_1
							| ElementValuePair'''

def p_ElementValuePair(p):
	'''ElementValuePair : Identifier ASSIGN ElementValue'''

def p_ElementValue(p):
	'''ElementValue : ConditionalExpression
				 | ElementValueArrayInitializer
				 | Annotation'''

def p_ElementValueArrayInitializer(p):
	'''ElementValueArrayInitializer : LEFT_CURL ElementValueList COMMA RIGHT_CURL
								 | LEFT_CURL ElementValueList RIGHT_CURL
								 | LEFT_CURL COMMA RIGHT_CURL
								 | LEFT_CURL RIGHT_CURL'''

def p_ElementValueList(p):
	'''ElementValueList : ElementValue ElementValue_1
						| ElementValue'''

def p_MarkerAnnotation(p):
	'''MarkerAnnotation : ATTHERATE TypeName'''

def p_SingleElementAnnotation(p):
	'''SingleElementAnnotation : ATTHERATE TypeName LEFT_PAREN ElementValue RIGHT_PAREN'''

#Products of 10

def p_ArrayInitializer(p):
	'''ArrayInitializer : LEFT_CURL VariableInitializerList COMMA RIGHT_CURL
					 | LEFT_CURL VariableInitializerList RIGHT_CURL
					 | LEFT_CURL COMMA RIGHT_CURL
					 | LEFT_CURL RIGHT_CURL'''

def p_VariableInitializerList(p):
	'''VariableInitializerList : VariableInitializer VariableInitializer_2
								| VariableInitializer'''

#products of 14

def p_Block(p):
	'''Block : LEFT_CURL BlockStatements RIGHT_CURL
		  | LEFT_CURL RIGHT_CURL'''

def p_BlockStatements(p):
	'''BlockStatements : BlockStatement BlockStatement_1
						| BlockStatement'''

def p_BlockStatement(p):
	'''BlockStatement : LocalVariableDeclarationStatement
				   | ClassDeclaration
				   | Statement'''

def p_LocalVariableDeclarationStatement(p):
	'''LocalVariableDeclarationStatement : LocalVariableDeclaration SEMICOLON'''

def p_LocalVariableDeclaration(p):
	'''LocalVariableDeclaration : VariableModifier_1 UnannType VariableDeclaratorList
								| UnannType VariableDeclaratorList'''

def p_Statement(p):
	'''Statement : StatementWithoutTrailingSubstatement
			  | LabeledStatement
			  | IfThenStatement
			  | IfThenElseStatement
			  | WhileStatement
			  | ForStatement'''
	print("ERROR::: StatementNoShortIf...............88888.")		  

def p_StatementNoShortIf(p):
	'''StatementNoShortIf : StatementWithoutTrailingSubstatement
					   | LabeledStatementNoShortIf
					   | IfThenElseStatementNoShortIf
					   | WhileStatementNoShortIf
					   | ForStatementNoShortIf'''
	print("ERROR::: StatementNoShortIf................")

def p_StatementWithoutTrailingSubstatement(p):
	'''StatementWithoutTrailingSubstatement : Block
										 | EmptyStatement
										 | ExpressionStatement
										 | AssertStatement
										 | SwitchStatement
										 | DoStatement
										 | BreakStatement
										 | ContinueStatement
										 | ReturnStatement
										 | SynchronizedStatement
										 | ThrowStatement
										 | TryStatement'''

def p_EmptyStatement(p):
	'''EmptyStatement : SEMICOLON'''

def p_LabeledStatement(p):
	'''LabeledStatement : Identifier COLON Statement'''

def p_LabeledStatementNoShortIf(p):
	'''LabeledStatementNoShortIf : Identifier COLON StatementNoShortIf'''

def p_ExpressionStatement(p):
	'''ExpressionStatement : StatementExpression SEMICOLON'''

def p_StatementExpression(p):
	'''StatementExpression : Assignment
						| PreIncrementExpression
						| PreDecrementExpression
						| PostIncrementExpression
						| PostDecrementExpression
						| MethodInvocation
						| ClassInstanceCreationExpression'''

def p_IfThenStatement(p):
	'''IfThenStatement : IF LEFT_PAREN Expression RIGHT_PAREN Statement'''

def p_IfThenElseStatement(p):
	'''IfThenElseStatement : IF LEFT_PAREN Expression RIGHT_PAREN StatementNoShortIf ELSE Statement'''

def p_IfThenElseStatementNoShortIf(p):
	'''IfThenElseStatementNoShortIf : IF LEFT_PAREN Expression RIGHT_PAREN StatementNoShortIf ELSE StatementNoShortIf'''

def p_AssertStatement(p):
	'''AssertStatement : ASSERT Expression SEMICOLON
					| ASSERT Expression COLON Expression SEMICOLON'''

def p_SwitchStatement(p):
	'''SwitchStatement : SWITCH LEFT_PAREN Expression RIGHT_PAREN SwitchBlock'''

def p_SwitchBlock(p):
	'''SwitchBlock : LEFT_CURL SwitchBlockStatementGroup_1 SwitchLabel_1 RIGHT_CURL
					| LEFT_CURL SwitchLabel_1 RIGHT_CURL
					| LEFT_CURL SwitchBlockStatementGroup_1 RIGHT_CURL
					| LEFT_CURL RIGHT_CURL'''

def p_SwitchBlockStatementGroup(p):
	'''SwitchBlockStatementGroup : SwitchLabels BlockStatements'''

def p_SwitchLabels(p):
	'''SwitchLabels : SwitchLabel SwitchLabel_1
					| SwitchLabel'''

def p_SwitchLabel(p):
	'''SwitchLabel : CASE ConstantExpression COLON
				| CASE EnumConstantName COLON
				| DEFAULT COLON'''

def p_EnumConstantName(p):
	'''EnumConstantName : Identifier'''

def p_WhileStatement(p):
	'''WhileStatement : WHILE LEFT_PAREN Expression RIGHT_PAREN Statement'''
	print("ERROR::WhileStatement ")

def p_WhileStatementNoShortIf(p):
	'''WhileStatementNoShortIf : WHILE LEFT_PAREN Expression RIGHT_PAREN StatementNoShortIf'''

def p_DoStatement(p):
	'''DoStatement : DO Statement WHILE LEFT_PAREN Expression LEFT_PAREN SEMICOLON'''

def p_ForStatement(p):
	'''ForStatement : BasicForStatement
				 | EnhancedForStatement'''

def p_ForStatementNoShortIf(p):
	'''ForStatementNoShortIf : BasicForStatementNoShortIf
						  | EnhancedForStatementNoShortIf'''

def p_BasicForStatement(p):
	'''BasicForStatement : FOR LEFT_PAREN ForInit SEMICOLON Expression SEMICOLON ForUpdate RIGHT_PAREN Statement
					  | FOR LEFT_PAREN ForInit SEMICOLON Expression SEMICOLON RIGHT_PAREN Statement
					  | FOR LEFT_PAREN ForInit SEMICOLON SEMICOLON RIGHT_PAREN Statement
					  | FOR LEFT_PAREN SEMICOLON Expression SEMICOLON RIGHT_PAREN Statement
					  | FOR LEFT_PAREN ForInit SEMICOLON SEMICOLON ForUpdate RIGHT_PAREN Statement
					  | FOR LEFT_PAREN SEMICOLON Expression SEMICOLON ForUpdate RIGHT_PAREN Statement
					  | FOR LEFT_PAREN SEMICOLON SEMICOLON ForUpdate RIGHT_PAREN Statement
					  | FOR LEFT_PAREN SEMICOLON SEMICOLON RIGHT_PAREN Statement'''

def p_BasicForStatementNoShortIf(p):
	'''BasicForStatementNoShortIf : FOR LEFT_PAREN ForInit SEMICOLON Expression SEMICOLON ForUpdate RIGHT_PAREN StatementNoShortIf
							   | FOR LEFT_PAREN ForInit SEMICOLON Expression SEMICOLON RIGHT_PAREN StatementNoShortIf
							   | FOR LEFT_PAREN ForInit SEMICOLON SEMICOLON RIGHT_PAREN StatementNoShortIf
							   | FOR LEFT_PAREN SEMICOLON Expression SEMICOLON RIGHT_PAREN StatementNoShortIf
							   | FOR LEFT_PAREN ForInit SEMICOLON SEMICOLON ForUpdate RIGHT_PAREN StatementNoShortIf
							   | FOR LEFT_PAREN SEMICOLON Expression SEMICOLON ForUpdate RIGHT_PAREN StatementNoShortIf
							   | FOR LEFT_PAREN SEMICOLON SEMICOLON ForUpdate RIGHT_PAREN StatementNoShortIf
							   | FOR LEFT_PAREN SEMICOLON SEMICOLON RIGHT_PAREN StatementNoShortIf'''

def p_ForInit(p):
	'''ForInit : StatementExpressionList
			| LocalVariableDeclaration'''

def p_ForUpdate(p):
	'''ForUpdate : StatementExpressionList'''

def p_StatementExpressionList(p):
	'''StatementExpressionList : StatementExpression StatementExpression_1
								| StatementExpression'''

def p_EnhancedForStatement(p):
	'''EnhancedForStatement : FOR LEFT_PAREN VariableModifier_1 UnannType VariableDeclaratorId COLON Expression RIGHT_PAREN Statement
							| FOR LEFT_PAREN UnannType VariableDeclaratorId COLON Expression RIGHT_PAREN Statement'''

def p_EnhancedForStatementNoShortIf(p):
	'''EnhancedForStatementNoShortIf : FOR LEFT_PAREN VariableModifier_1 UnannType VariableDeclaratorId COLON Expression RIGHT_PAREN StatementNoShortIf
									| FOR LEFT_PAREN UnannType VariableDeclaratorId COLON Expression RIGHT_PAREN StatementNoShortIf'''

def p_BreakStatement(p):
	'''BreakStatement : BREAK Identifier SEMICOLON
				   | BREAK SEMICOLON'''

def p_ContinueStatement(p):
	'''ContinueStatement : CONTINUE Identifier SEMICOLON
					  | CONTINUE SEMICOLON'''

def p_ReturnStatement(p):
	'''ReturnStatement : RETURN Expression SEMICOLON
					| RETURN SEMICOLON'''

def p_ThrowStatement(p):
	'''ThrowStatement : THROW Expression SEMICOLON'''

def p_SynchronizedStatement(p):
	'''SynchronizedStatement : SYNCHRONIZED LEFT_PAREN Expression RIGHT_PAREN Block'''

def p_TryStatement(p):
	'''TryStatement : TRY Block Catches
				 | TRY Block Catches Finally
				 | TRY Block Finally
				 | TryWithResourcesStatement'''

def p_Catches(p):
	'''Catches : CatchClause CatchClause_1
				| CatchClause'''

def p_CatchClause(p):
	'''CatchClause : CATCH LEFT_PAREN CatchFormalParameter RIGHT_PAREN Block'''

def p_CatchFormalParameter(p):
	'''CatchFormalParameter : VariableModifier_1 CatchType VariableDeclaratorId
							| CatchType VariableDeclaratorId'''

def p_CatchType(p):
	'''CatchType : UnannClassType ClassType_1
				| UnannClassType'''

def p_Finally(p):
	'''Finally : FINALLY Block'''

def p_TryWithResourcesStatement(p):
	'''TryWithResourcesStatement : TRY ResourceSpecification Block Catches Finally
							  | TRY ResourceSpecification Block Finally
							  | TRY ResourceSpecification Block Catches
							  | TRY ResourceSpecification Block'''

def p_ResourceSpecification(p):
	'''ResourceSpecification : LEFT_PAREN ResourceList SEMICOLON RIGHT_PAREN
						  | LEFT_PAREN ResourceList RIGHT_PAREN'''

def p_ResourceList(p):
	'''ResourceList : Resource Resource_1
					| Resource'''

def p_Resource(p):
	'''Resource : VariableModifier_1 UnannType VariableDeclaratorId ASSIGN Expression
				| UnannType VariableDeclaratorId ASSIGN Expression'''

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
			| BOOLEAN SquareBracket DOT CLASS
			| TypeName DOT CLASS
			| NumericType DOT CLASS
			| BOOLEAN DOT CLASS
			| VOID DOT CLASS'''

def p_ClassInstanceCreationExpression(p):
	'''ClassInstanceCreationExpression : UnqualifiedClassInstanceCreationExpression
			| ExpressionName DOT UnqualifiedClassInstanceCreationExpression
			| Primary DOT UnqualifiedClassInstanceCreationExpression'''

def p_UnqualifiedClassInstanceCreationExpression(p):
	'''UnqualifiedClassInstanceCreationExpression : NEW TypeArguments ClassOrInterfaceTypeToInstantiate LEFT_PAREN ArgumentList RIGHT_PAREN ClassBody
												| NEW ClassOrInterfaceTypeToInstantiate LEFT_PAREN ArgumentList RIGHT_PAREN ClassBody
												| NEW TypeArguments ClassOrInterfaceTypeToInstantiate LEFT_PAREN RIGHT_PAREN ClassBody
												| NEW ClassOrInterfaceTypeToInstantiate LEFT_PAREN RIGHT_PAREN ClassBody
												| NEW TypeArguments ClassOrInterfaceTypeToInstantiate LEFT_PAREN ArgumentList RIGHT_PAREN 
												| NEW ClassOrInterfaceTypeToInstantiate LEFT_PAREN ArgumentList RIGHT_PAREN
												| NEW TypeArguments ClassOrInterfaceTypeToInstantiate LEFT_PAREN RIGHT_PAREN
												| NEW ClassOrInterfaceTypeToInstantiate LEFT_PAREN RIGHT_PAREN'''

def p_ClassOrInterfaceTypeToInstantiate(p):
	'''ClassOrInterfaceTypeToInstantiate : Annotation_1 Identifier AnnotationIdentifierMany TypeArgumentsOrDiamond
										| Identifier AnnotationIdentifierMany TypeArgumentsOrDiamond
										| Annotation_1 Identifier TypeArgumentsOrDiamond
										| Identifier TypeArgumentsOrDiamond
										| Annotation_1 Identifier AnnotationIdentifierMany
										| Identifier AnnotationIdentifierMany
										| Annotation_1 Identifier
										| Identifier'''

def p_TypeArgumentsOrDiamond(p):
	'''TypeArgumentsOrDiamond : TypeArguments
			| LESSER GREATER'''

def p_FieldAccess(p):
	'''FieldAccess : Primary DOT Identifier
			| SUPER DOT Identifier
			| TypeName DOT SUPER DOT Identifier'''

def p_ArrayAccess(p):
	'''ArrayAccess : ExpressionName LEFT_SQUARE Expression RIGHT_SQUARE
			| PrimaryNoNewArray LEFT_SQUARE Expression RIGHT_SQUARE'''

def p_MethodInvocation(p):
	'''MethodInvocation : MethodName LEFT_PAREN ArgumentList RIGHT_PAREN
			| TypeName DOT TypeArguments Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| ExpressionName DOT TypeArguments Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| Primary DOT TypeArguments Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| SUPER DOT TypeArguments Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| TypeName DOT SUPER DOT TypeArguments Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| TypeName DOT Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| ExpressionName DOT Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| Primary DOT Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| SUPER DOT Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| TypeName DOT SUPER DOT Identifier LEFT_PAREN ArgumentList RIGHT_PAREN
			| MethodName LEFT_PAREN RIGHT_PAREN
			| TypeName DOT TypeArguments Identifier LEFT_PAREN RIGHT_PAREN
			| ExpressionName DOT TypeArguments Identifier LEFT_PAREN RIGHT_PAREN
			| Primary DOT TypeArguments Identifier LEFT_PAREN RIGHT_PAREN
			| SUPER DOT TypeArguments Identifier LEFT_PAREN RIGHT_PAREN
			| TypeName DOT SUPER DOT TypeArguments Identifier LEFT_PAREN RIGHT_PAREN
			| TypeName DOT Identifier LEFT_PAREN RIGHT_PAREN
			| ExpressionName DOT Identifier LEFT_PAREN RIGHT_PAREN
			| Primary DOT Identifier LEFT_PAREN RIGHT_PAREN
			| SUPER DOT Identifier LEFT_PAREN RIGHT_PAREN
			| TypeName DOT SUPER DOT Identifier LEFT_PAREN RIGHT_PAREN'''

def p_ArgumentList(p):
	'''ArgumentList : Expression COMMAExpressionMany
					| Expression'''

def p_MethodReference(p):
	'''MethodReference : ExpressionName DOUBLECOLON TypeArguments Identifier
			| ReferenceType DOUBLECOLON TypeArguments Identifier
			| Primary DOUBLECOLON TypeArguments Identifier
			| SUPER DOUBLECOLON TypeArguments Identifier
			| TypeName DOT SUPER DOUBLECOLON TypeArguments Identifier
			| ClassType DOUBLECOLON TypeArguments NEW
			| ExpressionName DOUBLECOLON Identifier
			| ReferenceType DOUBLECOLON Identifier
			| Primary DOUBLECOLON Identifier
			| SUPER DOUBLECOLON Identifier
			| TypeName DOT SUPER DOUBLECOLON Identifier
			| ClassType DOUBLECOLON NEW
			| ArrayType DOUBLECOLON NEW'''

def p_ArrayCreationExpression(p):
	'''ArrayCreationExpression : NEW PrimitiveType DimExprs Dims
			| NEW ClassOrInterfaceType DimExprs Dims
			| NEW PrimitiveType DimExprs
			| NEW ClassOrInterfaceType DimExprs
			| NEW PrimitiveType Dims ArrayInitializer
			| NEW ClassOrInterfaceType Dims ArrayInitializer'''

def p_DimExprs(p):
	'''DimExprs : DimExpr DimExprMany
				| DimExpr'''

def p_DimExpr(p):
	'''DimExpr : Annotation_1 LEFT_PAREN Expression RIGHT_PAREN
				| LEFT_PAREN Expression RIGHT_PAREN'''

def p_Expression(p):
	'''Expression : LambdaExpression
			| AssignmentExpression'''

def p_LambdaExpression(p):
	'''LambdaExpression : LambdaParameters LEFTARROW LambdaBody'''

def p_LambdaParameters(p):
	'''LambdaParameters : Identifier
			| LEFT_PAREN FormalParameterList RIGHT_PAREN
			| LEFT_PAREN InferredFormalParameterList RIGHT_PAREN
			| LEFT_PAREN RIGHT_PAREN'''

def p_InferredFormalParameterList(p):
	'''InferredFormalParameterList : Identifier COMMAIdentifierMany
									| Identifier'''

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
			| LEFT_PAREN ReferenceType AdditionalBound_1 RIGHT_PAREN UnaryExpressionNotPlusMinus
			| LEFT_PAREN ReferenceType AdditionalBound_1 RIGHT_PAREN LambdaExpression
			| LEFT_PAREN ReferenceType RIGHT_PAREN UnaryExpressionNotPlusMinus
			| LEFT_PAREN ReferenceType RIGHT_PAREN LambdaExpression'''

def p_ConstantExpression(p):
	'''ConstantExpression : Expression'''

#EXTRAS

def p_Annotation_1(p):
	'''Annotation_1 : Annotation Annotation_1
				 | Annotation'''

def p_TypeParameterModifier_1(p):
	'''TypeParameterModifier_1 : TypeParameterModifier TypeParameterModifier_1
							| TypeParameterModifier'''

# def p_TypeBound_1(p):
# 	'''TypeBound_1 : TypeBound 
# 				| epsilon'''

def p_AdditionalBound_1(p):
	'''AdditionalBound_1 : AdditionalBound AdditionalBound_1
					  | AdditionalBound'''

def p_TypeArgument_1(p):
	'''TypeArgument_1 : COMMA TypeArgument TypeArgument_1
				   | COMMA TypeArgument'''

# def p_WildcardBounds_1(p):
# 	'''WildcardBounds_1 : WildcardBounds 
# 					 | epsilon'''

def p_ImportDeclaration_1(p):
	'''ImportDeclaration_1 : ImportDeclaration ImportDeclaration_1
						| ImportDeclaration'''

def p_TypeDeclaration_1(p):
	'''TypeDeclaration_1 : TypeDeclaration TypeDeclaration_1
						| TypeDeclaration'''

def p_PackageModifier_1(p):
	'''PackageModifier_1 : PackageModifier PackageModifier_1
						| PackageModifier'''

def p_ClassModifier_1(p):
	'''ClassModifier_1 : ClassModifier ClassModifier_1 
					| ClassModifier'''

# def p_TypeParameters_1(p):
# 	'''TypeParameters_1 : TypeParameters
# 					 | epsilon'''

# def p_Superclass_1(p):
# 	'''Superclass_1 : Superclass 
# 				 | epsilon'''

# def p_Superinterfaces_1(p):
# 	'''Superinterfaces_1 : Superinterfaces
# 					  | epsilon'''

def p_TypeParameter_1(p):
	'''TypeParameter_1 : COMMA TypeParameter TypeParameter_1
					| COMMA TypeParameter'''

def p_InterfaceType_1(p):
	'''InterfaceType_1 : COMMA InterfaceType InterfaceType_1
					| COMMA InterfaceType'''

def p_ClassBodyDeclaration_1(p):
	'''ClassBodyDeclaration_1 : ClassBodyDeclaration ClassBodyDeclaration_1 
							| ClassBodyDeclaration'''

def p_FieldModifier_1(p):
	'''FieldModifier_1 : FieldModifier FieldModifier_1
					| FieldModifier'''

def p_VariableDeclarator_1(p):
	'''VariableDeclarator_1 : COMMA VariableDeclarator VariableDeclarator_1
						 | COMMA VariableDeclarator'''

# def p_VariableInitializer_1(p):
# 	'''VariableInitializer_1 : ASSIGN VariableInitializer 
# 						  | ASSIGN'''

# def p_TypeArguments_1(p):
# 	'''TypeArguments_1 : TypeArguments 
# 					| epsilon'''

def p_MethodModifier_1(p):
	'''MethodModifier_1 : MethodModifier MethodModifier_1 
					| MethodModifier'''

# def p_FormalParameterList_1(p):
# 	'''FormalParameterList_1 : FormalParameterList
# 						  | epsilon'''

def p_FormalParameter_1(p):
	'''FormalParameter_1 : COMMA FormalParameter FormalParameter_1 
					  | COMMA FormalParameter'''

def p_VariableModifier_1(p):
	'''VariableModifier_1 : VariableModifier VariableModifier_1 
					   | VariableModifier'''

def p_Identifier_1(p):
	'''Identifier_1 : DOT Identifier Identifier_1 
				 | DOT Identifier'''

def p_ExceptionType_1(p):
	'''ExceptionType_1 : COMMA ExceptionType ExceptionType_1
					| COMMA ExceptionType'''

# def p_Throws_1(p):
# 	'''Throws_1 : Throws 
# 			 | epsilon'''

def p_ConstructorModifier_1(p):
	'''ConstructorModifier_1 : ConstructorModifier ConstructorModifier_1
						  | ConstructorModifier'''

# def p_ExplicitConstructorInvocation_1(p):
# 	'''ExplicitConstructorInvocation_1 : ExplicitConstructorInvocation 
# 									| epsilon'''

# def p_BlockStatements_1(p):
# 	'''BlockStatements_1 : BlockStatements 
# 					  | epsilon'''

# def p_ArgumentList_1(p):
# 	'''ArgumentList_1 : ArgumentList
# 				   | epsilon'''

# def p_EnumConstantList_1(p):
# 	'''EnumConstantList_1 : EnumConstantList
# 					   | epsilon'''
# def p_comma_1(p):
# 	'''comma_1 : COMMA 
# 			| epsilon'''

# def p_EnumBodyDeclarations_1(p):
# 	'''EnumBodyDeclarations_1 : EnumBodyDeclarations 
# 						   | epsilon'''

def p_EnumConstant_1(p):
	'''EnumConstant_1 : COMMA EnumConstant EnumConstant_1
				   | COMMA EnumConstant'''

def p_InterfaceModifier_1(p):
	'''InterfaceModifier_1 : InterfaceModifier InterfaceModifier_1
						| InterfaceModifier'''

def p_InterfaceMemberDeclaration_1(p):
	'''InterfaceMemberDeclaration_1 : InterfaceMemberDeclaration InterfaceMemberDeclaration_1
									| InterfaceMemberDeclaration'''

def p_ConstantModifier_1(p):
	'''ConstantModifier_1 : ConstantModifier ConstantModifier_1
						| ConstantModifier'''

def p_InterfaceMethodModifier_1(p):
	'''InterfaceMethodModifier_1 : InterfaceMethodModifier InterfaceMethodModifier_1
								| InterfaceMethodModifier'''

def p_AnnotationTypeMemberDeclaration_1(p):
	'''AnnotationTypeMemberDeclaration_1 : AnnotationTypeMemberDeclaration AnnotationTypeMemberDeclaration_1
										| AnnotationTypeMemberDeclaration'''

def p_AnnotationTypeElementModifier_1(p):
	'''AnnotationTypeElementModifier_1 : AnnotationTypeElementModifier AnnotationTypeElementModifier_1
									| AnnotationTypeElementModifier'''

def p_ElementValuePair_1(p):
	'''ElementValuePair_1 : COMMA ElementValuePair ElementValuePair_1
					   | COMMA ElementValuePair'''

def p_ElementValue_1(p):
	'''ElementValue_1 : COMMA ElementValue ElementValue_1
				   | COMMA ElementValue'''

def p_BlockStatement_1(p):
	'''BlockStatement_1 : BlockStatement BlockStatement_1
					 | BlockStatement'''

def p_SwitchBlockStatementGroup_1(p):
	'''SwitchBlockStatementGroup_1 : SwitchBlockStatementGroup SwitchBlockStatementGroup_1
								| SwitchBlockStatementGroup'''

def p_SwitchLabel_1(p):
	'''SwitchLabel_1 : SwitchLabel SwitchLabel_1
				  | SwitchLabel'''

def p_StatementExpression_1(p):
	'''StatementExpression_1 : COMMA StatementExpression StatementExpression_1
						  | COMMA StatementExpression'''

def p_CatchClause_1(p):
	'''CatchClause_1 : CatchClause CatchClause_1
				  | CatchClause'''

def p_ClassType_1(p):
	'''ClassType_1 : OR ClassType ClassType_1
				| OR ClassType'''

def p_Resource_1(p):
	'''Resource_1 : SEMICOLON Resource Resource_1
			   | SEMICOLON Resource'''

def p_Annotation_2(p):
	'''Annotation_2 : Annotation_1 LEFT_SQUARE RIGHT_SQUARE Annotation_2
				 | LEFT_SQUARE RIGHT_SQUARE Annotation_2
				 | LEFT_SQUARE RIGHT_SQUARE
				 | Annotation_1 LEFT_SQUARE RIGHT_SQUARE'''

def p_VariableInitializer_2(p):
	'''VariableInitializer_2 : COMMA VariableInitializer VariableInitializer_2
						  | COMMA VariableInitializer'''

def p_AnnotationIdentifierMany(p):
	'''AnnotationIdentifierMany : DOT Annotation_1 Identifier AnnotationIdentifierMany
			| DOT Annotation_1 Identifier
			| DOT Identifier AnnotationIdentifierMany
			| DOT Identifier'''

def p_COMMAExpressionMany(p):
	'''COMMAExpressionMany : COMMA Expression COMMAExpressionMany
			| COMMA Expression'''

def p_DimExprMany(p):
	'''DimExprMany : DimExpr DimExprMany
			| DimExpr'''

def p_COMMAIdentifierMany(p):
	'''COMMAIdentifierMany : COMMA Identifier COMMAIdentifierMany 
			| COMMA Identifier'''

# def p_ClassBodyZeroOne(p):
# 	'''ClassBodyZeroOne : ClassBody
# 			|  epsilon'''

# def p_TypeArgumentsOrDiamondZeroOne(p):
# 	'''TypeArgumentsOrDiamondZeroOne : TypeArgumentsOrDiamond
# 			| epsilon'''

# def p_DimsZeroOne(p):
# 	'''DimsZeroOne : Dims
# 			| epsilon'''

def p_EnumConstantModifier_1(p):
	'''EnumConstantModifier_1 : EnumConstantModifier EnumConstantModifier_1
							| EnumConstantModifier'''

def p_SquareBracket(p):
	'''SquareBracket : LEFT_SQUARE RIGHT_SQUARE SquareBracket
			| LEFT_SQUARE RIGHT_SQUARE'''


# ##########
def p_epsilon(p):
	'''epsilon : '''
	p[0] = ""

def p_error(p):
	print("Syntax error: ", p)
	exit()


parser = yacc.yacc()
s = data
result = parser.parse(s, debug = True)
# result = parser.parse(s)