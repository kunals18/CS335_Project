import sys
import ply.lex as lex
from ply.lex import TOKEN
import ply.yacc as yacc


def p_Identifier(p):
	'''Identifier : Identifier_chars'''

def p_Identifier_chars(p):
	'''Identifier_chars : JavaLetter JavaLetterOrDigit
						| JavaLetter'''

def p_Literal(p):
	'''Literal : IntegerLiteral
				| FloatingPointLiteral
				| BooleanLiteral
				| CharacterLiteral
				| StringLiteral
				| NullLiteral'''

def p_Type(p):
	'''Type: PrimitiveType 
		| ReferenceType'''

#prods from 6'''

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

def PackageName(p):
	'''PackageName : Identifier
				| PackageName DOT Identifier'''

def AmbiguousName(p):
	'''AmbiguousName : Identifier
					| AmbiguousName DOT Identifier'''

#prod from 8

def p_ClassDeclaration(p):
	'''ClassDeclaration : NormalClassDeclaration
					 | EnumDeclaration'''

def p_NormalClassDeclaration(p):
	'''NormalClassDeclaration : ClassModifier_1 CLASS Identifier TypeParameters_1 Superclass_1 Superinterfaces_1 ClassBody'''

def p_ClassModifier_1(p):
	'''ClassModifier_1 : ClassModifier ClassModifier_1 
					| epsilon'''

def p_TypeParameters_1(p):
	'''TypeParameters_1 : TypeParameters
					 | epsilon'''

def p_ClassModifier(p):
	'''ClassModifier : Annotation
				  | PUBLIC	 
				  | PROTECTED
				  | PRIVATE 
				  | ABSTRACT
				  | STATIC
				  | FINAL
				  | STRICTFP'''

def Superclass_1(p):
	'''Superclass_1 : Superclass 
				 | epsilon'''

def Superinterfaces_1(p):
	'''Superinterfaces_1 : Superinterfaces
					  | epsilon
'''

def TypeParameterList(p):
	'''TypeParameterList : TypeParameter TypeParameter_1'''

def TypeParameter_1(p):
	'''TypeParameter_1 : COMMA TypeParameter TypeParameter_1
					| epsilon'''

def p_Superclass(p):
	'''Superclass : extends ClassType'''

def p_Superinterfaces(p):
	'''Superinterfaces : implements InterfaceTypeList	'''

def p_InterfaceTypeList(p):
	'''InterfaceTypeList : InterfaceType InterfaceType_1'''

def p_InterfaceType_1(p):
	'''InterfaceType_1 : COMMA InterfaceType Interface_1
					| epsilon'''

def p_ClassBody(p):
	'''ClassBody : LEFT_CURL ClassBodyDeclaration_1 RIGHT_CURL'''

def ClassBodyDeclaration_1(p):
	'''ClassBodyDeclaration_1 : ClassBodyDeclaration ClassBodyDeclaration_1 
							| epsilon'''

def ClassBodyDeclaration(p):
	'''ClassBodyDeclaration : ClassMemberDeclaration
						 | InstanceInitializer
						 | StaticInitializer
						 | ConstructorDeclaration'''

def ClassMemberDeclaration(p):
	'''ClassMemberDeclaration : FieldDeclaration
						   | MethodDeclaration
						   | ClassDeclaration
						   | InterfaceDeclaration
						   | COMMA'''

def FieldDeclaration(p):
	'''FieldDeclaration : FieldModifier_1 UnannType VariableDeclaratorList COMMA'''

def FieldModifier_1(p):
	'''FieldModifier_1 : FieldModifier FieldModifier_1
					| epsilon'''

def FieldModifier(p):
	'''FieldModifier : Annotation 
				  | PUBLIC
				  |	PROTECTED 
				  |	PRIVATE
				  |	STATIC
				  |	FINAL
				  |	TRANSIENT 
				  |	VOLATILE'''

def VariableDeclaratorList(p):
	'''VariableDeclaratorList : VariableDeclarator VariableDeclarator_1'''

def VariableDeclarator_1(p):
	'''VariableDeclarator_1 : COMMA VariableDeclarator VariableDeclarator_1
						 | epsilon
''' 
def VariableDeclarator(p):
	'''VariableDeclarator : VariableDeclaratorId VariableInitializer_1'''

def VariableInitializer_1(p):
	'''VariableInitializer_1 : EQUAL VariableInitializer 
						  | epsilon'''

def VariableDeclaratorId(p):
	'''VariableDeclaratorId : Identifier Dims 
						 | Identifier'''

def VariableInitializer(p):
	'''VariableInitializer : Expression 
						| ArrayInitializer'''

def UnannType(p):
	'''UnannType : UnannPrimitiveType
			  | UnannReferenceType'''

def UnannPrimitiveType(p):
	'''UnannPrimitiveType : NumericType
					   | BOOLEAN'''

def UnannReferenceType(p):
	'''UnannReferenceType : UnannClassOrInterfaceType
					   | UnannTypeVariable
					   | UnannArrayType'''

def UnannClassOrInterfaceType(p):
	'''UnannClassOrInterfaceType : UnannClassType
							  | UnannInterfaceType'''

def UnannClassType(p):
	'''UnannClassType : Identifier TypeArguments_1
	UnannClassOrInterfaceType DOT Annotation_1 Identifier TypeArguments_1'''

def TypeArguments_1(p):
	'''TypeArguments_1 : TypeArguments 
					| epsilon'''

def Annotation_1(p):
	'''Annotation_1 : Annotation Annotation_1 
				 | epsilon'''

def UnannInterfaceType(p):
	'''UnannInterfaceType : UnannClassType'''

def UnannTypeVariable(p):
	'''UnannTypeVariable : Identifier'''

def UnannArrayType(p):
	'''UnannArrayType : UnannPrimitiveType Dims
				   | UnannClassOrInterfaceType Dims
				   | UnannTypeVariable Dims'''

def MethodDeclaration(p):
	'''MethodDeclaration : MethodModifier_1 MethodHeader MethodBody'''

def MethodModifier_1(p):
	'''MethodModifier_1 : MethodModifier MethodModifier_1 | epsilon'''

def MethodModifier(p):
	'''MethodModifier : Annotation
				   | PUBLIC
				   | PROTECTED
				   | PRIVATE
				   | ABSTRACT 
				   | STATIC
				   | FINAL
				   | SYNCHRONISED
				   | NATIVE
				   | STRICTF'''P
def MethodHeader(p):
	'''MethodHeader : Result MethodDeclarator Throws 
				 | Result MethodDeclarator
				 | TypeParameters Annotation_1 Result MethodDeclarator Throws
				 | TypeParameters Annotation_1 Result MethodDeclarator'''

def Result(p):
	'''Result : UnannType
	       | void'''

def MethodDeclarator(p):
	'''MethodDeclarator : Identifier LEFT_PARAN FormalParameterList_1 RIGHT_PARAN Dims'''

def FormalParameterList_1(p):
	'''FormalParameterList_1 : FormalParameterList
						  | epsilon'''

def FormalParameterList(p):
	'''FormalParameterList : ReceiverParameter
						| FormalParameters COMMA LastFormalParameter
						| LastFormalParameter'''

def FormalParameters(p):
	'''FormalParameters : FormalParameter FormalParameter_1 
					 | ReceiverParameter FormalParameter_1'''

def FormalParameter_1(p):
	'''FormalParameter_1 : COMMA FormalParameter FormalParameter_1 
					  | epsilon'''

def FormalParameter(p):
	'''FormalParameter : VariableModifier_1 UnannType VariableDeclaratorId'''

def VariableModifier_1(p):
	'''VariableModifier_1 : VariableModifier VariableModifier_1 
					   | epsilon'''

def VariableModifier(p):
	'''VariableModifier : Annotation
					 | FINAL'''

def LastFormalParameter(p):
	'''LastFormalParameter : VariableModifier_1  UnannType Annotation_1 DOT DOT DOT  VariableDeclaratorId
						| FormalParameter'''

def ReceiverParameter(p):
	'''ReceiverParameter : Annotation_1 UnannType  Identifier_1 THIS'''

def Identifier_1(p):
	'''Identifier_1 : Identifier DOT Identifier_1 
				 | epsilon
'''	
def Throws(p):
	'''Throws : THROWS ExceptionTypeList'''

def ExceptionTypeList(p):
	'''ExceptionTypeList : ExceptionType ExceptionType_1'''

def ExceptionType_1(p):
	'''ExceptionType_1 : COMMA ExceptionType ExceptionType_1
					| epsilon'''

def ExceptionType(p):
	'''ExceptionType : ClassType
				  |	TypeVariable'''

def MethodBody(p):
	'''MethodBody : Block
			   | SEMIC_COLON'''

def InstanceInitializer(p):
	'''InstanceInitializer : Block'''

def StaticInitializer(p):
	'''StaticInitializer : STATIC Block'''

def ConstructorDeclaration(p):
	'''ConstructorDeclaration : ConstructorModifier_1 ConstructorDeclarator Throws_1 ConstructorBody'''

def Throws_1(p):
	'''Throws_1 : Throws 
			 | epsilon
''' 
def ConstructorModifier_1(p):
	'''ConstructorModifier_1 : ConstructorModifier ConstructorModifier_1
						  | epsilon'''

def ConstructorModifier(p):
 '''	ConstructorModifier : Annotation
 						| PUBLIC
 						| PROTECTED
 						| PRIVATE'''

def ConstructorDeclarator(p):
	'''ConstructorDeclarator : TypeParameters_1 SimpleTypeName LEFT_PARAN FormalParameterList_1 RIGHT_PARAN'''

def SimpleTypeName(p):
	'''SimpleTypeName : Identifier'''

def ConstructorBody(p):
	'''ConstructorBody : LEFT_CURL ExplicitConstructorInvocation_1 BlockStatements_1 RIGHT_CURL'''

def ExplicitConstructorInvocation_1(p):
	'''ExplicitConstructorInvocation_1 : ExplicitConstructorInvocation 
									| epsilon'''

def BlockStatements_1(p):
	'''BlockStatements_1 : BlockStatements 
					  | epsilon'''

def ExplicitConstructorInvocation(p):
	'''ExplicitConstructorInvocation : TypeArguments_1 THIS LEFT_PARAN ArgumentList_1 RIGHT_PARAN SEMIC_COLON
								  | TypeArguments_1 SUPER LEFT_PARAN ArgumentList_1 RIGHT_PARAN SEMIC_COLON
								  | ExpressionName DOT TypeArguments_1 SUPER LEFT_PARAN ArgumentList_1 RIGHT_PARAN SEMIC_COLON
								  | Primary DOT TypeArguments_1 SUPER LEFT_PARAN ArgumentList_1 RIGHT_PARAN SEMIC_COLON'''

def ArgumentList_1(p):
	'''ArgumentList_1 : ArgumentList
				   | epsilon'''

def EnumDeclaration(p):
	'''EnumDeclaration : ClassModifier_1 ENUM Identifier Superinterfaces_1
					| EnumBody'''

def EnumBody(p):
	'''EnumBody : LEFT_CURL EnumConstantList_1 comma_1 EnumBodyDeclarations_1 RIGHT_CURL'''

def EnumConstantList_1(p):
	'''EnumConstantList_1 : EnumConstantList
					   | epsilon'''
def comma_1(p):
	'''comma_1 : COMMA 
			| epsilon'''

def EnumBodyDeclarations_1(p):
	'''EnumBodyDeclarations_1 : EnumBodyDeclarations 
						   | epsilon'''

def EnumConstantList(p):
	'''EnumConstantList : EnumConstant EnumConstant_1'''

def EnumConstant_1(p):
	'''EnumConstant_1 : COMMA EnumConstant EnumConstant_1
				   | epsilon'''

def EnumConstant(p):
	'''EnumConstant : EnumConstantModifier_1 Identifier [( [ArgumentList] )] [ClassBody]'''

def EnumConstantModifier(p):
	'''EnumConstantModifier : Annotation'''

def EnumBodyDeclarations(p):
	'''EnumBodyDeclarations : COMMA ClassBodyDeclaration_1
'''

def Dims(p):
	'''Dims : Annotation_1 LEFT_SQUARE RIGHT_SQUARE Annotation_2 '''

def Annotation_2(p):
	'''Annotation_2 : Annotation_1 LEFT_SQUARE RIGHT_SQUARE Annotation_2
				 | epsilon'''

def TypeParameter(p):
	'''TypeParameter : TypeParameterModifier_1 Identifier TypeBound_1'''

def TypeParameterModifier_1(p):
	'''TypeParameterModifier_1 : TypeParameterModifier TypeParameterModifier_1
							| epsilon'''

def TypeBound_1(p):
	'''TypeBound_1 : TypeBound 
				| epsilon'''

def TypeParameterModifier(p):
	'''TypeParameterModifier : Annotation'''

def TypeBound(p):
	'''TypeBound : EXTENDS TypeVariable
			  | EXTENDS ClassOrInterfaceType AdditionalBound_1'''

def AdditionalBound_1(p):
	'''AdditionalBound_1 : AdditionalBound AdditionalBound_1
					  | epsilon'''

def AdditionalBound(p):
	'''AdditionalBound : AND InterfaceType'''

def TypeArgumentList(p):
	'''TypeArgumentList : TypeArgument TypeArgument_1'''

def TypeArgument_1(p):
	'''TypeArgument_1 : COMMA TypeArgument TypeArgument_1
				   | epsilon'''

def TypeArgument(p):
	'''TypeArgument : ReferenceType
				 | Wildcard'''

def Wildcard(p):
	'''Wildcard : Annotation_1 QUESTION WildcardBounds_1'''

def WildcardBounds_1(p):
	'''WildcardBounds_1 : WildcardBounds 
					 | epsilon'''

def WildcardBounds(p):
	'''WildcardBounds : EXTENDS ReferenceType 
				   | SUPER ReferenceType
