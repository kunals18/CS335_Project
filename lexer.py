#!/usr/bin/python
import sys
import ply.lex as lex
from ply.lex import TOKEN

class Lex(object):

	keywords = {'ABSTRACT', 'CONTINUE', 'FOR', 'NEW', 'SWITCH', 'ASSERT', 'DEFAULT', 'IF', 'PACKAGE', 'SYNCHRONIZED', 'BOOLEAN', 'DO', 'GOTO', 'PRIVATE', 'THIS', 'BREAK', 'DOUBLE', 'IMPLEMENTS', 'PROTECTED', 'THROW', 'BYTE', 'ELSE', 'IMPORT', 'PUBLIC', 'THROWS', 'CASE', 'ENUM', 'INSTANCEOF', 'RETURN', 'TRANSIENT', 'CATCH', 'EXTENDS', 'INT', 'SHORT', 'TRY', 'CHAR', 'FINAL', 'INTERFACE', 'STATIC', 'VOID', 'CLASS', 'FINALLY', 'LONG', 'STRICTFP', 'VOLATILE', 'CONST', 'FLOAT', 'NATIVE', 'SUPER', 'WHILE'}
	# keywords = ["abstract","assert","boolean","break","byte","catch","case","char","const","continue","class","default","do","double","else","enum","extends","final","finally","float","for","goto","if","import","implements","int","interface","insatnceof","long","native","new","package","private","public","protected","return","short","static","switch","strictfp","super","synchronized","this","throw","throws","transient","try","volatile","void","while"]
	operators = {'ASSIGN', 'GREATER', 'LESSER', 'NOT', 'BIT_WISE_NOT', 'QUESTIONMARK', 'COLON', 'LEFTARROW', 'EQUALS', 'GREATER_EQUALS', 'LESS_EQUALS', 'NOT_ASSIGN', 'LOGICAL_AND', 'LOGICAL_OR', 'INC', 'DEC', 'PLUS', 'MINUS', 'STAR', 'DIVIDE', 'AND', 'OR', 'XOR', 'MOD', 'LSHIFT', 'RSHIFT', 'UNRSHIFT', 'PLUS_ASSIGN', 'MINUS_ASSIGN', 'STAR_ASSIGN', 'DIVIDE_ASSIGN', 'AND_ASSIGN', 'OR_ASSIGN', 'XOR_ASSIGN', 'MOD_ASSIGN', 'LSHIFT_ASSIGN', 'RSHIFT_ASSIGN', 'UNSIGN_RSHIFT_ASSIGN'}
	separators = {'LEFT_PAREN', 'RIGHT_PAREN', 'LEFT_CURL', 'RIGHT_CURL', 'LEFT_SQUARE', 'RIGHT_SQUARE', 'SEMICOLON', 'COMMA', 'DOT', 'VARARGS', 'ATTHERATE', 'DOUBLECOLON'}
	types = {'INTEGER', 'FLOATING', 'OCTAL','CHARACTER', 'HEX', 'STRING', 'BOOL'}

	reserved = {'false':'FALSE', 'true':'TRUE', 'null':'NULL'}

	for r in keywords:
		reserved[r.lower()] = r

	identity = {'IDENTIFIER'}
	comments = {'COMMENT'}

	tokens =  list(comments) + list(operators) + list(separators) + list(types) + list(identity)  + list(reserved.values())

	# ignored character
	t_ignore = ' \t'

	# operators
	t_ASSIGN = r'='
	t_GREATER = r'>'
	t_LESSER = r'<'
	t_NOT = r'!'
	t_BIT_WISE_NOT = r'~'
	t_QUESTIONMARK = r'\?'
	t_COLON = r':'
	t_LEFTARROW = r'->'
	t_EQUALS = r'=='
	t_GREATER_EQUALS = r'>='
	t_LESS_EQUALS = r'<='
	t_NOT_ASSIGN = r'!='
	t_LOGICAL_AND = r'&&'
	t_LOGICAL_OR = r'\|\|'
	t_INC = r'\+\+'
	t_DEC = r'--'
	t_PLUS = r'\+'
	t_MINUS = r'-'
	t_STAR = r'\*'
	t_DIVIDE = r'/'
	t_AND = r'&'
	t_OR = r'\|'
	t_XOR = r'\^'
	t_MOD = r'%'
	t_LSHIFT = r'<<'
	t_RSHIFT = r'>>'
	t_UNRSHIFT = r'>>>'
	t_PLUS_ASSIGN = r'\+='
	t_MINUS_ASSIGN = r'-='
	t_STAR_ASSIGN = r'\*='
	t_DIVIDE_ASSIGN = r'/='
	t_AND_ASSIGN = r'&='
	t_OR_ASSIGN = r'\|='
	t_XOR_ASSIGN = r'\^='
	t_MOD_ASSIGN = r'%='
	t_LSHIFT_ASSIGN = r'<<='
	t_RSHIFT_ASSIGN = r'>>='
	t_UNSIGN_RSHIFT_ASSIGN = r'>>>='

	# seperators
	t_LEFT_PAREN = r'\('
	t_RIGHT_PAREN = r'\)'
	t_LEFT_CURL = r'\{'
	t_RIGHT_CURL = r'\}'
	t_LEFT_SQUARE = r'\['
	t_RIGHT_SQUARE = r'\]'
	t_SEMICOLON = r';'
	t_COMMA = r','
	t_DOT = r'\.'
	t_VARARGS = r'\.\.\.'
	t_ATTHERATE = r'@'
	t_DOUBLECOLON = r'::'

	# Integer Literal
	decimal_int_literal = "(0|([1-9][0-9]*))"
	hex_int_literal = "0[Xx][0-9a-fA-F]+"
	octal_int_literal = "(0[0-7]*)"	

	# Floating Point Literal
	float_literal = "[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?"

	# String Literal
	string_literal = """("[^"]*")"""

	t_CHARACTER = r'\'[^\']\''

	bool_literal =  "true|false"

	identifier_lit = "[a-zA-Z_][a-zA-Z_0-9]*"

	@TOKEN(identifier_lit)
	def t_IDENTIFIER(self,t):
		t.type = self.reserved.get(t.value, 'IDENTIFIER')
		return t

	@TOKEN(string_literal)
	def t_STRING(self,t):
	    t.value = t.value[1:-1]
	    return t

	@TOKEN(hex_int_literal)
	def t_HEX(self,t):
		t.value = int(t.value, 16)
		return t

	@TOKEN(octal_int_literal)
	def t_OCTAL(self,t):
	    t.value = int(t.value, 8)
	    return t

	@TOKEN(float_literal)
	def t_FLOATING(self,t):
	    t.value = float(t.value)
	    return t

	@TOKEN(decimal_int_literal)
	def t_INTEGER(self,t):
	    t.value = int(t.value)
	    return t

	@TOKEN(bool_literal)
	def t_BOOL(self,t):
		return t.value

	# Comments
	def t_COMMENT(self,t):
		r'(/\*)(.|\n|\r)*(\*\/)|//(.)*(\n)'

	# track line number
	def t_newline(self, t):
		r'\n+'
		t.lexer.lineno += len(t.value)

    # error handler
	def t_error(self, t):
		print("Illegal character '%s'" % t.value[0])
		t.lexer.skip(1)

    # Build the lexer
	def build(self, **kwargs):
		self.lexer = lex.lex(module=self, **kwargs)

    # print the lexeme, token and count
	def print_lex(self, data):
		self.lexer.input(data)

		lex_tok = {}
		lex_count = {}

		while True:
			tok = self.lexer.token()
			if tok:
				# print(tok.type, tok.value)
				print(tok)
				if tok.type in self.keywords:
					lex_tok[tok.value] = 'Keyword'
				elif tok.type in self.operators:
					lex_tok[tok.value] = 'Operator'
				elif tok.type in self.separators:
					lex_tok[tok.value] = 'Separator'
				elif tok.type in self.types:
					lex_tok[tok.value] = 'Literal'
				else:
					lex_tok[tok.value] = tok.type

				lex_count[tok.value] = lex_count.get(tok.value, 0) + 1
			else:
				break

		# for i in lex_tok:
		# 	print("%s, %s, %d" % (i, lex_tok[i], lex_count[i]))

Lexeme = Lex()
Lexeme.build()

javafile = sys.argv[1]
f = open(javafile, 'r')
data = f.read()
# print(data)
Lexeme.print_lex(data)

