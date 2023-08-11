import ply.lex as lex
import ply.yacc as yacc


# Класс для представления токенов
class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value


# class MyToken:
#     def __init__(self, type, value, lineno, lexpos):
#         self.type = type
#         self.value = value
#         self.lineno = lineno
#         self.lexpos = lexpos

# Список ключевых слов
keywords = {
    'program': 'PROGRAM',
    'const': 'CONST',
    'type': 'TYPE',
    'var': 'VAR',
    'begin': 'BEGIN',
    'end': 'END',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'not': 'NOT',
    'end.': 'END_DOT'
}

# Список токенов
tokens = [
    'LETTER',         # Буквы
    'NUMBER',         # Числа
    'ASSIGN',         # Оператор :=
    'OPERATOR',       # Операторы =, <, >, <=, >=
    'SEMICOLON',      # Точка с запятой
    'COLON',          # двоеточие
    'ADD',            # Операторы +, -
    'MUL',            # Операторы *, /
    'LPAREN',         # Левая скобка (
    'RPAREN',         # Правая скобка )
    'ID',             # Идентификаторы
    'STRING',         # Строковые литералы
    'COMMA',          # Запятая
    'UNDERSCORE',     #нижнее подчеркивание
    'EXPONENT',       #экспонента
] + list(keywords.values())

# Регулярные выражения для токенов
#t_NUMBER = r'\d+'
t_ASSIGN = r':='
t_OPERATOR = r'<=|>=|<|>|='
t_SEMICOLON = r';'
t_COLON = r':'
t_ADD = r'\+|-'
t_MUL = r'\*|/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_UNDERSCORE = r'_'
t_EXPONENT = r'e'

#t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'  # Идентификатор
#t_STRING = r'"[^"]*"'  # Строковый литерал в двойных кавычках
t_COMMA = r','

def t_NUMBER(t):
    r'\d+\.\d+([eE][+-]?\d+)?|\d+[eE][+-]?\d+|\d+'
    t.value = float(t.value) if '.' in t.value or 'e' in t.value.lower() else int(t.value)
    return t

def t_STRING(t):
    r'(\'[^\']*\')|("[^"]*")'
    t.value = t.value[1:-1]
    return t

def t_END_DOT(t):
    r'end\.'
    t.type = keywords.get(t.value, 'END_DOT')
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = keywords.get(t.value, 'ID')
    if t.type == 'ID' and t.value in keywords:
        t.type = keywords[t.value]
    return t

# Пропуск пробелов и табуляций
t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Нераспознанный символ: '{t.value[0]}'")
    t.lexer.skip(1)

#_____________________________-________________________________________________________-
# Класс для представления узлов синтаксического дерева
class Node:
    def __init__(self, type, children=None, value=None):
        self.type = type
        self.children = children if children is not None else []
        self.value = value

# def p_program(p):
#     '''program : program_heading SEMICOLON program_block END_DOT'''
#     p[0] = Node('PROGRAM', [p[1], p[3]])

def p_program(p):
    '''program : program_heading SEMICOLON program_block'''
    p[0] = Node('PROGRAM', [p[1], p[3]])

def p_program_heading(p):
    '''program_heading : PROGRAM identifier
                       | PROGRAM identifier LPAREN program_parameter_list RPAREN'''
    if len(p) == 3:
        p[0] = Node('PROGRAM_HEADING', [Node('PROGRAM'), Node('IDENTIFIER', value=p[2])])
    else:
        p[0] = Node('PROGRAM_HEADING', [Node('PROGRAM'), Node('IDENTIFIER', value=p[2]), p[4]])

def p_program_parameter_list(p):
    '''program_parameter_list : identifier_list'''
    p[0] = Node('PROGRAM_PARAMETER_LIST', [p[1]])

def p_identifier_list(p):
    '''identifier_list : identifier
                       | identifier_list COMMA identifier'''
    if len(p) == 2:
        p[0] = Node('IDENTIFIER_LIST', [p[1]])
    else:
        p[0] = p[1]
        p[0].children.append(p[3])

def p_identifier(p):
    '''identifier : ID
                  | UNDERSCORE
                  | identifier STRING
                  | identifier NUMBER
                  | NUMBER '''
              #    | number''' #не активно
    if len(p) == 2:
        p[0] = Node('IDENTIFIER', value=p[1])
    else:
        p[0] = p[1]
        p[0].value += p[2]

# def p_letter(p):
#     '''letter : STRING
#               | UNDERSCORE'''
#     p[0] = Node('LETTER', [p[1]])

def p_program_block(p):
    '''program_block : block'''
    p[0] = Node('PROGRAM_BLOCK', [p[1]])

# Определения для других нетерминалов можно продолжить таким же образом
def p_block(p):
    '''block : declarative_part statement_part'''
    if len(p) == 3:
        p[0] = Node('BLOCK', [p[1], p[2]])
    else:
        p[0] = Node('BLOCK', [p[1]])

def p_declarative_part(p):
    '''declarative_part : declaration_group
                        | declarative_part declaration_group'''
    if len(p) == 2:
        p[0] = Node('DECLARATIVE_PART', [p[1]])
    else:
        p[0] = p[1]
        p[0].children.append(p[2])

def p_declaration_group(p):
    '''declaration_group : constant_definition_group
                         | type_definition_group
                         | variable_declaration_group'''
    p[0] = Node('DECLARATION_GROUP', [p[1]])

def p_constant_definition_group(p):
    '''constant_definition_group : CONST constant_definition SEMICOLON
                                 | constant_definition_group constant_definition SEMICOLON'''
    if len(p) == 4:
        p[0] = Node('CONSTANT_DEFINITION_GROUP', [p[2]])
    else:
        p[0] = p[1]
        p[0].children.append(p[2])

def p_constant_definition(p):
    '''constant_definition : ID OPERATOR constant'''
    p[0] = Node('CONSTANT_DEFINITION', [Node('ID', value=p[1]), p[3]])

def p_constant(p):
    '''constant : number
                | sign number
                | sign real_number'''
    if len(p) == 3:
        p[0] = Node('CONSTANT', [p[1], p[2]])
    else:
        p[0] = p[1]

def p_sign(p):
    '''sign : ADD
            | MUL'''
    p[0] = Node('SIGN', value=p[1])

def p_number(p):# так то нумбер ранее есть
    '''number : decimal_integer
              | sign decimal_integer'''
    if len(p) == 2:
        p[0] = Node('NUMBER', value=p[1])
    else:
        p[0] = Node('NUMBER', value=p[1] + p[2])

def p_decimal_integer(p):
    '''decimal_integer : digit_sequence'''
    p[0] = Node('DECIMAL_INTEGER', value=p[1])

def p_digit_sequence(p):
    '''digit_sequence : NUMBER
                      | digit_sequence NUMBER'''
    if len(p) == 2:
        p[0] = Node('DIGIT_SEQUENCE', value=p[1])
    else:
        p[0] = p[1]
        p[0].value += p[2]

def p_real_number(p):
    '''real_number : digit_sequence ADD fractional_part
                   | digit_sequence exponent scale_factor
                   | digit_sequence ADD fractional_part exponent scale_factor'''
    if len(p) == 4:
        p[0] = Node('REAL_NUMBER', [Node('DIGIT_SEQUENCE', value=p[1]), p[3]])
    elif len(p) == 5:
        p[0] = Node('REAL_NUMBER', [Node('DIGIT_SEQUENCE', value=p[1]), p[4]])
    else:
        p[0] = Node('REAL_NUMBER', [Node('DIGIT_SEQUENCE', value=p[1]), p[3], p[5]])

def p_fractional_part(p):
    '''fractional_part : digit_sequence'''
    p[0] = Node('FRACTIONAL_PART', value=p[1])

def p_scale_factor(p):
    '''scale_factor : sign digit_sequence'''
    if len(p) == 2:
        p[0] = Node('SCALE_FACTOR', value=p[1])
    else:
        p[0] = Node('SCALE_FACTOR', value=p[1] + p[2])

def p_exponent(p):
    '''exponent : EXPONENT'''
    p[0] = Node('EXPONENT', value=p[1])

def p_variable_declaration_group(p):
    '''variable_declaration_group : VAR variable_declaration SEMICOLON
                                  | variable_declaration_group variable_declaration SEMICOLON'''
    if len(p) == 4:
        p[0] = Node('VARIABLE_DECLARATION_GROUP', [p[2]])
    else:
        p[0] = p[1]
        p[0].children.append(p[2])

def p_variable_declaration(p):
    '''variable_declaration : identifier_list COLON type_denoter'''
    p[0] = Node('VARIABLE_DECLARATION', [p[1], p[3]])

def p_type_denoter(p):
    '''type_denoter : type_identifier'''
    p[0] = Node('TYPE_DENOTER', [p[1]])

def p_type_identifier(p):
    '''type_identifier : ID'''
    p[0] = Node('TYPE_IDENTIFIER', value=p[1])

def p_type_definition_group(p):
    '''type_definition_group : TYPE type_definition SEMICOLON
                             | type_definition_group type_definition SEMICOLON'''
    if len(p) == 4:
        p[0] = Node('TYPE_DEFINITION_GROUP', [p[2]])
    else:
        p[0] = p[1]
        p[0].children.append(p[2])

def p_type_definition(p):
    '''type_definition : ID ASSIGN type_denoter'''
    p[0] = Node('TYPE_DEFINITION', [Node('ID', value=p[1]), p[3]])

def p_statement_part(p):
    '''statement_part : compound_statement'''
    p[0] = Node('STATEMENT_PART', [p[1]])

# def p_compound_statement(p):
#     '''compound_statement : BEGIN statement_sequence END'''
#     p[0] = Node('COMPOUND_STATEMENT', p[2])

def p_compound_statement(p):
    '''compound_statement : BEGIN statement_sequence END_DOT
    | BEGIN statement_sequence END SEMICOLON'''
    p[0] = Node('COMPOUND_STATEMENT', [p[2]])

def p_statement_sequence(p):
    '''statement_sequence : statement
                          | statement_sequence SEMICOLON statement'''
    if len(p) == 2:
        p[0] = Node('STATEMENT_SEQUENCE', [p[1]])
    else:
        p[0] = p[1]
        p[0].children.append(p[3])

def p_statement(p):
    '''statement : simple_statement
                 | structured_statement
                 '''
    p[0] = Node('STATEMENT', [p[1]])

def p_simple_statement(p):
    '''simple_statement : empty_statement
                        | assignment_statement
                        | procedure_statement'''
    p[0] = Node('SIMPLE_STATEMENT', [p[1]])

def p_structured_statement(p):
    '''structured_statement : compound_statement
                             | conditional_statement
                             | repetitive_statement'''
    p[0] = Node('STRUCTURED_STATEMENT', [p[1]])

def p_empty_statement(p):
    '''empty_statement : SEMICOLON'''
    p[0] = Node('EMPTY_STATEMENT')

def p_assignment_statement(p):
    '''assignment_statement : assignment_statement_lhs ASSIGN expression'''
    p[0] = Node('ASSIGNMENT_STATEMENT', [p[1], p[3]])

def p_assignment_statement_lhs(p):
    '''assignment_statement_lhs : variable_access'''
    p[0] = Node('ASSIGNMENT_STATEMENT_LHS', [p[1]])

def p_file_variable(p):
    '''file_variable : variable_access'''
    p[0] = Node('FILE_VARIABLE', [p[1]])

def p_variable_access(p):
    '''variable_access : entire_variable
                       | component_variable'''
    p[0] = Node('VARIABLE_ACCESS', [p[1]])

def p_entire_variable(p):
    '''entire_variable : variable_identifier'''
    p[0] = Node('ENTIRE_VARIABLE', [p[1]])

def p_variable_identifier(p):
    '''variable_identifier : ID'''
    p[0] = Node('VARIABLE_IDENTIFIER', value=p[1])


def p_component_variable(p):
    '''component_variable : field_designator'''
    p[0] = Node('COMPONENT_VARIABLE', [p[1]])


def p_field_designator(p):
    '''field_designator : field_designator_identifier'''
    p[0] = Node('FIELD_DESIGNATOR', value=p[1])

def p_field_designator_identifier(p):
    '''field_designator_identifier : ID'''
    p[0] = Node('FIELD_DESIGNATOR_IDENTIFIER', value=p[1])


def p_expression(p):
    '''expression : shift_expression
                  | shift_expression relational_operator shift_expression'''
    if len(p) == 2:
        p[0] = Node('EXPRESSION', [p[1]])
    else:
        p[0] = Node('EXPRESSION', [p[1], Node('RELATIONAL_OPERATOR', value=p[2]), p[3]])

def p_relational_operator(p):
    '''relational_operator : OPERATOR'''
    p[0] = Node('RELATIONAL_OPERATOR', value=p[1])

# def p_shift_expression(p):
#     '''shift_expression : simple_expression
#                          | simple_expression shift_operator simple_expression'''
#     if len(p) == 2:
#         p[0] = Node('SHIFT_EXPRESSION', [p[1]])
#     else:
#         p[0] = Node('SHIFT_EXPRESSION', [p[1], Node('SHIFT_OPERATOR', value=p[2]), p[3]])
#
# def p_shift_operator(p):
#     '''shift_operator : SHL
#                       | SHR'''
#     p[0] = Node('SHIFT_OPERATOR', value=p[1])

def p_shift_expression(p):
    '''shift_expression : simple_expression'''
    p[0] = Node('SHIFT_EXPRESSION', [p[1]])

def p_simple_expression(p):
    '''simple_expression : term
                          | term ADD term
                          | term MUL term'''
    if len(p) == 2:
        p[0] = Node('SIMPLE_EXPRESSION', [p[1]])
    else:
        p[0] = Node('SIMPLE_EXPRESSION', [p[1], p[3]])#исправил

def p_term(p):
    '''term : factor
            | factor multiplying_operator factor'''
    if len(p) == 2:
        p[0] = Node('TERM', [p[1]])
    else:
        p[0] = Node('TERM', [p[1], Node('MULTIPLYING_OPERATOR', value=p[2]), p[3]])

def p_factor(p):
    '''factor : sign unsigned_constant
            | number
              | variable_access
              | LPAREN expression RPAREN
              | NOT factor'''
    # if len(p) == 2:
    #     p[0] = Node('FACTOR', value=p[1]) #тоже допустимо
    if len(p) == 2:
        p[0] = Node('FACTOR', [p[1]])
    elif len(p) == 3:
        p[0] = Node('FACTOR', [p[1], p[2]])
    elif len(p) == 4:
        p[0] = Node('FACTOR', [p[2]])
    else:
        p[0] = Node('FACTOR', [p[1], p[2]])

def p_multiplying_operator(p):
    '''multiplying_operator : MUL'''
    p[0] = Node('MULTIPLYING_OPERATOR', value=p[1])

def p_unsigned_constant(p):
    '''unsigned_constant : number
                         | real_number
                         | constant_identifier'''
    p[0] = Node('UNSIGNED_CONSTANT', [p[1]])

def p_constant_identifier(p):
    '''constant_identifier : ID'''
    p[0] = Node('CONSTANT_IDENTIFIER', value=p[1])



def p_conditional_statement(p):
    '''conditional_statement : if_statement'''
    p[0] = Node('CONDITIONAL_STATEMENT', [p[1]])

def p_if_statement(p):
    '''if_statement : IF boolean_expression THEN statement else_part'''
    p[0] = Node('IF_STATEMENT', [p[2], p[4], p[5]])

def p_boolean_expression(p):
    '''boolean_expression : expression'''
    p[0] = Node('BOOLEAN_EXPRESSION', [p[1]])

def p_else_part(p):
    '''else_part : ELSE statement
                 | empty_statement'''
    p[0] = Node('ELSE_PART', [p[2]] if p[2].type != 'EMPTY_STATEMENT' else [])

def p_repetitive_statement(p):
    '''repetitive_statement : while_statement'''
    p[0] = Node('REPETITIVE_STATEMENT', [p[1]])

def p_while_statement(p):
    '''while_statement : WHILE boolean_expression DO statement'''
    p[0] = Node('WHILE_STATEMENT', [p[2], p[4]])

def p_write_parameter(p):
    '''write_parameter : expression
                       | expression COLON expression
                       | expression COLON expression COLON expression'''
    if len(p) == 2:
        p[0] = Node('WRITE_PARAMETER', [p[1]])
    elif len(p) == 4:
        p[0] = Node('WRITE_PARAMETER', [p[1], p[3]])
    else:
        p[0] = Node('WRITE_PARAMETER', [p[1], p[3], p[5]])

def p_write_parameter_list(p):
    '''write_parameter_list : LPAREN write_parameter RPAREN
                            | LPAREN STRING COMMA write_parameter RPAREN
                            | LPAREN file_variable COMMA write_parameter RPAREN
                            | LPAREN file_variable COMMA write_parameter COMMA write_parameter RPAREN'''

    if len(p) == 4:
        p[0] = Node('WRITE_PARAMETER_LIST', [p[2]])
    elif len(p) == 5:  #для STRING иначе не видит
        p[0] = Node('WRITE_PARAMETER_LIST', [p[4]])
    elif len(p) == 6:
        p[0] = Node('WRITE_PARAMETER_LIST', [p[2], p[4]])
    else:
        p[0] = Node('WRITE_PARAMETER_LIST', [p[2], p[4], p[6]])

def p_read_parameter_list(p):
    '''read_parameter_list : LPAREN variable_access RPAREN
                           | LPAREN file_variable COMMA variable_access RPAREN
                           | LPAREN file_variable COMMA variable_access COMMA variable_access RPAREN'''
    if len(p) == 4:
        p[0] = Node('READ_PARAMETER_LIST', [p[2]])
    elif len(p) == 6:
        p[0] = Node('READ_PARAMETER_LIST', [p[2], p[4]])
    else:
        p[0] = Node('READ_PARAMETER_LIST', [p[2], p[4], p[6]])

def p_procedure_statement(p):
    '''procedure_statement : procedure_identifier read_parameter_list
                            | procedure_identifier write_parameter_list'''
    p[0] = Node('PROCEDURE_STATEMENT', [p[1], p[2]])

def p_procedure_identifier(p):
    '''procedure_identifier : ID'''
    p[0] = Node('PROCEDURE_IDENTIFIER', value=p[1])

# Обработка ошибок
def p_error(p):
    if p:
        print(f"Синтаксическая ошибка в символе: {p.value}")
    else:
        print("Синтаксическая ошибка в конце программы")



# Пример функции для построения AST
def build_ast(parser, input_code):
    ast = parser.parse(input_code, lexer=lexer)
    return ast


# Функция для вывода дерева АСД
def print_ast(node, level=0):
    if node is None:
        return
    print("  " * level + str(node.type))
    for child in node.children:
        print_ast(child, level + 1)

#_____________________________-________________________________________________________-











#_____________________________-________________________________________________________-



# Пример использования
if __name__ == "__main__":

    code = '''
        program Test;
        const PI = 3.14;
        var radius: real;
        begin
            radius := 2.5;
            if radius = 2.5 then
                radius := 1.5
            else radius := 3;
            while radius > 0 do
                begin
                   radius := radius - 0.1;
                   radius := radius - 0.21;
                   write( PI * radius * radius)
                end;
        end.
    '''
    # radius := radius - 0.21;
    #                 write( PI * radius * radius);
    lexer = lex.lex()
    lexer.input(code)
    print("результат лексического анализа")

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok.type, tok.value)#результат лексера



    # Создаем синтаксический анализатор
    parser = yacc.yacc()
    result = parser.parse(code, lexer=lexer)#это дерево, лексер оъявлен выше
    print(result)

    # Вывод дерева АСД
    print("Abstract Syntax Tree:")
    print_ast(result)

    # # Создание генератора машинного кода
    # generator = MachineCodeGenerator()
    #
    # # Генерация машинного кода из AST
    # machine_code = generator.generate_code(result)
    #
    # # Вывод сгенерированного машинного кода
    # print("Generated Machine Code:")
    # print(machine_code)
