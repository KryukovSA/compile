import ply.lex as lex
import ply.yacc as yacc
# import llvmlite.ir as ir
# from llvmlite.ir import DoubleType, Function, GlobalVariable
# from llvmlite.binding import Target

from lex_analyzer.generate_code import generate_assembly_code

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
             'LETTER',  # Буквы
             'NUMBER',  # Числа
             'ASSIGN',  # Оператор :=
             'OPERATOR',  # Операторы =, <, >, <=, >=
             'SEMICOLON',  # Точка с запятой
             'COLON',  # двоеточие
             'ADD',  # Операторы +, -
             'MUL',  # Операторы *, /
             'LPAREN',  # Левая скобка (
             'RPAREN',  # Правая скобка )
             'ID',  # Идентификаторы
             'STRING',  # Строковые литералы
             'COMMA',  # Запятая
             'UNDERSCORE',  # нижнее подчеркивание
             'EXPONENT',  # экспонента
         ] + list(keywords.values())

# Регулярные выражения для токенов
# t_NUMBER = r'\d+'
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

# t_ID = r'[a-zA-Z_][a-zA-Z0-9_]*'  # Идентификатор
# t_STRING = r'"[^"]*"'  # Строковый литерал в двойных кавычках
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


# _____________________________-________________________________________________________-
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


def p_number(p):  # так то нумбер ранее есть
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
        p[0] = Node('SIMPLE_EXPRESSION', [p[1], p[3]])  # исправил


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
    elif len(p) == 5:  # для STRING иначе не видит
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
#______________________________________________________________________________________________



# Генератор кода ассемблера
def generate_assembly_code_final(node):
    if node.type == 'PROGRAM':
        code = "section .text\n"
        code += "global _start\n"
        code += "_start:\n"
        for child in node.children:
            code += generate_assembly_code_final(child)
        # Генерация завершения программы
        code += "mov eax, 1\n"
        code += "int 0x80\n"
        return code

    elif node.type == 'PROGRAM_HEADING':
        return ""

    elif node.type == 'PROGRAM_BLOCK':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'IDENTIFIER':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        identifier_name = node.value
        code = f"    ; Load value of identifier: {identifier_name}\n"
        code += f"mov eax, [{identifier_name}]\n"
        return code
    elif node.type == 'BLOCK':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code

    elif node.type == 'DECLARATIVE_PART':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'DECLARATION_GROUP':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'CONSTANT_DEFINITION_GROUP':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'CONSTANT_DEFINITION':
        code = ""
        identifier = node.children[0].value
        constant_value = node.children[1].value
        for child in node.children:#---------------------------
            code += generate_assembly_code_final(child)#--------------------------
        code +=f"{identifier} equ {constant_value}\n"
        return code
    #типо IDENTIFIER его дублирует
    elif node.type == 'ID':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        identifier_name = node.value
        code = f"    ; Load value of identifier: {identifier_name}\n"
        code += f"mov eax, [{identifier_name}]\n"
        return code
    elif node.type == 'NUMBER':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        number_value = node.value
        code = f"    ; Load constant number value\n"
        code += f"    mov eax, {number_value}\n"
        return code
    elif node.type == 'VARIABLE_DECLARATION_GROUP':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    # elif node.type == 'VARIABLE_DECLARATION':
    #     code = ""
    #     for child in node.children:
    #         code += generate_assembly_code_final(child)

    elif node.type == 'VARIABLE_DECLARATION':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        for var_node in node.children[0].children:
            var_name = var_node.value
        code += f'{var_name} resd 1\n'
        return code
    elif node.type == 'IDENTIFIER_LIST':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code

    elif node.type == 'TYPE_DENOTER':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'TYPE_IDENTIFIER':
        asm_code = ""
        type_identifier = node.value
        asm_code += f"    ; Load type identifier: {type_identifier}\n"
        asm_code += f"    mov eax, {type_identifier} ; Move type identifier into EAX register\n"
        return asm_code




    elif node.type == 'STATEMENT_PART':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'COMPOUND_STATEMENT':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'STATEMENT_SEQUENCE':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'STATEMENT':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'SIMPLE_STATEMENT':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'ASSIGNMENT_STATEMENT':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'ASSIGNMENT_STATEMENT_LHS':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'VARIABLE_ACCESS':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    elif node.type == 'ENTIRE_VARIABLE':
        code = ""
        for child in node.children:
            code += generate_assembly_code_final(child)
        return code
    #вызывает дублирование
    elif node.type == 'VARIABLE_IDENTIFIER':
        asm_code = ""

        variable_name = node.value
        asm_code += f"    mov eax, [{variable_name}] \n"
        return asm_code
    elif node.type == 'EXPRESSION':
        asm_code = ""
        for child in node.children:
            asm_code += generate_assembly_code_final(child)
        return asm_code
    elif node.type == 'SHIFT_EXPRESSION':
        asm_code = ""
        for child in node.children:
            asm_code += generate_assembly_code_final(child)
        return asm_code
    elif node.type == 'SIMPLE_EXPRESSION':
        asm_code = ""
        for child in node.children:
            asm_code += generate_assembly_code_final(child)
        return asm_code
    elif node.type == 'TERM':
        asm_code = ""
        for child in node.children:
            asm_code += generate_assembly_code_final(child)
        return asm_code
    elif node.type == 'FACTOR':
        asm_code = ""
        for child in node.children:
            asm_code += generate_assembly_code_final(child)
        return asm_code
    elif node.type == 'PROCEDURE_STATEMENT':
        asm_code = ""
        for child in node.children:
            asm_code += generate_assembly_code_final(child)
        return asm_code
    elif node.type == 'PROCEDURE_IDENTIFIER':
        identifier = node.value
        # Генерация ассемблерного кода для вызова процедуры по идентификатору
        asm_code = "    ; Call procedure {}\n".format(identifier)
        asm_code += "    call {}\n".format(identifier)

        return asm_code
    elif node.type == 'WRITE_PARAMETER_LIST':
        asm_code = ""
        for child in node.children:
            asm_code += generate_assembly_code_final(child)
        return asm_code
    elif node.type == 'WRITE_PARAMETER':
        asm_code = ""
        expression = node.children[0]
        asm_code += generate_assembly_code_final(expression)
        # Ассемблерный код для вызова функции вывода значения
        asm_code += "; Output value\n"
        asm_code += "push eax  ; Сохранить значение в регистре eax на стеке\n"
        asm_code += "call print ; Вызов функции вывода числа\n"
        asm_code += "add esp, 4  ; Очистить аргумент из стека (4 байта)\n"
        return asm_code
    elif node.type == 'WHILE_STATEMENT':
        asm_code = ""
        # Генерация оператора цикла while
        condition_code = generate_assembly_code(node.children[0])  # Генерация кода для условия
        loop_code = generate_assembly_code(node.children[1])  # Генерация кода для тела цикла

        loop_start_label = f"while_loop_start_{node.line}"  # Метка начала цикла
        loop_end_label = f"while_loop_end_{node.line}"  # Метка конца цикла

        asm_code += f"{loop_start_label}:\n"
        asm_code += condition_code
        asm_code += "    cmp eax, 0  ; Сравнение результата условия с нулем\n"
        asm_code += f"    je {loop_end_label}  ; Переход в конец цикла, если условие не выполнилось\n"
        asm_code += loop_code
        asm_code += f"    jmp {loop_start_label}  ; Переход в начало цикла\n"
        asm_code += f"{loop_end_label}:\n"
        return asm_code
    if node.type == "MULTIPLYING_OPERATOR":
        #operator = node.value
        operator = '*'
        print(operator)
        asm_code = ""

        if operator == "*":
            asm_code = "; Multiply operation for floating-point numbers\n"
            asm_code += "fld dword [esp+4]  ; Load the second operand from the stack\n"
            asm_code += "fld dword [esp]  ; Load the first operand from the stack\n"
            asm_code += "fmul  ; Multiply ST0 with ST1 and store result in ST0\n"
            asm_code += " add esp, 4  ; Adjust the stack pointer to remove one operand\n"
        # Обработка других операторов умножения, если необходимо

        return asm_code


    else:
        return ""

























#------------------------------------------------------------------------------------
if __name__ == "__main__":
    # FOR TEST ALL PROGRAM
    code = ''' program Test;
        const PI = 3.14;
        var radius: real;
        begin
            radius := 1.5;
            write( PI * radius)
        end.'''
    #FOR TEST LEXIC AND SYNTAX ANALIZER
    # code = '''
    #     program Test;
    #     const PI = 3.14;
    #     var radius: real;
    #     begin
    #         radius := 2.5;
    #         if radius = 2.5 then
    #             radius := 1.5
    #         else radius := 3;
    #         while radius > 0 do
    #             begin
    #                radius := radius - 0.1;
    #                radius := radius - 0.21;
    #                write( PI * radius * radius)
    #
    #             end;
    #     end.
    # '''

    lexer = lex.lex()
    lexer.input(code)
    print("результат лексического анализа")

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok.type, tok.value)  # результат лексера

    # Создаем синтаксический анализатор
    parser = yacc.yacc()
    result = parser.parse(code, lexer=lexer)  # это дерево, лексер оъявлен выше
    print(result)

    # Вывод дерева АСД
    print("Abstract Syntax Tree:")
    print_ast(result)


    # Генерация кода
    assembly_code = generate_assembly_code_final(result)
    print("Generated Assembly Code:")
    print(assembly_code)

  # # Генерация кода OLD
  #   assembly_code = generate_assembly_code(result)
  #   print("Generated Assembly Code OLD:")
  #   print(assembly_code)