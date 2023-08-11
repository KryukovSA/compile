import ply.yacc as yacc



# Указываем операторы с наименьшим и наибольшим приоритетом
precedence = (
    ('left', 'ADD'),
    ('left', 'MUL'),
)

# Класс для представления узлов AST
class Node:
    def __init__(self, type, children=None, leaf=None):
        self.type = type
        self.children = children
        self.leaf = leaf

# Правила грамматики для синтаксического анализатора
def p_program(p):
    '''program : PROGRAM ID SEMICOLON program_block END_DOT'''
    p[0] = Node('program', children=[p[2], p[4]])

def p_program_block(p):
    '''program_block : program_heading SEMICOLON program_declarations compound_statement'''
    p[0] = Node('program_block', children=[p[1], p[3], p[4]])

def p_program_heading(p):
    '''program_heading : PROGRAM ID'''
    p[0] = Node('program_heading', leaf=p[2])

def p_program_declarations(p):
    '''program_declarations : const_declarations type_declarations var_declarations
                            | const_declarations type_declarations
                            | const_declarations var_declarations
                            | type_declarations var_declarations
                            | const_declarations
                            | type_declarations
                            | var_declarations
                            | empty'''
    p[0] = Node('program_declarations', children=p[1:])

def p_const_declarations(p):
    '''const_declarations : CONST const_list'''
    p[0] = Node('const_declarations', children=[p[2]])

def p_const_list(p):
    '''const_list : const_list constant_definition SEMICOLON
                  | constant_definition SEMICOLON'''
    if len(p) == 4:
        p[0] = Node('const_list', children=[p[1], p[2]])
    else:
        p[0] = Node('const_list', children=[p[1]])

def p_constant_definition(p):
    '''constant_definition : ID OPERATOR constant'''
    p[0] = Node('constant_definition', children=[p[1], p[3]])

def p_constant(p):
    '''constant : NUMBER
                | STRING'''
    p[0] = Node('constant', leaf=p[1])

# Другие правила грамматики здесь...

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print(f"Синтаксическая ошибка: Неожиданный символ '{p.value}' на позиции {p.lexpos}")

# Создаем парсер
parser = yacc.yacc()

# Пример использования синтаксического анализатора
if __name__ == "__main__":
    code = '''
        program Test;
        const
            PI = 3.14;
        var
            radius: real;
        begin
            radius := 2.5;
            while radius > 0 do
            begin
                writeln('Area = ', PI * radius * radius);
                radius := radius - 0.1;
            end;
        end.
    '''

    result = parser.parse(code)
    print(result)
