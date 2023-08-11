import ply.lex as lex

# Класс для представления токенов
class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

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
] + list(keywords.values())

# Регулярные выражения для токенов
t_ASSIGN = r':='
t_OPERATOR = r'<=|>=|<|>|='
t_SEMICOLON = r';'
t_COLON = r':'
t_ADD = r'\+|-'
t_MUL = r'\*|/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
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

lexer = lex.lex()

# Пример использования лексического анализатора с циклом while
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
                writeln('Area = ', PI * radius * radius);
                radius := radius - 0.1;
            end;
        end.
    '''

    lexer.input(code)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok.type, tok.value)
