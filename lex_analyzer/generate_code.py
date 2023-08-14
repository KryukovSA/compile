#Некоторые из написанных, но не использованных кодогенераторов


class MachineCodeGenerator:
    def __init__(self):
        self.code = ""

    def generate_code(self, ast):
        self.code += "section .text\n"
        self.visit(ast)
        return self.code

    def visit(self, node):
        method_name = 'visit_' + node.type
        method = getattr(self, method_name, self.generic_visit)
        return method(node)

    def generic_visit(self, node):
        if node.children:
            for child in node.children:
                self.visit(child)

    def visit_program(self, node):
        self.generic_visit(node)

    def visit_statement_list(self, node):
        self.generic_visit(node)

    def visit_statement(self, node):
        if node.children[0].value == '=':
            # Assignment statement
            variable = node.children[0]
            expr = node.children[1]
            self.visit(expr)
            self.code += f"MOV [{variable.value}], AX\n"
        else:
            # Print statement
            expr = node.children[0]
            self.visit(expr)
            self.code += "MOV DX, AX\n"
            self.code += "MOV AH, 2\n"
            self.code += "INT 21h\n"

    def visit_expr(self, node):
        if len(node.children) == 3:
            # Binary operation
            self.visit(node.children[0])
            self.code += "PUSH AX\n"
            self.visit(node.children[2])
            self.code += "POP BX\n"
            if node.children[1].type == 'PLUS':
                self.code += "ADD AX, BX\n"
            elif node.children[1].type == 'MINUS':
                self.code += "SUB AX, BX\n"

    def visit_term(self, node):
        if len(node.children) == 3:
            # Binary operation
            self.visit(node.children[0])
            self.code += "PUSH AX\n"
            self.visit(node.children[2])
            self.code += "POP BX\n"
            if node.children[1].type == 'TIMES':
                self.code += "MUL BX\n"
            elif node.children[1].type == 'DIVIDE':
                self.code += "XCHG AX, BX\n"
                self.code += "CWD\n"
                self.code += "IDIV BX\n"

    def visit_factor(self, node):
        if node.value.isdigit():
            # Integer literal
            self.code += f"MOV AX, {node.value}\n"
        else:
            # Variable
            self.code += f"MOV AX, [{node.value}]\n"

#-------------------------------------------------------------------------
#Функция для генерации ассемблерского кода из AST
else_labels = []  # Список для хранения меток блоков else
end_labels = []   # Список для хранения меток конца блоков
def generate_assembly_code(node):
    if node is None:
        return

    if node.type == 'PROGRAM':
        print('section .data')
        print('section .bss')
        generate_assembly_code(node.children[0])  # Генерация объявлений переменных
        print('section .text')
        print('global _start')
        print('_start:')
        generate_assembly_code(node.children[1])  # Генерация кода блока
        print('mov eax, 1')  # Выход из программы
        print('int 0x80')
    elif node.type == 'BLOCK':
        generate_assembly_code(node.children[0])  # Генерация объявлений переменных
        generate_assembly_code(node.children[1])  # Генерация операторов программы
    elif node.type == 'PROGRAM_HEADING':
        generate_program_heading_code(node)
    elif node.type == 'PROGRAM_BLOCK':
        generate_program_block_code(node)
    elif node.type == 'DECLARATIVE_PART':
        generate_declarative_part_code(node)
    elif node.type == 'STATEMENT_PART':
        generate_statement_part_code(node)
    elif node.type == 'DECLARATION_GROUP':
        for declaration in node.children:
            generate_assembly_code(declaration)  # Генерация объявления (const, var, type)
    elif node.type == 'CONSTANT_DEFINITION_GROUP':
        generate_constant_definition_group_code(node)
    elif node.type == 'COMPOUND_STATEMENT':
        generate_compound_statement_code(node)
    elif node.type == 'STATEMENT_SEQUENCE':
        generate_statement_sequence_code(node)
    elif node.type == 'CONSTANT_DEFINITION':
        # Генерация объявления константы
        constant_name = node.children[0].value
        constant_value = node.children[1].value
        assembly_code = f"{constant_name} equ {constant_value}"
        print(assembly_code)
    elif node.type == 'VARIABLE_DECLARATION':
        for var_node in node.children[0].children:
            var_name = var_node.value
            print(f'{var_name} resd 1')  # Объявление переменной
    elif node.type == 'STATEMENT':
        generate_statement_code(node)
    elif node.type == 'SIMPLE_STATEMENT':
        generate_simple_statement_code(node)
    elif node.type == 'STRUCTURED_STATEMENT':
        generate_structured_statement_code(node)
    elif node.type == 'VARIABLE_DECLARATION_GROUP':
        for variable_declaration in node.children:
            generate_assembly_code(variable_declaration)  # Генерация объявления переменной
    elif node.type == 'ASSIGNMENT_STATEMENT':
        generate_assembly_code(node.children[1])  # Вычисление выражения
        var_name = node.children[0].value
        print(f'mov [{var_name}], eax')  # Присваивание значения переменной
    elif node.type == 'BINARY_OPERATOR':
        generate_assembly_code(node.children[0])  # Левый операнд
        print('push eax')  # Сохранение результатов на стеке
        generate_assembly_code(node.children[2])  # Правый операнд
        print('pop ebx')  # Восстановление левого операнда
        if node.value == '+':
            print('add eax, ebx')
        elif node.value == '-':
            print('sub eax, ebx')
        elif node.value == '*':
            print('imul eax, ebx')
        elif node.value == '/':
            print('cdq')  # Расширение знака
            print('idiv ebx')
    elif node.type == 'WHILE_STATEMENT':
        # Генерация оператора цикла while
        boolean_expression = generate_expression_code(node.children[0])
        generate_assembly_code(node.children[1])  # Генерация тела цикла
        assembly_code = f"while_start:"
        assembly_code += f"    {boolean_expression}"
        assembly_code += f"    jz while_end"
        assembly_code += f"    ; Тело цикла"
        assembly_code += f"    jmp while_start"
        assembly_code += f"while_end:"
        print(assembly_code)
    # Обработка других типов узлов
    elif node.type == 'NUMBER':
        print(f'mov eax, {node.value}')
    elif node.type == 'IF_STATEMENT':
        generate_assembly_code(node.children[0])  # Вычисление условия
        print('cmp eax, 0')
        else_label = f'else_{len(else_labels)}'
        end_label = f'end_{len(end_labels)}'
        print(f'je {else_label}')  # Переход к блоку else при равенстве нулю
        generate_assembly_code(node.children[1])  # Генерация кода блока then
        print(f'jmp {end_label}')  # Переход в конец условного оператора
        print(f'{else_label}:')
        if len(node.children) == 3:
            generate_assembly_code(node.children[2])  # Генерация кода блока else
        print(f'{end_label}:')
    elif node.type == 'WRITE_PARAMETER':
        expression = generate_expression_code(node.children[0])
        assembly_code = f"mov eax, {expression}"
        assembly_code += f"call print"  # Предполагается наличие функции print
        print(assembly_code)
    elif node.type == 'EXPRESSION':
        generate_expression_code(node)
    elif node.type == 'CONDITIONAL_STATEMENT':
        generate_conditional_statement_code(node)
    elif node.type == 'REPETITIVE_STATEMENT':
        generate_repetitive_statement_code(node)
    elif node.type == 'SIMPLE_EXPRESSION':
        generate_simple_expression_code(node)
    elif node.type == 'FACTOR':
        generate_factor_code(node)
    elif node.type == 'VARIABLE_ACCESS':
        generate_variable_access_code(node)
    elif node.type == 'TERM':
        generate_term_code(node)
    elif node.type == 'MULTIPLYING_OPERATOR':
        generate_multiplying_operator_code(node)
    elif node.type == 'PROCEDURE_STATEMENT':
        generate_procedure_statement_code(node)
    else:
        # Обработка неизвестного типа узла
        print(f"Unknown node type: {node.type}")

# _____________________________-________________________________________________________-


def generate_conditional_statement_code(conditional_statement_node):
    if len(conditional_statement_node.children) == 2:
        # Обработка условного оператора if-then
        generate_assembly_code(conditional_statement_node.children[0])  # Вычисление условия
        print('cmp eax, 0')
        else_label = f'else_{len(else_labels)}'
        end_label = f'end_{len(end_labels)}'
        print(f'je {else_label}')  # Переход к блоку else при равенстве нулю
        generate_assembly_code(conditional_statement_node.children[1])  # Генерация кода блока then
        print(f'jmp {end_label}')  # Переход в конец условного оператора
        print(f'{else_label}:')
        print(f'{end_label}:')
    elif len(conditional_statement_node.children) == 3:
        # Обработка условного оператора if-then-else
        generate_assembly_code(conditional_statement_node.children[0])  # Вычисление условия
        print('cmp eax, 0')
        else_label = f'else_{len(else_labels)}'
        end_label = f'end_{len(end_labels)}'
        print(f'je {else_label}')  # Переход к блоку else при равенстве нулю
        generate_assembly_code(conditional_statement_node.children[1])  # Генерация кода блока then
        print(f'jmp {end_label}')  # Переход в конец условного оператора
        print(f'{else_label}:')
        generate_assembly_code(conditional_statement_node.children[2])  # Генерация кода блока else
        print(f'{end_label}:')

def generate_repetitive_statement_code(repetitive_statement_node):
    # Генерация оператора цикла while
    boolean_expression = generate_expression_code(repetitive_statement_node.children[0])
    assembly_code = f"while_start:"
    assembly_code += f"    {boolean_expression}"
    assembly_code += f"    jz while_end"
    assembly_code += f"    ; Тело цикла"
    assembly_code += f"    jmp while_start"
    assembly_code += f"while_end:"
    print(assembly_code)
def generate_expression_code(expression_node):
    generate_shift_expression_code(expression_node.children[0])
def generate_shift_expression_code(shift_expression_node):
    if len(shift_expression_node.children) == 1:
        return generate_simple_expression_code(shift_expression_node.children[0])
    else:
        left_expression = generate_shift_expression_code(shift_expression_node.children[0])
        right_expression = generate_simple_expression_code(shift_expression_node.children[1])
        operator = shift_expression_node.children[1].value
        if operator == '<<':
            return f"{left_expression} shl {right_expression}"
        elif operator == '>>':
            return f"{left_expression} shr {right_expression}"
        else:
            print(f"Unknown shift operator: {operator}")
            return ''
def generate_simple_expression_code(simple_expression_node):
    if len(simple_expression_node.children) == 1:
        return generate_term_code(simple_expression_node.children[0])
    else:
        left_term = generate_term_code(simple_expression_node.children[0])
        operator = simple_expression_node.children[1].value
        return f"{left_term} {operator} {generate_term_code(simple_expression_node.children[2])}"

def generate_term_code(term_node):
    if len(term_node.children) == 1:
        return generate_factor_code(term_node.children[0])
    else:
        left_factor = generate_factor_code(term_node.children[0])
        right_factor = generate_factor_code(term_node.children[2])
        operator = term_node.children[1].value

        if operator == '*':
            return f"{left_factor} * {right_factor}"
        elif operator == '/':
            return f"{left_factor} / {right_factor}"
        else:
            print(f"Unknown operator in term: {operator}")
            return ''

def generate_factor_code(factor_node):
    if factor_node.type == 'VARIABLE_ACCESS':
        return generate_variable_access_code(factor_node.children[0])
    elif factor_node.type == 'NUMBER':
        return factor_node.value
    else:
        print(f"Unknown factor node type: {factor_node.type}")
        return ''
#
def generate_variable_access_code(variable_access_node):
    if variable_access_node.type == 'ENTIRE_VARIABLE':
        variable_identifier = variable_access_node.children[0].value
        return variable_identifier
    else:
        print(f"Unknown variable access node type: {variable_access_node.type}")
        return ''

def generate_program_heading_code(program_heading_node):
    program_name = program_heading_node.children[1].value
    print(f'global {program_name}')

def generate_program_block_code(program_block_node):
    for child in program_block_node.children:
        generate_assembly_code(child)

def generate_declarative_part_code(declarative_part_node):
    for declaration_group in declarative_part_node.children:
        generate_assembly_code(declaration_group)

def generate_statement_part_code(statement_part_node):
    for statement in statement_part_node.children:
        generate_assembly_code(statement)
def generate_statement_sequence_code(statement_sequence_node):
    for statement in statement_sequence_node.children:
        generate_assembly_code(statement)
def generate_constant_definition_group_code(constant_definition_group_node):
    for constant_definition in constant_definition_group_node.children:
        generate_assembly_code(constant_definition)

def generate_compound_statement_code(compound_statement_node):
    for statement in compound_statement_node.children:
        generate_assembly_code(statement)

def generate_statement_code(statement_node):
    child = statement_node.children[0]
    generate_assembly_code(child)

def generate_simple_statement_code(simple_statement_node):
    child = simple_statement_node.children[0]
    generate_assembly_code(child)

def generate_structured_statement_code(structured_statement_node):
    child = structured_statement_node.children[0]
    generate_assembly_code(child)
def generate_factor_code(factor_node):
    if factor_node.type == 'VARIABLE_ACCESS':
        return generate_variable_access_code(factor_node.children[0])
    elif factor_node.type == 'NUMBER':
        return factor_node.value
    elif factor_node.type == 'FACTOR':
        return generate_factor_code(factor_node.children[0])
    elif factor_node.type == 'SIMPLE_EXPRESSION':
        return generate_simple_expression_code(factor_node.children[0])
    else:
        print(f"Unknown factor node type: {factor_node.type}")
        return ''

def generate_variable_access_code(variable_access_node):
    if variable_access_node.type == 'ENTIRE_VARIABLE':
        variable_identifier = variable_access_node.children[0].value
        return variable_identifier
    else:
        print(f"Unknown variable access node type: {variable_access_node.type}")
        return ''

def generate_multiplying_operator_code(node):
    if node.type == 'MULTIPLYING_OPERATOR':
        return node.value
    else:
        print(f"Unknown multiplying operator node type: {node.type}")
        return ''
def generate_term_code(term_node):
    if len(term_node.children) == 1:
        return generate_factor_code(term_node.children[0])
    else:
        left_factor = generate_factor_code(term_node.children[0])
        right_factor = generate_factor_code(term_node.children[2])
        operator = term_node.children[1].value

        if operator == '*':
            return f"{left_factor} * {right_factor}"
        elif operator == '/':
            return f"{left_factor} / {right_factor}"
        else:
            print(f"Unknown operator in term: {operator}")
            return ''

def generate_procedure_statement_code(procedure_statement_node):
    procedure_identifier = procedure_statement_node.children[0].value
    if procedure_identifier == 'write':
        generate_write_code(procedure_statement_node.children[1])
    else:
        print(f"Unknown procedure identifier: {procedure_identifier}")

def generate_write_code(write_parameter_list_node):
    for write_parameter_node in write_parameter_list_node.children:
        generate_expression_code(write_parameter_node.children[0])  # Генерация кода выражения
        print('push eax')  # Помещение значения на стек для параметра функции write
        print('call print')  # Вызов функции вывода
        print('add esp, 4')  # Очистка параметра из стека
