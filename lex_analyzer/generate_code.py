import ctypes

import llvmlite.ir as llvm_ir
import llvmlite.binding as llvm

# Инициализация LLVM
llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()

# Создание модуля LLVM и функции
module = llvm_ir.Module(name="calculator")
function_type = llvm_ir.FunctionType(llvm_ir.DoubleType(), [])
function = llvm_ir.Function(module, function_type, name="main")

# Создание базового блока
block = function.append_basic_block(name="entry")
builder = llvm_ir.IRBuilder(block)

# Генерация кода для арифметического выражения (например, 3 + 4)
value_1 = llvm_ir.Constant(llvm_ir.DoubleType(), 3.0)
value_2 = llvm_ir.Constant(llvm_ir.DoubleType(), 4.0)
result = builder.fadd(value_1, value_2, name="add_result")

# Возвращение результата
builder.ret(result)

# Создание движка исполнения и генерация машинного кода
target = llvm.Target.from_default_triple()
target_machine = target.create_target_machine()
engine = llvm.create_mcjit_compiler(module, target_machine)

# Получение указателя на функцию и вызов ее
func_ptr = engine.get_function_address("main")
calc_func = ctypes.CFUNCTYPE(ctypes.c_double)(func_ptr)
print("Result:", calc_func())

# Очистка ресурсов
llvm.shutdown()




#-----------------------------------------


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
