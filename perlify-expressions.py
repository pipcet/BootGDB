import gdb

def opcode_string(t):
    s = gdb.opcodes[t.code]
    return s

def print_expression_subexpressions(t):
    ret = ""
    try:
        ret2 = "["
        for f in t.children():
            ret2 += print_expression_rec(f)
            ret2 += ", "
        ret2 += "]"
        ret += ret2
    except Exception:
        pass
    return ret

def print_expression_inner(t):
    ret = "{"
    f = print_expression_subexpressions(t)
    if f != "":
        ret += "subexps=>" + f + ", "
    try:
        ret += "value=>'" + str(t.value()) + "', "
    except Exception:
        pass
    try:
        ret += "index=>'" + str(t.index()) + "', "
    except Exception:
        pass
    try:
        ret += "type=>" + print_type_rec(t.type(), True) + ", "
    except Exception:
        pass
    ret += "}"
    return ret

def print_expression_rec(t):
    return "$e->(" + opcode_string(t) + " => " + print_expression_inner(t) + ")"

def print_expression(expr):
    t = gdb.parse_expression(expr)
    return "$expr->('" + expr + "', " + print_expression_rec(t.opcodes()[0]) + ", " + str(t.address()) + ");\n"

# this is C-specific. Most other languages do not have tags, though.
def real_name(t):
    if t.name:
        return t.name
    elif t.tag:
        return type_code_string(t).lower() + " " + t.tag

def type_code_string(t):
    s = gdb.typecodes[t.code]
    s = s.replace("TYPE_CODE_", "")
    return s

def print_field(f, show):
    ret = "{"
    try:
        ret += "name=>'" + f.name + "', "
    except Exception:
        pass
    if f.bitsize != 0:
        ret += "bitsize=>" + str(f.bitsize) + ", "
    ret += "bitpos=>" + str(f.bitpos) + ", "
    ret += "type=>" + print_type_rec(f.type, False) + ", "
    ret += "}"
    return ret

def print_type_fields(t, show):
    ret = ""
    try:
        ret2 = "["
        for f in t.fields():
            ret2 += print_field(f, show)
            ret2 += ", "
        ret2 += "]"
        ret += ret2
    except Exception:
        pass
    return ret

def print_type_inner(t, show):
    ret = "{"
    f = print_type_fields(t,show)
    if f != "":
        ret += "fields=>" + f + ", "
    ret += "sizeof=>" + str(t.sizeof)
    try:
        ret += ", target=>" + print_type_rec(t.target(), False)
    except Exception:
        pass
    ret += "}"
    return ret

def print_type_rec(t, show):
    name = real_name(t)
    if name and not show:
        return "$t->(name => '" + name + "')"
    else:
        return "$t->(" + type_code_string(t) + " => " + print_type_inner(t, show) + ")"

def print_symbol_type(name):
    o = gdb.lookup_global_symbol(name)
    if not o:
        return ""
    t = o.type
    return "$def->('" + o.name + "', " + print_type_rec(t, True) + ");\n"

def print_type(name):
    t = gdb.lookup_type(name)
    return "$def->('" + real_name(t) + "', " + print_type_rec(t, True) + ");\n"

def print_macro(name, linespec):
    macros = gdb.macros(gdb.decode_line(linespec)[1][0])
    args = []
    m = macros[name]
    if m.argc() is not None:
        for i in range(m.argc()):
            args.append("$x" + str(i))
    s = m.expand(args, gdb.decode_line(linespec)[1][0])
    exp = gdb.parse_expression(s);
    return print_expression_rec(exp.opcodes()[0])

def macro_string(name, linespec):
    macros = gdb.macros(gdb.decode_line(linespec)[1][0])
    args = []
    m = macros[name]
    if m.argc() is not None:
        for i in range(m.argc()):
            args.append("arg" + str(i))
    try:
        s = m.expand(args, gdb.decode_line(linespec)[1][0])
        return s

    except Exception:
        return ""

def macro_expression(name, linespec):
    macros = gdb.macros(gdb.decode_line(linespec)[1][0])
    args = []
    m = macros[name]
    if m.argc() is not None:
        for i in range(m.argc()):
            args.append("$x" + str(i))
    try:
        s = m.expand(args, gdb.decode_line(linespec)[1][0])
        if s == "":
            return ""
        exp = print_expression(s)
        return exp

    except Exception:
        return ""

def print_macro_type(name, linespec):
    macros = gdb.macros(gdb.decode_line(linespec)[1][0])
    args = []
    m = macros[name]
    if m.argc() is not None:
        for i in range(m.argc()):
            args.append("$x" + str(i))
    try:
        s = m.expand(args, gdb.decode_line(linespec)[1][0])
        if s == "":
            return ""
        exp = gdb.parse_expression(s)
        t = exp.evaluate_type()
        return print_type_rec(t, True) + "\n"
    except Exception:
        return ""
