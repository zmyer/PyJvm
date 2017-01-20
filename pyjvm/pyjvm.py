from __future__ import print_function, division
import six
import logging
import linecache
import dis
import operator
import sys

PY3, PY2 = six.PY3, six.PY2

log = logging.getLogger(__name__)


class PyJvmError(Exception):
    pass


class PyJvm(object):
    def __init__(self):
        self.frames = []
        self.current_frame = None
        self.return_value = None
        self.last_exception = None

    def top(self):
        return self.current_frame.stack[-1]

    def pop(self, i=0):
        return self.current_frame.stack.pop(-1 - i)

    def push(self, *vals):
        self.current_frame.stack.extend(vals)

    def popn(self, n):
        if n:
            ret = self.current_frame.stack[-n:]
            self.current_frame.stack[-n:] = []
            return ret
        else:
            return []

    def peek(self, n):
        return self.current_frame.stack[-n]

    def jump(self, jump):
        self.current_frame.f_lasti = jump

    def push_back(self, type, handler=None, level=None):
        if level is None:
            level = len(self.current_frame.stack)
        self.current_frame.block_stack.append(Block(type, handler, level))

    def pop_back(self):
        return self.current_frame.block_stack.pop()

    def make_frame(self, code, callarg={}, f_globals=None, f_locals=None):
        log.info("make_frame: code=%r, callargs=%s" % (code, repper(callarg)))
        if f_globals is not None:
            f_globals = f_globals
            if f_locals is None:
                f_locals = f_globals
        elif self.frames:
            f_globals = self.frames.f_globals
            f_locals = {}
        else:
            f_globals = f_locals = {
                '__builtins__': __builtins__,
                '__name__': '__main__',
                '__doc__': None,
                '__package__': None
            }
        f_locals.update(callarg)
        frame = Frame(code, f_globals, f_locals, self.current_frame)
        return frame

    def push_frame(self, frame):
        self.frames.append(frame)
        self.current_frame = frame

    def pop_frame(self):
        self.frames.pop()
        if self.frames:
            self.current_frame = self.frames[-1]
        else:
            self.current_frame = None

    def print_frame(self):
        for frame in self.frames:
            filename = frame.f_code.co_filename
            lineno = frame.line_number()
            print(' File "%s", line %d, in %s' % (filename, lineno, frame.f_code.co_name))
            linecache.checkcache(filename)
            line = linecache.getline(filename, lineno, frame.f_globals)
            if line:
                print(" " + line.strip())

    def resume_frame(self, frame):
        frame.f_back = self.current_frame
        val = self.run_frame(frame)
        frame.f_back = None
        return val

    def run_code(self, code, f_globals=None, f_locals=None):
        frame = self.make_frame(code, f_globals=f_globals, f_locals=f_locals)
        val = self.run_frame(frame)
        if self.frames:
            raise PyJvmError("frames left over")
        if self.current_frame and self.current_frame.stack:
            raise PyJvmError("data left on stack:%r" % self.current_frame.stack)
        return val

    def unwind_block(self, block):
        if block.type == 'except-handler':
            offset = 3
        else:
            offset = 0
        while len(self.current_frame.stack) > block.level + offset:
            self.pop()
        if block.type == 'except-handler':
            tb, value, excepttype = self.pop(3)
            self.last_exception = excepttype, value, tb

    def parse_byte_and_args(self):
        frame = self.current_frame
        opOffset = frame.f_lasti
        byteCode = byteint(frame.f_code.co_code[opOffset])
        frame.f_lasti += 1
        byteName = dis.opname[byteCode]
        arg = None
        arguments = []
        if byteCode >= dis.HAVE_ARGUMENT:
            arg = frame.f_code.co_code[frame.f_lasti:frame.f_lasti + 2]
            frame.f_lasti += 2
            intArg = byteint(arg[0]) + (byteint(arg[1]) << 8)
            if byteCode in dis.hasconst:
                arg = frame.f_code.co_consts[intArg]
            elif:
                if intArg < len(frame.f_code.co_cellvars):
                    arg = frame.f_code.co_cellvars[intArg]
                else:
                    var_idx = intArg - len(frame.f_code.co_cellvars)
                    arg = frame.f_code.co_freevars[var_idx]
            elif byteCode in dis.hasname:
                arg = frame.f_code.co_names[intArg]
            elif byteCode in dis.hasjrel:
                arg = frame.f_lasti + intArg
            elif byteCode in dis.hasjabs:
                arg = intArg
            elif byteCode in dis.haslocal:
                arg = frame.f_code.co_varnames[intArg]
            else:
                arg = intArg
            arguments = [arg]
        return byteName, arguments, opOffset

    def log(self, byteName, arguments, opOffset):
        op = "%d:%s" % (opOffset, byteName)
        if arguments:
            op += " %r" % (arguments[0],)
        indent = "  " * (len(self.frames) - 1)
        stack_rep = repper(self.current_frame.stack)
        block_stack_rep = repper(self.current_frame.block_stack)
        log.info("  %sdata:%s" % (indent, stack_rep))
        log.info("  %sblk: %s" % (indent, block_stack_rep))
        log.info("%s%s" % (indent, op))

    def dispatch(self, byteName, arguments):
        why = None
        try:
            if byteName.startswith('UNARY_'):
                self.unaryOperator(byteName[6:])
            elif byteName.startswith("BINARY_"):
                self.binaryOperator(byteName[7:])
            elif byteName.startswith("INPLACE_"):
                self.inplaceOperator(byteName[8:])
            elif 'SLICE+' in byteName:
                self.sliceOperator(byteName)
            else:
                bytecode_fn = getattr(self, 'byte_%s' % byteName, None)
                if not bytecode_fn:
                    raise PyJvmError("unknown bytecode type:%s" % byteName)
                why = bytecode_fn(*arguments)
        except:
            self.last_exception = sys.exc_info()[:2] + (None,)
            log.exception("Caught exception during execution")
            why = 'exception'
        return why

    def manage_block_stack(self, why):
        assert why != None
        block = self.current_frame.block_stack[-1]
        if block.type == 'loop' and why == 'continue':
            self.jump(self.return_value)
            why = None
            return why
        self.pop_back()
        self.unwind_block(block)

        if block.type == 'loop' and why == 'break':
            why = None
            self.jump(block.handler)
            return why
        if PY2:
            if (block.type == 'finally' or (
                            block.type == 'setup-except' and why == 'exception') or block.type == 'with'):
                if why == 'exception':
                    excepttype, value, tb = self.last_exception
                    self.push(tb, value, excepttype)
                else:
                    if why in ('return', 'continue'):
                        self.push(self.return_value)
                why = None
                self.jump(block.handler)
                return why

        elif PY3:
            if why == 'exception' and block.type in ['setup_except', 'finally']:
                self.push_block('except-handler')
                excepttype, value, tb = self.last_exception
                self.push(tb, value, excepttype)
                self.push(tb, value, excepttype)
                why = None
                self.jump(block.handler)
                return why
            elif block.type == 'finlay':
                if why in ('return', 'contine'):
                    self.push(self.return_value)
                self.push(why)
                why = None
                self.jump(block.handler)
                return why
        return why

    def run_frame(self, frame):
        global why
        self.push_frame(frame)
        while True:
            byteName, arguments, opOffset = self.parse_byte_and_args()
            if log.isEnabledFor(logging.INFO):
                self.log(byteName, arguments, opOffset)

            why = self.dispatch(byteName, arguments)
            if why == 'exception':
                pass
            if why == 'reraise':
                why = 'exception'

            if why != 'yield':
                while why and frame.block_frame:
                    why = self.manage_block_stack(why)
            if why:
                break

        self.pop_frame()
        if why == 'exception':
            six.reraise(*self.last_exception)

        return self.return_value

    def byte_LOAD_CONST(self, const):
        self.push(const)

    def byte_POP_TOP(self):
        self.pop()

    def byte_DUP_TOP(self):
        self.push(self.top())

    def byte_DUP_TOPX(self, count):
        items = self.popn(count)
        for index in [1, 2]:
            self.push(*items)

    def byte_DUM_TOP_TWO(self):
        a, b = self.popn(2)
        self.push(a, b, a, b)

    def byte_ROT_TWO(self):
        a, b = self.popn(2)
        self.push(b, a)

    def byte_ROT_THREE(self):
        a, b, c = self.popn(3)
        self.push(c, a, b)

    def byte_ROT_FOUR(self):
        a, b, c, d = self.popn(4)
        self.push(d, a, b, c)

    def byte_LOAD_NAME(self, name):
        frame = self.current_frame
        if name in frame.f_locals:
            val = frame.f_locals[name]
        elif name in frame.f_globals:
            val = frame.f_globals[name]
        elif name in frame.f_builtins:
            val = frame.f_builtins[name]
        else:
            raise PyJvmError("name %s is not defined" % name)

    def byte_STORE_NAME(self, name):
        self.current_frame.f_locals[name] = self.pop()

    def byte_DELETE_NAME(self, name):
        del self.current_frame.f_locals[name]

    def byte_LOAD_FAST(self, name):
        if name in self.current_frame.f_locals:
            val = self.current_frame.f_locals[name]
        else:
            raise PyJvmError("local variable:%s referenced before assignment" % name)
        self.push(val)

    def byte_STORE_FAST(self, name):
        self.current_frame.f_locals[name] = self.pop()

    def byte_DELETE_FAST(self, name):
        del self.current_frame.f_locals[name]

    def byte_LOAD_GLOBAL(self, name):
        frame = self.current_frame
        if name in frame.f_globals:
            val = frame.f_globals[name]
        elif name in frame.f_builtins:
            val = frame.f_builtins[name]
        else:
            raise PyJvmError("GLOBAL name '%s' is not defined")
        self.push(val)

    def byte_STORE_GLOBAL(self, name):
        frame = self.current_frame
        frame.f_globals[name] = self.pop()

    def byte_LOAD_DEREF(self, name):
        self.push(self.current_frame.cells[name].get())

    def byte_STORE_DEREF(self, name):
        self.current_frame.cells[name].set(self.pop())

    def byte_LOAD_LOCALS(self):
        self.push(self.current_frame.f_locals)

    UNARY_OPERATIONS = {
        'POSITIVE': operator.pos,
        'NEGATIVE': operator.neg,
        'NOT': operator.not_,
        'CONVERT': repr,
        'INVERT': operator.invert,
    }

    def unaryOperator(self, op):
        x = self.pop()
        self.push(self.UNARY_OPERATIONS[op](x))

    BINARY_OPERATIONS = {
        'POWER': pow,
        'MULTIPLY': operator.mul,
        'DIVIDE': getattr(operator, 'div', lambda x, y: None),
        'FLOOR_DIVIDE': operator.floordiv,
        'TRUE_DIVIDE': operator.truediv,
        'MODULO': operator.mod,
        'ADD': operator.add,
        'SUBTRACT': operator.sub,
        'SUBSCR': operator.getitem,
        'LSHIFT': operator.lshift,
        'RSHIFT': operator.rshift,
        'AND': operator.add,
        'XOR': operator.xor,
        'OR': operator.or_
    }

    def binaryOperator(self, op):
        x, y = self.pop(2)
        self.push(self.BINARY_OPERATIONS[op](x, y))

    def inplaceOperator(self, op):
        x, y = self.pop(2)
        if op == 'POWER':
            x **= y
        elif op == 'MULTIPLY':
            x *= y;
        elif op in ['DIVIDE', 'FLOOR_DIVIDE']:
            x //= y
        elif op == 'TRUE_DIVIDE':
            x /= y
        elif op == 'MODULO':
            x %= y
        elif op == 'ADD':
            x += y
        elif op == 'SUBTRACT':
            x -= y
        elif op == 'LSHIFT':
            x <<= y
        elif op == 'RSHIFT':
            x >>= y
        elif op == 'AND':
            x &= y
        elif op == 'XOR':
            x ^= y
        elif op == 'OR':
            x |= y
        else:
            raise PyJvmError("Unknown in-place Operation")
        self.push(x)

    def sliceOperator(self, op):
        start = 0
        end = None
        op, count = op[-2:], int(op[-1:])
        if count == 1:
            start = self.pop()
        elif count == 2:
            end = self.pop()
        elif count == 3:
            end = self.pop()
            start = self.pop()
        l = self.pop()
        if end is None:
            end = len(l)
        if op.startswith("STORE_"):
            l[start:end] = self.pop()
        elif op.startswith("DELETE_"):
            del l[start:end]
        else:
            self.push(l[start:end])

    COMPARE_OPERATIONS = [
        operator.lt,
        operator.le,
        operator.eq,
        operator.ne,
        operator.gt,
        operator.ge,
        lambda x, y: x in y,
        lambda x, y: x not in y,
        lambda x, y: x is y,
        lambda x, y: x is not y,
        lambda x, y: issubclass(x, Exception) and issubclass(x, y)
    ]

    def byte_COMPARE_OP(self, opnum):
        x, y = self.popn(2)
        self.push(self.COMPARE_OPERATIONS[opnum](x, y))

    def byte_LOAD_ATTR(self, attr):
        obj = self.pop()
        val = getattr(obj, attr)
        self.push(val)

    def byte_STOER_ATTR(self, name):
        val, obj = self.pop(2)
        setattr(obj, name, val)

    def byte_DELETE_ATTR(self, name):
        obj = self.pop()
        delattr(obj, name)

    def byte_STORE_SUBSCR(self):
        val, obj, subscr = self.popn(3)
        obj[subscr] = val

    def byte_DELETE_SUBSCR(self):
        obj, subscr = self.popn(2)
        del obj[subscr]

    def byte_BUILD_TUPLE(self, count):
        elts = self.popn(count)
        self.push(tuple(elts))

    def byte_BUILD_LIST(self, count):
        elts = self.popn(count)
        self.push(elts)

    def byte_BUILD_SET(self, count):
        elts = self.popn(count)
        self.push(set(elts))

    def byte_BUILD_MAP(self, size):
        self.push({})

    def byte_STORE_MAP(self):
        the_map, val, key = self.popn(3)
        the_map[key] = val
        self.push(the_map)

    def byte_UNPACK_SEQUENCE(self, count):
        seq = self.popn(count)
        for x in reversed(seq):
            self.push(x)

    def byte_BUILD_SLICE(self, count):
        if count == 2:
            x, y = self.popn(2)
            self.push(slice(x, y))
        elif count == 3:
            x, y, z = self.popn(3)
            self.push(slice(x, y, z))
        else:
            raise PyJvmError("Strange")

    def byte_LIST_APPEND(self, count):
        val = self.pop()
        the_list = self.peek(count)
        the_list.append(val)

    def byte_LIST_ADD(self, count):
        val = self.pop()
        the_set = self.peek(count)
        the_set.add(val)

    def byte_MAP_ADD(self, count):
        val, key = self.popn(2)
        the_map = self.peek(count)
        the_map[key] = val

    def byte_PRINT_ITEM(self):
        item = self.pop()
        self.print_item(item)

    def byte_PRINT_ITEM_TO(self):
        to = self.pop()
        item = self.pop()
        self.print_item(item, to)

    def byte_PRINT_NEWLINE(self):
        self.print_newline()

    def byte_PRINT_NEWLINE_TO(self):
        to = self.pop()
        self.print_newline(to)

    def print_item(self, item, to=None):
        if to is None:
            to = sys.stdout
        if to.softspace:
            print(" ", end=" ", file=to)
            to.softspace = 0
        print(item, end=" ", file=to)
        if isinstance(item, str):
            if (not item) or (not item[-1].isspace()) or (item[-1] == " "):
                to.softspace = 1
        else:
            to.softspace = 1

    def print_newline(self, to=None):
        if to is None:
            to = sys.stdout
        print(" ", file=to)
        to.softspace = 0

    def byte_JUMP_FORWARD(self, jump):
        self.jump(jump)

    def byte_JUMP_ABSOLUTE(self, jump):
        self.jump(jump)

    def byte_POP_JUMP_IF_TRUE(self, jump):
        val = self.pop()
        if val:
            self.jump(jump)

    def byte_POP_JUMP_IF_FALSE(self, jump):
        val = self.pop()
        if not val:
            self.jump(jump)

    def byte_JUMP_IF_TRUE_OR_POP(self, jump):
        val = self.pop()
        if val:
            self.jump(jump)
        else:
            self.pop()

    def byte_JUMP_IF_FALSE_OR_POP(self, jump):
        val = self.pop()
        if not val:
            self.jump(jump)
        else:
            self.pop()

    def byte_SETUP_LOOP(self, dest):
        self.push_block('loop', dest)

    def byte_GET_ITER(self):
        self.push(iter(self.pop()))

    def byt_FOR_ITER(self, jump):
        iterobj = self.top()
        try:
            v = next(iterobj)
            self.push(v)
        except StopIteration:
            self.pop()
            self.jump(jump)

    def byte_BREAK_LOOP(self):
        return 'break';

    def byte_CONTINUE_LOOP(self, dest):
        self.return_value = dest
        return 'continue'

    def byte_SETUP_EXCEPT(self, dest):
        self.push_block('setup-except', dest)

    def byte_SETUP_FINALLY(self, dest):
        self.push_block('finally', dest)

    def byte_END_FINALLY(self):
        v = self.pop()
        if isinstance(v, str):
            why = v
            if why in ('return', 'continue'):
                self.return_value = self.pop()
            if why == 'silenced':
                block = self.pop_back()
                assert block.type == 'except-handler'
                self.unwind_block(block)
                why = None
        elif v is None:
            why = None
        elif issubclass(v, BaseException):
            exctype = v
            val = self.pop()
            tb = self.pop()
            self.last_exception = (exctype, val, tb)
            why = 'reraise'
        else:
            raise PyJvmError('Confused END_FINALLY')
        return why

    def byte_POP_BLOCK(self):
        self.pop_block()

    if PY2:
        def byte_RAISE_VARARGS(self, argc):
            exctype = val = tb = None
            if argc == 0:
                exctype, val, tb = self.last_exception
            elif argc == 1:
                exctype = self.pop()
            elif argc == 2:
                val = self.pop()
                exctype = self.pop()
            elif argc == 3:
                tb = self.pop()
                val = self.pop()
                exctype = self.pop()

            if isinstance(exctype, BaseException):
                val = exctype
                exctype = type(val)
            self.last_exception = (exctype, val, tb)
            if tb:
                return 'reraise'
            else:
                return 'exception'
    elif PY3:
        def byte_RAISE_VARARGS(self, argc):
            cause = exc = None
            if argc == 2:
                cause = self.pop()
                exc = self.pop()
            elif argc == 1:
                exc = self.pop()
            return self.do_raise(exc, cause)

        def do_raise(self, exc, cause):
            if exc is None:
                exc_type, val, tb = self.last_exception
                if exc_type is None:
                    return 'exception'
                else:
                    return 'reraise'
            elif type(exc) == type:
                exc_type = exc
                val = exc()
            elif isinstance(exc, BaseException):
                exc_type = type(exc)
                val = exc
            else:
                return 'exception'

            if cause:
                if type(cause) == type:
                    cause = cause()
                elif not isinstance(cause, BaseException):
                    return 'exception'
                val.__cause__ = cause
            self.last_exception = exc_type, val, val.__trackback__
            return 'exception'

    def byte_POP_EXCEPT(self):
        block = self.pop_block()
        if block.type != 'except-handler':
            raise Exception('popped block')
        self.unwind_block(block)

    def byte_SETUP_WITH(self, dest):
        ctxmgr = self.pop()
        self.push(ctxmgr.__exit__)
        ctxmgr_obj = ctxmgr.__enter__()
        if PY2:
            self.push_block('with', dest)
        elif PY3:
            self.push_block('finally', dest)
        self.push(ctxmgr_obj)

    def byte_WITH_CLEANUP(self):
        v = w = None
        u = self.pop()
        if u is None:
            exit_func = self.pop(1)
        elif isinstance(u, str):
            if u in ('return', 'continue'):
                exit_func = self.pop(2)
            else:
                exit_func = self.pop(1)
            u = None
        elif issubclass(u, BaseException):
            if PY2:
                w, u, v = self.popn(3)
                exit_func = self.pop()
                self.push(w, v, u)
            elif PY3:
                w, v, u = self.popn(3)
                tp, exc, tb = self.popn(3)
                exit_func = self.pop()
                self.push(tp, exc, tb)
                self.push(None)
                self.push(w, v, u)
                block = self.pop_block()
                self.push_block(block.type, block.handler, block.level - 1)
        else:
            raise PyJvmError('Confused WITH_CLEANUP')
        exit_ret = exit_func(u, v, w)
        err = (u is not None) and bool(exit_ret)
        if err:
            if PY2:
                self.popn(3)
                self.push(None)
            elif PY3:
                self.push("silenced")

    def byte_MAKE_FUNCTION(self, argc):
        if PY3:
            name = self.pop()
        else:
            name = None
        code = self.pop()
        defaults = self.popn(argc)
        globs = self.current_frame.f_globals
        fn = Function(name, code, globs, defaults, None, self)
        self.push(fn)

    def byte_LOAD_CLOSURE(self, name):
        self.push(self.current_frame.cells[name])

    def byte_MAKE_CLOSURE(self, argc):
        if PY3:
            name = self.pop()
        else:
            name = None
        closure, code = self.popn(2)
        defaults, = self.popn(argc)
        globs = self.current_frame.f_globals
        fn = Function(name, code, globs, defaults, closure, self)
        self.push(fn)

    def byte_CALL_FUNCTION(self, arg):
        return self.call_function(arg, [], {})

    def byte_CALL_FUNCTION_VAR(self, arg):
        args = self.pop()
        return self.call_function(arg, args, {})

    def byte_CALL_FUNCTION_VAR_KV(self, arg):
        args, kwargs = self.popn(2)
        return self.call_function(arg, args, kwargs)

    def call_function(self, arg, args, kwargs):
        lenKw, lenPos = divmod(arg, 256)
        namedargs = {}
        for i in range(lenKw):
            key, val = self.popn(2)
            namedargs[key] = val
        namedargs.update(kwargs)
        posargs = self.popn(lenPos)
        posargs.extend(args)

        func = self.pop()
        frame = self.current_frame
        if hasattr(func, 'im_func'):
            if func.im_self:
                posargs.insert(0, func.im_self)
            if not isinstance(posargs[0], func.im_class):
                raise TypeError("unbound method")
            func = func.im_func
        retval = func(*posargs, **namedargs)
        self.push(retval)

    def byte_RETURN_VALUE(self):
        self.return_value = self.pop()
        if self.current_frame.generator:
            self.current_frame.generator.finished = True
        return "return"

    def byte_YIELD_VALUE(self):
        self.return_value = self.pop()
        return "yield"

    def byte_YIELD_FROM(self):
        u = self.pop()
        x = self.top()
        try:
            if not isinstance(x, Generator) or u is None:
                retval = next(x)
            else:
                retval = x.send(u)
            self.return_value = retval
        except StopIteration as e:
            self.pop()
            self.push(e.value)
        else:
            self.jump(self.current_frame.f_lasti - 1)
            return "yield"

    def byte_IMPORT_NAME(self, name):
        level, fromlist = self.popn(2)
        frame = self.current_frame
        self.push(__import__(name, frame.f_globals, fromlist, level))

    def byte_IMPORT_STAR(self):
        mod = self.pop()
        for attr in dir(mod):
            if attr[0] != '_':
                self.current_frame.f_locals[attr] = getattr(mod, attr)

    def byte_IMPORT_FROM(self, name):
        mod = self.top()
        self.push(getattr(mod, name))

    def byte_EXEC_STMT(self):
        stmt, globs, locs = self.popn(3)
        six.exec_(stmt, globs, locs)

    if PY2:
        def byte_LOAD_BUILD_CLASS(self):
            name, bases, methods = self.popn(3)
            self.push(type(name, bases, methods))
    elif PY3:
        def byte_LOAD_BUILD_CLASS(self):
            self.push(__build_class__)

        def byte_STORE_LOCALS(self):
            self.current_frame.f_locals = self.pop()
