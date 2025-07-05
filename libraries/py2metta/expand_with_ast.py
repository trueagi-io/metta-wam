
import ast
import sys

class FullPythonUncompressor(ast.NodeTransformer):
    def __init__(self):
        self.counter = 0
        self.statements = []
        self.introduced_names = set()

    def unique_var(self, base="temp"):
        self.counter += 1
        return f"{base}_{self.counter}"

    def extract_expr(self, expr, base="temp"):
        if isinstance(expr, (ast.BinOp, ast.UnaryOp, ast.Call, ast.Compare, ast.BoolOp)):
            temp_name = self.unique_var(base)
            temp_var = ast.Name(id=temp_name, ctx=ast.Load())
            self.statements.append(ast.Assign(
                targets=[ast.Name(id=temp_name, ctx=ast.Store())],
                value=self.visit(expr)
            ))
            return temp_var
        elif isinstance(expr, ast.IfExp):
            temp_name = self.unique_var("result")
            cond = self.extract_expr(expr.test, "cond")
            body = self.extract_expr(expr.body, "if_true")
            orelse = self.extract_expr(expr.orelse, "if_false")
            temp_var = ast.Name(id=temp_name, ctx=ast.Load())
            self.statements.append(
                ast.If(
                    test=cond,
                    body=[ast.Assign(targets=[ast.Name(id=temp_name, ctx=ast.Store())], value=body)],
                    orelse=[ast.Assign(targets=[ast.Name(id=temp_name, ctx=ast.Store())], value=orelse)]
                )
            )
            return temp_var
        elif isinstance(expr, (ast.Constant, ast.Name)):
            return expr
        else:
            return self.visit(expr)

    def with_scratchpad(self, fn):
        saved = self.statements
        self.statements = []
        result = fn()
        collected = self.statements
        self.statements = saved
        return collected + ([result] if not isinstance(result, list) else result)


    def visit_UnaryOp(self, node):
        return self.with_scratchpad(lambda: self.handle_unaryop(node))

    def handle_unaryop(self, node):
        operand = self.extract_expr(node.operand, "operand")
        result_var = self.unique_var("result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.UnaryOp(op=node.op, operand=operand)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_BoolOp(self, node):
        return self.with_scratchpad(lambda: self.handle_boolop(node))

    def handle_boolop(self, node):
        values = [self.extract_expr(v, f"boolval_{i}") for i, v in enumerate(node.values)]
        result_var = self.unique_var("bool_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.BoolOp(op=node.op, values=values)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Compare(self, node):
        return self.with_scratchpad(lambda: self.handle_compare(node))

    def handle_compare(self, node):
        left = self.extract_expr(node.left, "cmp_left")
        comparators = [self.extract_expr(c, f"cmp_right_{i}") for i, c in enumerate(node.comparators)]
        result_var = self.unique_var("cmp_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Compare(left=left, ops=node.ops, comparators=comparators)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Call(self, node):
        return self.with_scratchpad(lambda: self.handle_call(node))

    def handle_call(self, node):
        func = self.extract_expr(node.func, "func")
        args = [self.extract_expr(arg, f"arg_{i}") for i, arg in enumerate(node.args)]
        result_var = self.unique_var("call_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Call(func=func, args=args, keywords=node.keywords)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_AugAssign(self, node):
        return self.with_scratchpad(lambda: self.handle_augassign(node))

    def handle_augassign(self, node):
        target = node.target
        value = self.extract_expr(node.value, "augval")
        return ast.AugAssign(target=target, op=node.op, value=value)

    def visit_List(self, node):
        return self.with_scratchpad(lambda: self.handle_list(node))

    def handle_list(self, node):
        elts = [self.extract_expr(e, f"elt_{i}") for i, e in enumerate(node.elts)]
        result_var = self.unique_var("list_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.List(elts=elts, ctx=ast.Load())
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Tuple(self, node):
        return self.with_scratchpad(lambda: self.handle_tuple(node))

    def handle_tuple(self, node):
        elts = [self.extract_expr(e, f"elt_{i}") for i, e in enumerate(node.elts)]
        result_var = self.unique_var("tuple_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Tuple(elts=elts, ctx=ast.Load())
        ))
        return ast.Name(id=result_var, ctx=ast.Load())



    def visit_Return(self, node):
        self.statements = []
        value = self.extract_expr(node.value, "result")
        return_stmts = self.statements + [ast.Return(value=value)]
        self.statements = []
        return return_stmts

    def visit_Assign(self, node):
        self.statements = []
        value = self.extract_expr(node.value)
        assign_stmts = self.statements + [ast.Assign(targets=node.targets, value=value)]
        self.statements = []
        return assign_stmts

    def visit_Expr(self, node):
        self.statements = []
        value = self.extract_expr(node.value)
        expr_stmts = self.statements + [ast.Expr(value=value)]
        self.statements = []
        return expr_stmts


    def visit_If(self, node):
        self.statements = []
        test = self.extract_expr(node.test, "cond")
        new_test = test
        new_body = self.flatten_block(node.body)
        new_orelse = self.flatten_block(node.orelse)
        return self.statements + [ast.If(test=new_test, body=new_body, orelse=new_orelse)]

    def flatten_block(self, block):
        saved = self.statements
        self.statements = []
        result = []
        for stmt in block:
            visited = self.visit(stmt)
            if isinstance(visited, list):
                result.extend(visited)
            else:
                result.append(visited)
        result = self.statements + result
        self.statements = saved
        return result

    def visit_FunctionDef(self, node):
        self.counter = 0
        self.statements = []
        node.body = self.flatten_block(node.body)
        return node
    
    def visit_While(self, node):
        self.statements = []
        test = self.extract_expr(node.test, "cond")
        new_test = test
        new_body = self.flatten_block(node.body)
        return self.statements + [ast.While(test=new_test, body=new_body, orelse=node.orelse)]
    
    def visit_For(self, node):
        self.statements = []
        iter_expr = self.extract_expr(node.iter, "iter")
        new_iter = iter_expr
        new_body = self.flatten_block(node.body)
        return self.statements + [ast.For(target=node.target, iter=new_iter, body=new_body, orelse=node.orelse)]

    def visit_BinOp(self, node):
        self.statements = []
        left = self.extract_expr(node.left, "left")
        right = self.extract_expr(node.right, "right")
        op = node.op
        if isinstance(op, ast.Add):
            new_op = ast.Add()
        elif isinstance(op, ast.Sub):
            new_op = ast.Sub()
        elif isinstance(op, ast.Mult):
            new_op = ast.Mult()
        elif isinstance(op, ast.Div):
            new_op = ast.Div()
        else:
            # give up and let the parent handle it
            return self.generic_visit(node)
        
        result_var = self.unique_var("result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.BinOp(left=left, op=new_op, right=right)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Attribute(self, node):
        return self.with_scratchpad(lambda: self.handle_attribute(node))

    def handle_attribute(self, node):
        value = self.extract_expr(node.value, "obj")
        result_var = self.unique_var("attr_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Attribute(value=value, attr=node.attr, ctx=ast.Load())
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Subscript(self, node):
        return self.with_scratchpad(lambda: self.handle_subscript(node))

    def handle_subscript(self, node):
        value = self.extract_expr(node.value, "container")
        index = self.extract_expr(node.slice, "index")
        result_var = self.unique_var("subscript_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Subscript(value=value, slice=index, ctx=ast.Load())
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Dict(self, node):
        return self.with_scratchpad(lambda: self.handle_dict(node))

    def handle_dict(self, node):
        keys = [self.extract_expr(k, f"key_{i}") for i, k in enumerate(node.keys)]
        values = [self.extract_expr(v, f"val_{i}") for i, v in enumerate(node.values)]
        result_var = self.unique_var("dict_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Dict(keys=keys, values=values)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Set(self, node):
        return self.with_scratchpad(lambda: self.handle_set(node))

    def handle_set(self, node):
        elts = [self.extract_expr(e, f"set_elt_{i}") for i, e in enumerate(node.elts)]
        result_var = self.unique_var("set_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Set(elts=elts)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Try(self, node):
        return self.with_scratchpad(lambda: self.handle_try(node))

    def handle_try(self, node):
        body = self.flatten_block(node.body)
        handlers = []
        for h in node.handlers:
            new_handler = ast.ExceptHandler(
                type=self.visit(h.type) if h.type else None,
                name=h.name,
                body=self.flatten_block(h.body)
            )
            handlers.append(new_handler)
        finalbody = self.flatten_block(node.finalbody)
        orelse = self.flatten_block(node.orelse)
        return ast.Try(body=body, handlers=handlers, orelse=orelse, finalbody=finalbody)

    def visit_ListComp(self, node):
        return self.with_scratchpad(lambda: self.handle_comp(node, ast.ListComp))

    def visit_SetComp(self, node):
        return self.with_scratchpad(lambda: self.handle_comp(node, ast.SetComp))

    def visit_DictComp(self, node):
        return self.with_scratchpad(lambda: self.handle_comp(node, ast.DictComp))

    def visit_GeneratorExp(self, node):
        return self.with_scratchpad(lambda: self.handle_comp(node, ast.GeneratorExp))

    def handle_comp(self, node, node_type):
        result_var = self.unique_var("comp_result")
        elt = self.extract_expr(getattr(node, "elt", None) or node.key)
        if isinstance(node, ast.DictComp):
            value = self.extract_expr(node.value)
            comp_node = node_type(key=elt, value=value, generators=node.generators)
        else:
            comp_node = node_type(elt=elt, generators=node.generators)

        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=comp_node
        ))
        return ast.Name(id=result_var, ctx=ast.Load())
    def visit_Lambda(self, node):
        return self.with_scratchpad(lambda: self.handle_lambda(node))

    def handle_lambda(self, node):
        func_name = self.unique_var("lambda_fn")
        args = node.args
        body_expr = self.extract_expr(node.body, "lambda_body")

        # Build function body: return $body_expr
        new_func = ast.FunctionDef(
            name=func_name,
            args=args,
            body=[ast.Return(value=body_expr)],
            decorator_list=[]
        )
        self.statements.append(new_func)
        return ast.Name(id=func_name, ctx=ast.Load())

    def visit_Yield(self, node):
        return self.with_scratchpad(lambda: self.handle_yield(node))

    def handle_yield(self, node):
        value = self.extract_expr(node.value, "yield_val") if node.value else None
        result_var = self.unique_var("yield_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Yield(value=value)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_YieldFrom(self, node):
        return self.with_scratchpad(lambda: self.handle_yieldfrom(node))

    def handle_yieldfrom(self, node):
        value = self.extract_expr(node.value, "yield_from_val")
        result_var = self.unique_var("yield_from_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.YieldFrom(value=value)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Await(self, node):
        return self.with_scratchpad(lambda: self.handle_await(node))

    def handle_await(self, node):
        value = self.extract_expr(node.value, "awaited")
        result_var = self.unique_var("await_result")
        self.statements.append(ast.Assign(
            targets=[ast.Name(id=result_var, ctx=ast.Store())],
            value=ast.Await(value=value)
        ))
        return ast.Name(id=result_var, ctx=ast.Load())

    def visit_Assert(self, node):
        return self.with_scratchpad(lambda: self.handle_assert(node))

    def handle_assert(self, node):
        test = self.extract_expr(node.test, "assert_test")
        msg = self.extract_expr(node.msg, "assert_msg") if node.msg else None
        return ast.Assert(test=test, msg=msg)

    def visit_Raise(self, node):
        return self.with_scratchpad(lambda: self.handle_raise(node))

    def handle_raise(self, node):
        exc = self.extract_expr(node.exc, "raise_expr") if node.exc else None
        cause = self.extract_expr(node.cause, "raise_cause") if node.cause else None
        return ast.Raise(exc=exc, cause=cause)

    def visit_Break(self, node):
        return node  # no transformation needed

    def visit_Continue(self, node):
        return node  # no transformation needed

    def visit_Pass(self, node):
        return node  # no transformation needed

    def visit_Global(self, node):
        # Typically safe to pass through — may be needed for accurate scoping
        return node

    def visit_Nonlocal(self, node):
        # Like `global`, treat as a no-op for transformation
        return node

    def visit_Delete(self, node):
        return self.with_scratchpad(lambda: self.handle_delete(node))

    def handle_delete(self, node):
        targets = [self.extract_expr(t, "del_target") for t in node.targets]
        return ast.Delete(targets=targets)

    def visit_With(self, node):
        return self.with_scratchpad(lambda: self.handle_with(node, ast.With))

    def visit_AsyncWith(self, node):
        return self.with_scratchpad(lambda: self.handle_with(node, ast.AsyncWith))

    def handle_with(self, node, node_type):
        items = []
        for item in node.items:
            context_expr = self.extract_expr(item.context_expr, "with_expr")
            optional_vars = item.optional_vars
            items.append(ast.withitem(context_expr=context_expr, optional_vars=optional_vars))
        body = self.flatten_block(node.body)
        return node_type(items=items, body=body)

    def visit_ClassDef(self, node):
        return self.with_scratchpad(lambda: self.handle_classdef(node))

    def handle_classdef(self, node):
        body = self.flatten_block(node.body)
        bases = [self.extract_expr(b, f"base_{i}") for i, b in enumerate(node.bases)]
        keywords = node.keywords
        return ast.ClassDef(
            name=node.name,
            bases=bases,
            keywords=keywords,
            body=body,
            decorator_list=node.decorator_list
        )

    def visit_Import(self, node):
        # Track symbol bindings if desired, otherwise no-op
        return node

    def visit_ImportFrom(self, node):
        # Track imports if desired, otherwise no-op
        return node

    def visit_Global(self, node):
        self.introduced_names.update(node.names)
        return node

    def visit_Nonlocal(self, node):
        self.introduced_names.update(node.names)
        return node

    def visit_Import(self, node):
        for alias in node.names:
            self.introduced_names.add(alias.asname or alias.name.split('.')[0])
        return node

    def visit_ImportFrom(self, node):
        for alias in node.names:
            self.introduced_names.add(alias.asname or alias.name)
        return node

    def visit_ClassDef(self, node):
        self.introduced_names.add(node.name)
        body = self.flatten_block(node.body)
        bases = [self.extract_expr(b, f"base_{i}") for i, b in enumerate(node.bases)]
        keywords = node.keywords
        return ast.ClassDef(
            name=node.name,
            bases=bases,
            keywords=keywords,
            body=body,
            decorator_list=node.decorator_list
        )

    def visit_With(self, node):
        return self.with_scratchpad(lambda: self.expand_with(node, ast.With))

    def visit_AsyncWith(self, node):
        return self.with_scratchpad(lambda: self.expand_with(node, ast.AsyncWith))


    def expand_with(self, node, node_type):
        preamble = []
        new_items = []
        for i, item in enumerate(node.items):
            ctx_expr = self.extract_expr(item.context_expr, f"with_expr_{i}")
            if item.optional_vars:
                if isinstance(item.optional_vars, ast.Name):
                    self.introduced_names.add(item.optional_vars.id)
                assign = ast.Assign(targets=[item.optional_vars], value=ctx_expr)
                preamble.append(assign)
            else:
                new_items.append(ast.withitem(context_expr=ctx_expr, optional_vars=None))
        body = self.flatten_block(node.body)
        if new_items:
            return preamble + [node_type(items=new_items, body=body)]
        else:
            return preamble + body

def main():
    if len(sys.argv) != 3:
        print("Usage: python expand_with_ast.py <input.py> <output.py>")
        sys.exit(1)

    input_path, output_path = sys.argv[1], sys.argv[2]
    with open(input_path) as f:
        source = f.read()
    tree = ast.parse(source)

    transformer = FullPythonUncompressor()
    transformed_tree = transformer.visit(tree)
    ast.fix_missing_locations(transformed_tree)

    expanded_code = ast.unparse(transformed_tree)

    with open(output_path, 'w') as f:
        f.write(expanded_code)

    print(f"✅ Expanded AST code written to {output_path}")

if __name__ == "__main__":
    main()