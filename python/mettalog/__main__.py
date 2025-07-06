import requests
import json
import mettalog 


from mettalog import MeTTaLog, mettalog
from mettalog import ValueAtom
from mettalog.hyperon import Atom
#from mettalog.core import quick_selftest
#import mettalog.dynamic
#.hyperon import V
#from mettalog.dynamic import S
from mettalog.hyperon.ext import E, MeTTa, register_atoms, Environment, Char, V, S
#from mettalog.dynamic.hyperon.stdlib import Char
#from mettalog.dynamic.hyperon import *


def demo():
    print("\n=== 🚀 MeTTaLog DEMO START ===\n")

    root = MeTTaLog()

    root.api.version = "v1"
    root.config["timeout"] = 42
    root.user.name = "douglas"

    chain = root.agent.control.panel.button
    result1 = root.session.auth.login("douglas", password="secret")
    result2 = root("eval").math.sqrt(9)
    response = root["x"]["y"].z(1, 2)

    print("\nAssigned Values:")
    print("api.version      →", root.api.version)
    print("config['timeout']→", root.config["timeout"])
    print("user.name        →", root.user.name)

    print("\nCall Results:")
    print("login(...) →", result1)
    print("eval(math.sqrt) →", result2)
    print("z(1,2) →", response)

    print("\n=== Trace Report ===")
    MeTTaLog.report()

    print("\n=== Atomspace Facts ===")
    print(MeTTaLog.to_atomspace())

    print("\n=== ✅ MeTTaLog DEMO COMPLETE ===")

def demo_dynamic():
    print("\n=== 🪄 demo_dynamic(): Dynamic Import from mettalog.dynamic ===\n")

    from mettalog.dynamic import Whatnot, FooBar, Spam

    # Phantom calls on dynamically generated symbols
    Whatnot.service.activate("X1")
    FooBar.network.connect(ip="127.0.0.1", port=8080)
    result = Spam("special").engine.startup(mode="silent")

    # Show trace
    from mettalog import MeTTaLog
    print("\n--- AtomSpace from demo_dynamic() ---")
    print(MeTTaLog.to_atomspace())

    print("\n=== ✅ demo_dynamic() COMPLETE ===")

def demo_hyperonic():
    #from mettalog.dynamic.hyperon.ext import register_atoms

    '''
    This is the very preliminary wrapper to the Kotlin-based
    MeTTa compiler, Jetta. For this gate to work, Jetta
    should be installed via clonning https://github.com/trueagi-io/jetta
    and running Application.kt of `server` subproject.
    '''

    default_url_base = 'http://0.0.0.0:9090/contexts'

    class JettaServerError(RuntimeError):
        pass

    def jetta(j_space_id: str, code: str, url=None):
        """
        The very basic caller to Jetta server with purely Python interface.
        """
        if url is None:
            url = default_url_base
        r = requests.post(url + "/" + j_space_id, data=code)
        if r.status_code != 200:
            raise JettaServerError(f"Status code: {r.status_code}")
        r = json.loads(r.content.decode())
        if not r['isSuccess']:
            raise JettaServerError(r['messages'])
        if r['type'] == 'java.lang.Integer':
            r['result'] = int(r['result'])
        # NOTE: disambiguation is needed if java.util.ArrayList is used
        #       as a grounded result instead of non-deterministic result
        return r['result'] if r['type'] == 'java.util.ArrayList' \
            else [r['result']]

    def _err_msg(expr, msg):
        if not isinstance(expr, Atom):
            expr = ValueAtom(str(expr))
        if not isinstance(msg, Atom):
            msg = ValueAtom(str(msg))
        return [E(S('Error'), expr, E(S('JettaCompileError'), msg))]

    def jetta_unwrap_atom(j_space_a: Atom, code_a: Atom,
                          url_a=ValueAtom(None)):
        """
        The caller to Jetta server with atom wrapping and unwrapping.
        This is needed to pass MeTTa expressions to `jetta` as well
        as to convert JettaServerError into ordinary MeTTa Error
        atom without Python error log.
        """
        j_space = j_space_a.get_object().value
        if isinstance(code_a, GroundedAtom):
            code_a = code_a.get_object().value
        if not isinstance(code_a, str):
            code_a = repr(code_a)
        url = url_a.get_object().value
        try:
            result = jetta(j_space, code_a, url)
            # NOTE: handling symbols and expressions will be needed at some point
            return [Atoms.UNIT if r is None else ValueAtom(r) for r in result]
        except JettaServerError as e:
            return _err_msg(code_a, e)
            #return [E(S('Error'), ValueAtom(code),
            #          E(S('JettaCompileError'), ValueAtom(str(e))))]

    def compile(metta: MeTTa, j_space_a, func_a, arity=None):
        code = ""
        # Get the function name
        j_space = j_space_a.get_object().content
        if arity is not None:
            arity = arity.get_object().content
        if isinstance(func_a, GroundedAtom):
            func = str(func_a.get_object().content)
        elif not isinstance(func_a, SymbolAtom):
            return _err_msg(func_a, "compile expects a function name")
        else:
            func = repr(func_a)

        # Get annotations (if any)
        annotations = metta.space().query(
            E(S('@'), S(func), V('$ann'))
        )
        for a in annotations:
            code += f"(@ {func} {repr(a['$ann'])})\n"

        # Get the type
        typ = metta.space().query(
            E(S(':'), S(func), V('t'))
        )
        typ = list(typ)
        assert len(typ) < 2, "Non-deterministic types are not supported yet"
        # TODO: different arities can be tried
        if len(typ) == 0:
            typ = ""
            if arity is None:
                return _err_msg(E(S('compile'), j_space_a, func_a),
                                "If type is not defined, arity should be provided" )
                #return [E(S('Error'), E(S('compile'), j_space_a, func_a),
                #    E(S('JettaCompileError'), ValueAtom("If type is not defined, arity should be provided")))]
        else:
            typ = typ[0]['t']
            arity = len(typ.get_children()) - 2
            typ = f"(: {func} {repr(typ)})\n"
        code += typ

        f_args = E(S(func), *[V(f'x{i}') for i in range(arity)])
        res = metta.space().query(
            E(S('='), f_args, V('__r'))
        )
        res = list(res)
        assert len(res) == 1, "Functions with one equality are allowed for now"
        code += "(= " + repr(f_args) + "\n   " +\
              repr(res[0]['__r']) + ")\n"
        # TODO: check if compilation is successful
        jetta(j_space, code)
        #TODO: doesn't work for passing expressions (e.g. lambdas)
        funcAtom = OperationAtom(func,
            lambda *args: jetta_unwrap_atom(j_space_a, E(S(func), *args)),
            unwrap=False)
        metta.register_atom(func+'-gnd', funcAtom)
        return [Atoms.UNIT]

    def jetta_space(url=default_url_base):
        r = requests.post(url)
        assert r.status_code == 200, "Failed to create jetta space"
        return r.content.decode()

    @register_atoms(pass_metta=True)
    def jettaspace_atoms(metta: MeTTa):
        newJSpaceAtom = OperationAtom('new-jetta-space', jetta_space)
        jettaAtom = OperationAtom('jetta', jetta_unwrap_atom, unwrap=False)
        compileAtom = OperationAtom('compile',
            lambda *args: compile(metta, *args), unwrap=False)
        return {
            r"new-jetta-space": newJSpaceAtom,
            r"jetta": jettaAtom,
            r"compile": compileAtom
        }

    print("\n=== 🪄 demo_hyperonic(): Hyperonic Jetta ===\n"
          "This is a preliminary wrapper to the Kotlin-based MeTTa compiler, Jetta.\n"
          "For this gate to work, Jetta should be installed via cloning "
          )

    metta = MeTTa()
    test_file = "test_load.metta"
    metta.run(f'''
        !(bind! &space (new-space))
        !(load-ascii &space {test_file})
    ''')
    content = metta.run("!(match &space $x $x)")[0]
    #with open(test_file) as f:
    #    self.assertEqualNoOrder(metta.parse_all(f.read()),
    #                            content)

    metta = MeTTa(env_builder=Environment.test_env())

    def assertEqualMettaRunnerResults(a, b):
        if isinstance(a, MeTTaLog):
            return a.EQUALS(b)
        #print(type(a))
        

    # Check that (repr (my atom)) == "(my atom)"
    assertEqualMettaRunnerResults(metta.run("!(repr (my atom))"),
                                      [[ValueAtom("(my atom)")]])

    # Check that (parse "(my atom)") == (my atom)
    assertEqualMettaRunnerResults(metta.run("!(parse \"(my atom)\")"),
                                      [[E(S("my"), S("atom"))]])

    #unstable renaming of variables causes random failures of the test
    assertEqualMettaRunnerResults(metta.run('!(parse "$X")'),
                                                                    [[(V("X"))]])

    assertEqualMettaRunnerResults(metta.run('!(parse "\\"A\\"")'),
                                              [[(ValueAtom("A"))]])

    assertEqualMettaRunnerResults(metta.run('!(parse "(func (Cons $x (Cons $xs $xss))) ")'),
                                       [[E(S("func"), E(S("Cons"), V("x"), E(S("Cons"), V("xs"), V("xss"))))]])

    assertEqualMettaRunnerResults(metta.run('!(parse "(A 2 \'S\')")'),
                              [[E(S("A"), ValueAtom(2), ValueAtom(Char("S")))]])

    # Check that (stringToChars "ABC") == ('A' 'B' 'C')
    assertEqualMettaRunnerResults(metta.run('!(stringToChars "ABC")'),
                                      [[E(ValueAtom(Char("A")), ValueAtom(Char("B")), ValueAtom(Char("C")))]])

    # Check that (charsToString ('A' 'B' 'C')) == "ABC"
    assertEqualMettaRunnerResults(metta.run("!(charsToString ('A' 'B' 'C'))"),
                                      [[ValueAtom("ABC")]])

    pass

if __name__ == "__main__":
    demo()
    demo_dynamic()
    #demo_hyperonic()
    print(MeTTaLog.to_atomspace())
    print(MeTTaLog.report())
    #print(mettalog.core.quick_selftest())
