# -*- coding: utf-8 -*-

import os
import sys
import logging
import tempfile

from metakernel import MetaKernel
from ipykernel.kernelbase import Kernel
from pexpect import replwrap, EOF
from subprocess import check_output
from metta_learner  import ExtendedMeTTa

import re
import signal

crlf_pat = re.compile(r'[\r\n]+')

def flush_console():
    try:
      sys.stdout.flush()
    except Exception: ""
    try:
      sys.stderr.flush()
    except Exception: ""

class MeTTaKernel(Kernel):
    implementation = 'MeTTa'
    implementation_version = '0.16'
    language = 'MeTTa'
    language_version = '0.16'

    language_info = {
        'name': 'MeTTa',
        'codemirror_mode': 'scheme',
        'mimetype': 'text/plain',
        'file_extension': '.metta'
    }

    banner = "An Jupyter kernel for MeTTa - version %s" % implementation_version

    def __init__(self, **kwargs):
        super(MeTTaKernel,self).__init__(**kwargs)
        self._start_MeTTa()

    def _start_MeTTa(self):
        sig = signal.signal(signal.SIGINT, signal.SIG_DFL)
        try:
            #self.MeTTa = replwrap.REPLWrapper("MeTTa", "MeTTa> ", None)
            self.MeTTa = ExtendedMeTTa()
        finally:
            signal.signal(signal.SIGINT, sig)

    def do_execute(self, code, silent, store_history=True,
                   user_expressions=None, allow_stdin=False):
        code = crlf_pat.sub(' ', code.strip())
        if not code:
            return {'status': 'ok', 'execution_count': self.execution_count,
                    'payload': [], 'user_expressions': {}}

        interrupted = False
        outputM = self.MeTTa.run(code)
        try:
            output = f"{outputM}"
            print("output="+output, file=sys.stderr)
            if sys.__stdout__ is not None: print("output="+output, file=sys.__stdout__)
            flush_console()
            #output = self.MeTTa.run_command(code, timeout=None)
        except KeyboardInterrupt:
            self.MeTTa.child.sendintr()
            interrupted = True
            self.MeTTa._expect_prompt()
            output = self.MeTTa.child.before
        except EOF:
            output = self.MeTTa.child.before + 'Restarting MeTTa, use CRTL+D to exit'
            self._start_MeTTa()

        if not silent:
            # Send standard output
            stream_content = {'name': 'stdout', 'text': "\r\n".join(output.splitlines()[1:])}
            self.send_response(self.iopub_socket, 'stream', stream_content)

        if interrupted:
            return {'status': 'abort', 'execution_count': self.execution_count}

        return {'status': 'ok', 'execution_count': self.execution_count,
                'payload': [], 'user_expressions': {}}



from jupyter_client.kernelspec import KernelSpecManager

from metakernel import MetaKernel
# from calysto_metta import metta

try:
    from IPython.core.latex_symbols import latex_symbols
    from IPython.display import Image
    #from IPython.utils import io
except Exception:
    Image = None
    latex_symbols = []

try:
    import yasi
    yasi.IF_LIKE = [] ## removed "if" so indents then-part and else-part the same
    opts = yasi.parse_args([])
    opts.dialect = "metta"
except:
    yasi = None

PY3 = (sys.version_info[0] >= 3)
if PY3:
    PY_STRINGS = (str,)
else:
    PY_STRINGS = (str, unicode)

ENVIRONMENT = {}

class MeTTaRepl(MetaKernel):
    global ENVIRONMENT
    implementation = 'metta'
    implementation_version = '0.16'
    language = 'metta'
    language_version = '0.16'
    banner = "MeTTa %s" % implementation_version
    language_info = {
        'mimetype': 'text/x-metta',
        'name': 'metta',
        'codemirror_mode': {'name': 'scheme'},
        'pygments_lexer': 'scheme',
    }

    kernel_json = {
        "argv": ["python3",
                 "-m", "metta_jupyter_kernel",
                 "-f", "{connection_file}"],
        "display_name": "MeTTa %i" % (3 if PY3 else 2),
        "language": "metta",
        "codemirror_mode": "metta",
        "name": "calysto_metta"
    }
    identifier_regex = r'\\?[\w\.][\w\.\?\!\-\>\<]*'
    function_call_regex = r'\(([\w\.][\w\.\?\!\-\>\>]*)[^\)\()]*\Z'
    magic_prefixes = dict(magic='%', shell='!', help='?')
    help_suffix = None

    def __init__(self, *args, **kwargs):
        super(MeTTaRepl, self).__init__(*args, **kwargs)
        ## self.log.setLevel(logging.DEBUG)
        ENVIRONMENT["raw_input"] = self.raw_input
        ENVIRONMENT["read"] = self.raw_input
        ENVIRONMENT["input"] = self.raw_input

        try:
            import getpass
            getpass.getpass = self.raw_input
        except Exception:
            pass

    def get_usage(self):
        return """MeTTa
=========================================

MeTTa offers a combination of convenient shell features,
special commands and a history mechanism for both input (command
history) and output (results caching, similar to Mathematica).

MAIN FEATURES
-------------

* Magic commands: type %magic for information on the magic subsystem.

* Dynamic object information:

  Typing ?word prints detailed information about an object.  If
  certain strings in the object are too long (docstrings, code, etc.) they get
  snipped in the center for brevity.

  Typing ??word gives access to the full information without
  snipping long strings. Long strings are sent to the screen through the less
  pager if longer than the screen, printed otherwise.

* Completion in the local namespace, by typing TAB at the prompt.

  At any time, hitting tab will complete any available commands or
  variable names, and show you a list of the possible completions if there's
  no unambiguous one. It will also complete filenames in the current directory.

  This feature requires the readline and rlcomplete modules, so it won't work
  if your system lacks readline support (such as under Windows).

* Search previous command history in two ways (also requires readline):

  - Start typing, and then use Ctrl-p (previous,up) and Ctrl-n (next,down) to
    search through only the history items that match what you've typed so
    far. If you use Ctrl-p/Ctrl-n at a blank prompt, they just behave like
    normal arrow keys.

  - Hit Ctrl-r: opens a search prompt. Begin typing and the system searches
    your history for lines that match what you've typed so far, completing as
    much as it can.

  - %hist: search history by index (this does *not* require readline).

* Persistent command history across sessions.

* Logging of input with the ability to save and restore a working session.

* System escape with !. Typing !ls will run 'ls' in the current directory.

* Input caching system:

  Offers numbered prompts (In/Out) with input and output caching. All
  input is saved and can be retrieved as variables (besides the usual arrow
  key recall).

  The following GLOBAL variables always exist (so don't overwrite them!):
  _i: stores previous input.
  _ii: next previous.
  _iii: next-next previous.
  _ih : a list of all input _ih[n] is the input from line n.

  Additionally, global variables named _i<n> are dynamically created (<n>
  being the prompt counter), such that _i<n> == _ih[<n>]

  For example, what you typed at prompt 14 is available as _i14 and _ih[14].

* Output caching system:

  For output that is returned from actions, a system similar to the input
  cache exists but using _ instead of _i. Only actions that produce a result
  (NOT assignments, for example) are cached. If you are familiar with
  Mathematica, Jupyter's _ variables behave exactly like Mathematica's %
  variables.

  The following GLOBAL variables always exist (so don't overwrite them!):
  _ (one underscore): previous output.
  __ (two underscores): next previous.
  ___ (three underscores): next-next previous.

  Global variables named _<n> are dynamically created (<n> being the prompt
  counter), such that the result of output <n> is always available as _<n>.

  Finally, a global dictionary named _oh exists with entries for all lines
  which generated output.

* Directory history:

  Your history of visited directories is kept in the global list _dh, and the
  magic %cd command can be used to go to any entry in that list.
"""

    def get_completions(self, info):
        token = info["help_obj"]
        matches = []
        # from latex
        matches = latex_matches(token)
        if matches:
            return matches
        # from the language environment:
        slist = metta.execute_string_rm("(get-completions)")
        if not metta.exception_q(slist):
            for item in slist:
                item_str = str(item)
                if item_str.startswith(token) and item_str not in matches:
                    matches.append(item_str)
        # special forms and constants:
        for item in ["add-atom", "remove-atom", "match", "let", "let*",
                     "!", "=", ":", "&",
                     "superpose", "collapse", "&self", "if", "case", "nop",
                     "init-state", "bind!", "init-space"]:
            if item.startswith(token) and item not in matches:
                matches.append(item)
        # add items from ENVIRONMENT
        for item in ENVIRONMENT:
            if item.startswith(token) and item not in matches:
                matches.append(item)
        # add properties and attributes if token is "numpy.ar"
        if "." in token:
            components, partial = token.rsplit(".", 1)
            slist = metta.execute_string_rm("(get-completions %s)" % components)
            if not metta.exception_q(slist):
                for item in slist:
                    item_str = str(item)
                    if item_str.startswith(partial) and item_str not in matches:
                        matches.append(components + "." + item_str)
        # done with language-specific completitions
        return matches

    def set_variable(self, name, value):
        """
        Set a variable in the kernel's enviroment.
        """
        ENVIRONMENT[name] = value

    def get_variable(self, name):
        """
        Get a variable in the kernel's enviroment.
        """
        # search through the local env, if one
        reg_env = metta.GLOBALS["env_reg"]
        if reg_env:
            def get_index(item, ls):
                pos = 0
                while ls != metta.Symbol("()"):
                    if item == ls.car:
                        return pos
                    ls = ls.cdr
                return None
            symbol_name = metta.Symbol(name)
            # car 'environment
            # cadr frame: (vector of vars, names)
            current_frame = reg_env = reg_env.cdr
            while current_frame != metta.Symbol("()"):
                if not hasattr(current_frame, "car"): break
                values = current_frame.car.car # vector of bindings (val . docstring)
                names = current_frame.car.cdr.car # list
                if symbol_name in names:
                    index = get_index(symbol_name, names)
                    return values[index].car
                current_frame = current_frame.cdr
        # if not found, search through ENVIRONMENT:
        if name in ENVIRONMENT:
            return ENVIRONMENT[name]

    def get_kernel_help_on(self, info, level=0, none_on_fail=False):
        expr = info["help_obj"]
        if expr == "":
            return None
        result = metta.execute_string_rm("(help %s)" % expr)
        if not metta.exception_q(result):
            return result
            #return {"text/html": "<b>%s</b>" % result}
        elif expr in ["define", "define!", "func", "callback", "if",
                     "help", "define-syntax", "begin", "lambda", "trace-lambda",
                     "try", "catch", "finally", "raise", "choose"]:
            help_text = {
                "define": "(define NAME [DOCSTRING] VALUE) define a global variable (special form)",
                "define!": "(define! NAME [DOCSTRING] VALUE) define a variable in the host system (special form)",
                "func": "(func PROCEDURE) for wrapping Metta procedures as a system function (special form)",
                "callback": "(callback PROCEDURE) returns a host system function for system callbacks (special form)",
                "if": "(if TEXT-EXPR TRUE-EXPR FALSE-EXPR) (special form)",
                "help": "(help ITEM) (special form)",
                "define-syntax": "(define-syntax NAME NAME-TEST ...) (special form)",
                "begin": "(begin EXPR...) (special form)",
                "lambda": "(lambda (VAR...) EXPR...) or (lambda VAR EXPR...) (special form)",
                "trace-lambda": "(trace-lambda NAME (VAR...) EXPR...) or (trace-lambda NAME VAR EXPR...) (special form)",
                "try": "(try EXPR (catch EXCEPTION NAME ...)...) (special form)",
                "catch": "(try EXPR (catch EXCEPTION NAME ...) ...) (special form)",
                "finally": "(try EXPR (catch EXCEPTION NAME ...)... (finally ...)) (special form)",
                "raise": "(raise EXCEPTION) (special form)",
                "choose": "Use (choose ITEM...) to setup non-deterministic interpreter, or use (choose) to go to next choice (special form)"
            }
            return help_text[expr]
        elif none_on_fail:
            return None
        else:
            return "No available help on '%s'" % expr

    def repr(self, item):
        if isinstance(item, list): # a metta vector
            items = " ".join(map(self.repr, item))
            try:
                return "#%d(%s)" % (len(item), items)
            except Exception as e:
                return str(e)
        elif isinstance(item, metta.cons): # a metta list
            if isinstance(item.car, metta.Symbol):
                ## HACK: fix me; represent procedues and environments as objs?
                if item.car.name == "procedure":
                    return "#<procedure>"
                elif item.car.name == "environment":
                    return "#<environment>"
                elif item.car.name == "exception-object":
                    return "#<exception>"
            else: # a pair
                retval = []
                current = item
                while isinstance(current, metta.cons):
                    ## HACK: fix me; represent procedues and environments as objs?
                    if hasattr(current.car, "name"):
                        if current.car.name == "procedure":
                             return "(%s)" % ((" ".join(retval)) + " . #<procedure>")
                        elif current.car.name == "environment":
                             return "(%s)" % ((" ".join(retval)) + " . #<environment>")
                        elif current.car.name == "exception-object":
                             return "(%s)" % ((" ".join(retval)) + " . #<exception>")
                    retval.append(self.repr(current.car))
                    current = current.cdr
                retval = " ".join(retval)
                if not (isinstance(current, metta.Symbol) and
                        current.name == "()"):
                    retval += " . " + self.repr(current)
                return "(%s)" % retval
        elif isinstance(item, PY_STRINGS):
            retval = repr(item)
            if retval.startswith("'"):
                retval = retval.replace('"', '\\"')
                retval = retval.replace('\n', '\\n')
                return '"' + retval[1:-1] + '"'
        # FIXME: newlines, chars?
        elif isinstance(item, bool):
            return '#t' if item else '#f'
        return repr(item)

    def do_execute_file(self, filename):
        # for the %run FILENAME magic
        retval = metta.execute_file_rm(filename);
        if metta.exception_q(retval):
            traceback = metta.get_traceback_string(retval)
            ename, evalue = metta.get_exception_values(retval)
            self.Error(traceback)
        return None

    def do_execute_direct(self, code):
        try:
            retval = metta.execute_string_top(code, "In [%s]" % self.execution_count)
        except:
            return "Unhandled Error: " + code
        if metta.exception_q(retval):
            traceback = metta.get_traceback_string(retval)
            ename, evalue = metta.get_exception_values(retval)
            self.Error(traceback)
            self.kernel_resp.update({
                "status": "error",
                'ename' : ename,   # Exception name, as a string
                'evalue' : evalue,  # Exception value, as a string
                'traceback' : [line + "\n" for line in traceback.split("\n")], # traceback frames as strings
            })
            retval = None
        if retval is metta.void_value:
            retval = None
        elif metta.end_of_session_q(retval):
            self.Print("Use ^D to exit from console; use 'Shutdown Kernel' for other Jupyter frontends.")
            retval = None
        return retval

    def do_function_direct(self, function_name, arg):
        f = self.do_execute_direct(function_name)
        return f(arg)

    def initialize_debug(self, code):
        self.original_debug_code = code
        self.running = True
        metta._startracing_on_q_star = True
        metta.GLOBALS["TRACE_GUI"] = True
        metta.GLOBALS["TRACE_GUI_COUNT"] = 0
        try:
            retval = metta.execute_string_rm(code)
        except metta.DebugException as e:
            retval = "highlight: [%s, %s, %s, %s]" % (e.data[0], e.data[1], e.data[2], e.data[3])
        except:
            return "Unhandled Error: " + code
        return retval

    def do_execute_meta(self, code):
        if code == "reset":
            return self.initialize_debug(self.original_debug_code)
        elif code == "stop":
            self.running = False
            metta._startracing_on_q_star = False
            metta.GLOBALS["TRACE_GUI"] = False
        elif code == "step":
            if not self.running:
                metta._startracing_on_q_star = False
                metta.GLOBALS["TRACE_GUI"] = False
                raise StopIteration()
            try:
                metta.m()
                retval = metta.trampoline()
            except metta.DebugException as e:
                if metta.pc:
                    return "highlight: [%s, %s, %s, %s]" % (e.data[0], e.data[1], e.data[2], e.data[3])
                else:
                    self.running = False
            except:
                return "Unhandled Error: " + code
        elif code.startswith("inspect "):
            variable = code[8:].strip()
            self.Print("%s => %s" % (variable, self.repr(self.get_variable(variable))))
        return None

    def do_is_complete(self, code):
        # status: 'complete', 'incomplete', 'invalid', or 'unknown'
        ## First, remove magic lines:
        ## need to distinguish between "%magic", and "%magic\n"
        if code.startswith("%"):
            lines = code.split("\n")
            start_line = 0
            for line in lines:
                if not line.startswith("%"):
                    break
                start_line += 1
            code = ("\n".join(lines[start_line:]))
            if lines[-1] == '':
                return {'status' : 'complete'}
            elif code == '':
                return {'status' : 'incomplete'}
        if yasi is not None:
            data = yasi.indent_code(code + "\n(", opts) ## where does next expression go?
            if data["indented_code"][-1] == "(":
                return {'status' : 'complete'}
            else:
                return {'status' : 'incomplete',
                        'indent': data["indented_code"][-1][:-1]}
        elif metta.ready_to_eval(code):
            return {'status' : 'complete'}
        else:
            return {'status' : 'incomplete',
                    'indent': ' ' * 4}

    def handle_plot_settings(self):
        """
        Override of base method. Set up matplotlib plotting options
        """
        try:
            import matplotlib
            import matplotlib.pyplot as plt
        except Error:
            matplotlib = None

        if matplotlib is not None:
            if self.plot_settings["backend"] == "inline":
                matplotlib.use("Agg")

                def show():
                    with tempfile.NamedTemporaryFile(suffix=".png") as fp:
                        plt.savefig(fp.name)
                        if Image is not None:
                            return Image(fp.name)
                        else:
                            return None

                plt.show = show


def latex_matches(text):
    """
    Match Latex syntax for unicode characters.
    After IPython.core.completer
    """
    slashpos = text.rfind('\\')
    if slashpos > -1:
        s = text[slashpos:]
        if s in latex_symbols:
            # Try to complete a full latex symbol to unicode
            return [latex_symbols[s]]
        else:
            # If a user has partially typed a latex symbol, give them
            # a full list of options \al -> [\aleph, \alpha]
            matches = [k for k in latex_symbols if k.startswith(s)]
            return matches
    return []




if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=MeTTaKernel)

