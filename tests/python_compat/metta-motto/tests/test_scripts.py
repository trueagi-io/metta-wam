from hyperon import MeTTa, E
from pathlib import Path
pwd = Path(__file__).parent

def process_exceptions(results):
    for result in results:
        assert result in [[E()], []]

def run_script(fname):
    with open(fname) as f:
        return MeTTa().run(f.read())

def test_scripts():
    process_exceptions(run_script(f"{pwd}/basic_direct_call.metta"))
    process_exceptions(run_script(f"{pwd}/basic_function_call.metta"))
    process_exceptions(run_script(f"{pwd}/basic_agent_stateful.metta"))
    process_exceptions(run_script(f"{pwd}/basic_agent_script.metta"))
    process_exceptions(run_script(f"{pwd}/basic_script_call.metta"))
    process_exceptions(run_script(f"{pwd}/basic_agent_call.metta"))
    process_exceptions(run_script(f"{pwd}/metta_chat.metta"))
    process_exceptions(run_script(f"{pwd}/nested_script_direct.metta"))
    process_exceptions(run_script(f"{pwd}/nested_dialog_call.metta"))
    process_exceptions(run_script(f"{pwd}/sparql_functions_test.metta"))
