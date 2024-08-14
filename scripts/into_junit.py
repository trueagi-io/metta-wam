import xml.etree.ElementTree as ET
import sys
import re
from collections import defaultdict

def create_testcase_element(testclass, testname, stdout, identifier, got, expected, status, url):
    # Create the testcase XML element with the class and test name attributes
    testcase = ET.Element("testcase", classname=testclass, name=testname)

    if status == "PASS":
        # If the test passed, add system-out with a clickable link and details
        system_out = ET.SubElement(testcase, "system-out")
        system_out.text = f"<![CDATA[\n<a href=\"{url}\">Test Report</a>\n\nAssertion: {stdout}\nExpected: {expected}\nActual: {got}\n]]>"
    else:  # status == "FAIL"
        # If the test failed, add a failure element with details and a clickable link
        failure_message = f"Test failed: Expected '{expected}' but got '{got}'"
        failure = ET.SubElement(testcase, "failure", message=failure_message, type="AssertionError")
        failure.text = f"<![CDATA[\nAssertionError: {failure_message}\n]]>"
        system_out = ET.SubElement(testcase, "system-out")
        system_out.text = f"<![CDATA[\n<a href=\"{url}\">Test Report</a>\n\nAssertion: {stdout}\nExpected: {expected}\nActual: {got}\n]]>"

    return testcase

def parse_test_line(line):
    # Split the line into parts based on the table format
    parts = re.split(r'\s*\|\s*(?![^()]*\))', line.strip())
    if len(parts) < 7:
        raise ValueError(f"Line does not have the expected number of parts: {len(parts)} found")

    full_identifier = parts[1].strip()  # The second field contains the test package/class and name
    status = parts[2].strip()  # The third field contains the pass/fail status
    url = re.search(r'\((.*?)\)', parts[3]).group(1).strip()  # Extract the URL inside the parentheses
    stdout = parts[4].strip()  # The fifth field contains the assertion
    got = parts[5].strip()  # The sixth field contains the actual result
    expected = parts[6].strip()  # The seventh field contains the expected result

    try:
        # Split the identifier into the package, class, and test names
        testpackage, testname = full_identifier.split('.', 1)
        if not testpackage or not testname:
            raise ValueError("Test package or test name is empty after splitting.")
    except ValueError as e:
        raise ValueError(f"Identifier does not contain the expected format: {full_identifier}. Error: {str(e)}")

    return testpackage, testname, stdout, full_identifier, got, expected, status, url

def generate_junit_xml(input_file):
    testsuites = ET.Element("testsuites")
    packages_dict = defaultdict(list)  # Dictionary to group test cases by their testpackage

    with open(input_file, 'r') as file:
        for line in file:
            parts = None
            if line.startswith("|"):
                try:
                    parts = re.split(r'\s*\|\s*(?![^()]*\))', line.strip())
                    testpackage, testname, stdout, full_identifier, got, expected, status, url = parse_test_line(line)
                    testcase = create_testcase_element(testpackage, testname, stdout, full_identifier, got, expected, status, url)
                    packages_dict[testpackage].append(testcase)
                    print(f"Processing {testpackage}.{testname}: {status}", file=sys.stderr)
                except ValueError as e:
                    print(f"Skipping line due to error: {e}\nLine: {line}\nParts: {parts}", file=sys.stderr)

    # Create a testsuite for each testpackage group
    for testpackage, testcases in packages_dict.items():
        testsuite = ET.Element("testsuite", name=testpackage)
        for testcase in testcases:
            testsuite.append(testcase)
        testsuites.append(testsuite)

    # Generate the XML tree and return it as a string
    tree = ET.ElementTree(testsuites)
    return ET.tostring(testsuites, encoding="utf-8", xml_declaration=True).decode("utf-8")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python scripts/into_junit.py <input_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    junit_xml = generate_junit_xml(input_file)
    print(junit_xml)
