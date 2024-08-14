import xml.etree.ElementTree as ET
import sys
import re

def create_testcase_element(testclass, testname, stdout, identifier, got, expected, status, url):
    testcase = ET.Element("testcase", classname=testclass, name=testname)
    
    description = f"Test {identifier} with URL: {url}"
    
    if status == "PASS":
        system_out = ET.SubElement(testcase, "system-out")
        system_out.text = f"<![CDATA[\n<a href=\"{url}\">Test Report</a>\n\nAssertion: {stdout}\nExpected: {expected}\nActual: {got}\n]]>"
    else:  # status == "FAIL"
        failure_message = f"Test failed: Expected '{expected}' but got '{got}'"
        failure = ET.SubElement(testcase, "failure", message=failure_message, type="AssertionError")
        failure.text = f"<![CDATA[\nAssertionError: {failure_message}\n]]>"
        system_out = ET.SubElement(testcase, "system-out")
        system_out.text = f"<![CDATA[\n<a href=\"{url}\">Test Report</a>\n\nAssertion: {stdout}\nExpected: {expected}\nActual: {got}\n]]>"
    
    return testcase

def parse_test_line(line):
    parts = re.split(r'\s*\|\s*(?![^()]*\))', line.strip())
    if len(parts) < 7:
        raise ValueError(f"Line does not have the expected number of parts: {len(parts)} found")

    full_identifier = parts[1].strip()  # The second field contains the test class and name
    status = parts[2].strip()  # The third field contains the pass/fail status
    url = re.search(r'\((.*?)\)', parts[3]).group(1).strip()  # Extract the URL inside the parentheses
    stdout = parts[4].strip()  # The fifth field contains the assertion
    got = parts[5].strip()
    expected = parts[6].strip()

    try:
        testclass, testname = full_identifier.rsplit('.', 1)
        if '.' in testclass:
            testname = f"{testclass.split('.')[-1]}.{testname}"  # Combine to form the correct testname
        if not testclass or not testname:
            raise ValueError("Test class or test name is empty after splitting.")
    except ValueError as e:
        raise ValueError(f"Identifier does not contain the expected format: {full_identifier}. Error: {str(e)}")

    return testclass, testname, stdout, full_identifier, got, expected, status, url

def generate_junit_xml(input_file):
    testsuites = ET.Element("testsuites")
    testsuite = ET.Element("testsuite", name="Metta Tests")
    testsuites.append(testsuite)

    with open(input_file, 'r') as file:
        for line in file:
            parts = None
            if line.startswith("|"):
                try:
                    parts = re.split(r'\s*\|\s*(?![^()]*\))', line.strip())
                    testclass, testname, stdout, full_identifier, got, expected, status, url = parse_test_line(line)
                    testcase = create_testcase_element(testclass, testname, stdout, full_identifier, got, expected, status, url)
                    testsuite.append(testcase)
                    print(f"Processing {testclass}.{testname}: {status}", file=sys.stderr)
                except ValueError as e:
                    print(f"Skipping line due to error: {e}\nLine: {line}\nParts: {parts}", file=sys.stderr)

    tree = ET.ElementTree(testsuites)
    return ET.tostring(testsuites, encoding="utf-8", xml_declaration=True).decode("utf-8")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python generate_junit.py <input_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    junit_xml = generate_junit_xml(input_file)
    print(junit_xml)
