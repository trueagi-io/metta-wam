import xml.etree.ElementTree as ET
import sys
import os
import re
from collections import defaultdict
import datetime

def testfile_name(url):
    return url.split('#',1)[0].split('/')[-1]

def create_testcase_element(testcase_info, failcounts_dict):
    testpackage, testname, stdout, full_identifier, got, expected, status, url, time = testcase_info
    return create_testcase_element_(testpackage, testname, stdout, full_identifier, got, expected, status, url, time, failcounts_dict)

def create_testcase_element_(testclass, testname, stdout, identifier, got, expected, status, url, time, failcounts_dict):
    # Create the testcase XML element with the class and test name attributes
    testcase = ET.Element("testcase", classname=testclass, name=testname, time=time)

    test_res = f"Assertion: {stdout}\nExpected: {expected}\nActual: {got}"
    url = url.replace("/./","/").replace("//","/").replace(":/","://")    
    if testclass != 'WHOLE-TESTS':
        testfile = testfile_name(url)
        failcount = failcounts_dict[testfile]
        url = url.replace('.metta.html', '.metta.' + str(failcount) + '_failed.html')    
    GITHUB_REPOSITORY_OWNER = os.getenv("GITHUB_REPOSITORY_OWNER", 'trueagi-io')
    if GITHUB_REPOSITORY_OWNER is not None:  # Correct checking against None
        url = url.replace('trueagi-io', GITHUB_REPOSITORY_OWNER.lower())  # Correct method to lower case
    sys_out_text = f"<![CDATA[\n<a href=\"{url}\">Test Report</a>\n\n{test_res}\n]]>"

    if status == "PASS":
        # If the test passed, add system-out with a clickable link and details
        system_out = ET.SubElement(testcase, "system-out")
        system_out.text = sys_out_text        
    else:  # status == "FAIL"
        # If the test failed, add a failure element with details and a clickable link
        failure_message = f"Test failed: Expected '{expected}' but got '{got}'"
        failure = ET.SubElement(testcase, "failure", message=failure_message, type="AssertionError")
        failure.text = f"AssertionError: {failure_message}"
        system_out = ET.SubElement(testcase, "system-out")
        system_out.text = sys_out_text

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
    time = parts[7].strip() # The eighth field contains how long it took to run the test

    try:
        # Split the identifier into the package, class, and test names
        testpackage, testname = full_identifier.split('.', 1)
        if not testpackage or not testname:
            raise ValueError("Test package or test name is empty after splitting.")
    except ValueError as e:
        raise ValueError(f"Identifier does not contain the expected format: {full_identifier}. Error: {str(e)}")

    return testpackage, testname, stdout, full_identifier, got, expected, status, url, time

def generate_junit_xml(input_file, timestamp):
    dt = datetime.datetime.fromisoformat(timestamp)
    timestamps_dict = {}
    failcounts_dict = defaultdict(int) # track number of failures per file
    packages_dict = defaultdict(list)  # Dictionary to group test cases by their testpackage

    with open(input_file, 'r') as file:
        for line in file:
            parts = None
            if line.startswith("|"):
                try:
                    parts = re.split(r'\s*\|\s*(?![^()]*\))', line.strip())
                    testpackage, testname, stdout, full_identifier, got, expected, status, url, time = parse_test_line(line)
                    if status == 'FAIL' and testpackage != 'WHOLE-TESTS':
                        testfile = testfile_name(url)
                        failcounts_dict[testfile] += 1
                    dt += datetime.timedelta(seconds=float(time))
                    if testpackage not in timestamps_dict:
                        timestamps_dict[testpackage] = dt
                    testcase_info = (testpackage, testname, stdout, full_identifier, got, expected, status, url, time)
                    print(testcase_info)
                    packages_dict[testpackage].append(testcase_info)
                    print(f"Processed {testpackage}.{testname}: {status}", file=sys.stderr)
                except ValueError as e:
                    print(f"Skipping line due to error: {e}\nLine: {line}\nParts: {parts}", file=sys.stderr)

    # Create a testsuite for each testpackage group
    testsuites = ET.Element("testsuites", timestamp=timestamp)
    testsuites_time = 0.0
    for testpackage, testcase_infos in packages_dict.items():
        testcases = [create_testcase_element(testcase_info, failcounts_dict) for testcase_info in testcase_infos]
        testsuite_timestamp = timestamps_dict[testpackage].isoformat(timespec='seconds')
        testsuite = ET.Element("testsuite", name=testpackage, timestamp=testsuite_timestamp)
        testsuite_time = 0.0
        for testcase in testcases:
            testsuite_time += float(testcase.get('time'))
            testsuite.append(testcase)
        testsuites_time += testsuite_time
        testsuite.set('time', str(testsuite_time))
        testsuites.append(testsuite)
    testsuites.set('time', str(testsuites_time))

    # Generate the XML tree and return it as a string
    tree = ET.ElementTree(testsuites)
    return ET.tostring(testsuites, encoding="utf-8", xml_declaration=True).decode("utf-8")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python scripts/into_junit.py <input_file> <timestamp>")
        sys.exit(1)

    input_file = sys.argv[1]
    timestamp = sys.argv[2]
    junit_xml = generate_junit_xml(input_file, timestamp)
    print(junit_xml)
