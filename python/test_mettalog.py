
# tests/test_mettalog.py

import unittest
from hyperlog import MeTTaLogImpl

class TestMeTTaLog(unittest.TestCase):

    def setUp(self):
        self.mlog = MeTTaLogImpl()

    def test_addition(self):
        result = self.mlog.run("(+ 10 5)")
        self.assertIn("15", result)

    def test_function_def(self):
        self.mlog.run("(= (f $x) (+ $x 40))")
        result = self.mlog.run("(f 2)")
        self.assertIn("42", result)

    def test_parse(self):
        parsed = self.mlog.parse("(+ 2 2)")
        self.assertTrue(len(parsed) > 0)

if __name__ == "__main__":
    unittest.main()
