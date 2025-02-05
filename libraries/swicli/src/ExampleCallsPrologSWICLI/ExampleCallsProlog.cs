using System;

namespace ExampleCallsProlog
{
    public static class ExampleCallsPrologClass
    {
        static ExampleCallsPrologClass()
        {
            Message("ExampleCallsProlog::SWICLITestClass.<clinit>()");
        }

        public static void install()
        {
            Message("ExampleCallsProlog::SWICLITestClass.install()");

            try
            {
                var a = System.Reflection.Assembly.Load("csharp");
                if (a == null)
                {
                    Warning("Warning: Assembly 'csharp' could not be loaded.");
                    return;
                }

                var e = a.EntryPoint;
                if (e == null)
                {
                    Warning("Warning: EntryPoint is null.");
                    return;
                }

                var dt = e.DeclaringType;
                if (dt == null)
                {
                    Warning("Warning: DeclaringType is null.");
                    return;
                }

                Message("ExampleCallsProlog::install press Ctrl-D to leave CSharp");

                var m = dt.GetMethod("Main", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static);
                if (m == null)
                {
                    Warning("Warning: Method 'Main' could not be found in the declaring type.");
                    return;
                }

                m.Invoke(null, new object[] { new String[0] });
            }
            catch (Exception ex)
            {
                Error($"Error in install(): {ex.Message}");
            }
        }

        public static void Main(string[] args0)
        {
            Message("ExampleCallsProlog::SWICLITestClass.install()");
        }

        public static void Message(string p)
        {
            Console.WriteLine($"[INFO] {p}");
            Console.Out.Flush();
        }

        public static void Warning(string p)
        {
            Console.WriteLine($"[WARNING] {p}");
            Console.Out.Flush();
        }

        public static void Error(string p)
        {
            Console.WriteLine($"[ERROR] {p}");
            Console.Out.Flush();
        }
    }
}
