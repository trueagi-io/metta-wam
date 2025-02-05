using System;
namespace Example4SWICLI
{
    public static class Example4SWICLIClass
    {
        static Example4SWICLIClass()
        {
            Message("Example4SWICLI::SWICLITestClass.<clinit>()");
        }
        public static void install()
        {
            Message("Example4SWICLI::SWICLITestClass.install()");
            Message("SWICLITestClass::install press Ctrl-D to leave CSharp");

            try
            {
                var a = System.Reflection.Assembly.Load("csharp");
                if (a == null)
                {
                    Warning("Assembly 'csharp' could not be loaded.");
                    return;
                }

                var e = a.EntryPoint;
                if (e == null)
                {
                    Warning("EntryPoint is null. Assembly might not have an entry point.");
                    return;
                }

                var dt = e.DeclaringType;
                if (dt == null)
                {
                    Warning("DeclaringType is null. EntryPoint might not belong to a type.");
                    return;
                }

                var m = dt.GetMethod("Main", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static);
                if (m == null)
                {
                    Warning("Method 'Main' not found in the declaring type.");
                    return;
                }

                Message("Invoking 'Main' method...");
                m.Invoke(null, new object[] { new String[0] });
            }
            catch (Exception ex)
            {
                Error("Exception in install(): " + ex.Message);
            }
        }

        public static void Main(string[] args0)
        {
            Message("Example4SWICLI::SWICLITestClass.Main() called");
        }

        public static void Message(string p)
        {
            Console.WriteLine("[INFO] " + p);
            Console.Out.Flush();
        }

        public static void Warning(string p)
        {
            Console.WriteLine("[WARNING] " + p);
            Console.Out.Flush();
        }

        public static void Error(string p)
        {
            Console.WriteLine("[ERROR] " + p);
            Console.Out.Flush();
        }
    }
}
