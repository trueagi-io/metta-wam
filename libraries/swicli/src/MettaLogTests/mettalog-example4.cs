using System;
using Swicli.Library;
using System.Runtime.InteropServices;
using System.Drawing;
using System.Security;
namespace MettaLogTests
{
    public static class example4
    {
        static example4()
        {
            Console.WriteLine("MettaLogTests::example4.<clinit>()");
        }
        
        public static void Main(String[] args)
        {
			ExecuteWithExceptionHandling(() => PrologCLR.ClientReady = true, "Set PrologCLR.ClientReady");
			ExecuteWithExceptionHandling(PrologCLR.cliDynTest_1, nameof(PrologCLR.cliDynTest_1));
            ExecuteWithExceptionHandling(() => PrologCLR.cliDynTest_3<string>(), nameof(PrologCLR.cliDynTest_3));
            ExecuteWithExceptionHandling(PrologCLR.cliDynTest_2, nameof(PrologCLR.cliDynTest_2));
            ExecuteWithExceptionHandling(MettaLogTestsWindows.install, nameof(MettaLogTestsWindows.install));
            ExecuteWithExceptionHandling(() => MettaLogTestsWindows.WinMain(args), nameof(MettaLogTestsWindows.WinMain));
            ExecuteWithExceptionHandling(() => PrologCLR.Main(args), nameof(PrologCLR.Main));
        }

        private static void ExecuteWithExceptionHandling(Action action, string actionName)
        {
            try
            {
				Console.WriteLine($"Starting,,, {actionName}:");
                action();
				Console.WriteLine($"Finished,,, {actionName}:");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"An error occurred in {actionName}: {ex.Message}");
                // Optionally, log the exception or handle it as needed
            }
        }

        public static void install()
        {
            Console.WriteLine("MettaLogTests::example4.install()");
            //Console.WriteLine("example4::install press ctrol-D to leave CSharp");
            //System.Reflection.Assembly.Load("csharp").EntryPoint.DeclaringType.GetMethod("Main", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static).Invoke(null, new object[] { new String[0] });
        }
    }
 
}
