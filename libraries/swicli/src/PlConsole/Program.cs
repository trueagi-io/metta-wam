using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using SbsSW.SwiPlCs;
using Swicli.Library;

namespace PlConsole
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                string className = args.Length > 0 ? args[0] : "org.armedbear.lisp.Main";
                Type mainType = Type.GetType(className);

                if (mainType == null)
                {
                    Console.WriteLine($"[ERROR] Could not find type: {className}");
                    // return;
                }

                var mainMethod = mainType.GetMethod("main");

                if (mainMethod == null)
                {
                    Console.WriteLine($"[ERROR] Could not find method 'main' in type: {className}");
                    return;
                }

                Console.WriteLine($"[INFO] Invoking {className}.main()...");
                mainMethod.Invoke(null, new object[] { args });
            }
            catch (Exception exception)
            {
                Embedded.WriteException(exception);
            }
        }
    }
}
