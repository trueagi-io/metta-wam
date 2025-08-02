from mettalog import MeTTaLog

def demo():
    print("\n=== 🚀 MeTTaLog DEMO START ===\n")

    root = MeTTaLog()

    root.api.version = "v1"
    root.config["timeout"] = 42
    root.user.name = "douglas"

    chain = root.agent.control.panel.button
    result1 = root.session.auth.login("douglas", password="secret")
    result2 = root("eval").math.sqrt(9)
    response = root["x"]["y"].z(1, 2)

    print("\nAssigned Values:")
    print("api.version      →", root.api.version)
    print("config['timeout']→", root.config["timeout"])
    print("user.name        →", root.user.name)

    print("\nCall Results:")
    print("login(...) →", result1)
    print("eval(math.sqrt) →", result2)
    print("z(1,2) →", response)

    print("\n=== Trace Report ===")
    MeTTaLog.report()

    print("\n=== Prolog Facts ===")
    print(MeTTaLog.to_prolog_facts())

    print("\n=== ✅ MeTTaLog DEMO COMPLETE ===")

def demo_madeup():
    print("\n=== 🪄 demo_madeup(): Dynamic Import from mettalog.madeup ===\n")

    from mettalog.madeup import Whatnot, FooBar, Spam

    # Phantom calls on dynamically generated symbols
    Whatnot.service.activate("X1")
    FooBar.network.connect(ip="127.0.0.1", port=8080)
    result = Spam("special").engine.startup(mode="silent")

    # Show trace
    from mettalog import MeTTaLog
    print("\n--- Prolog Facts from demo_madeup() ---")
    print(MeTTaLog.to_prolog_facts())

    print("\n=== ✅ demo_madeup() COMPLETE ===")


if __name__ == "__main__":
    demo()
    demo_madeup()
