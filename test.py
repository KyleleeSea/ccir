import lldb
import os


def setup():
    # Compile all binaries for stage 1 tests
    stage_1_tests = os.listdir("../tests/stage_1/valid/")
    print(os.getcwd())
    for test in stage_1_tests:
        os.system(f"cargo run ../tests/stage_1/valid/{test} {test[:len(test)-2]}")

    # Cleanup
    os.system("rm -rf assembly.s")
        

def test_stage_1(expected_return_value, exec_name):
    # Create a new debugger instance
    debugger = lldb.SBDebugger.Create()
    debugger.SetAsync(False)

    # Create a target from the compiled executable
    target = debugger.CreateTargetWithFileAndArch(exec_name, lldb.LLDB_ARCH_DEFAULT)

    passed = False

    if target:
        target.BreakpointCreateByName("main")

        # Create a process and launch it
        process = target.LaunchSimple(None, None, ".")

        if process:
            state = process.GetState()

            if state == lldb.eStateStopped:
                thread = process.GetSelectedThread()

                while True:          
                    thread.StepInstruction(False)  # 'False' to step over, 'True' to step into
                    frame = thread.GetFrameAtIndex(0)
                    disasm = frame.Disassemble()
                    if "ret" in disasm:
                        thread.StepOver()
                        frame = thread.GetFrameAtIndex(0)
                        eax_value = frame.FindRegister("eax").GetValueAsUnsigned()

                        if eax_value == expected_return_value:
                            passed = True
                            break
                        else:
                            break
    
    if passed:
        print(f"Test passed: %eax contains {expected_return_value}")
    else:
        print(f"Test failed: %eax does not contain {expected_return_value}")

    # Cleanup
    lldb.SBDebugger.Destroy(debugger)


def main():
    if os.path.exists("stage1_testing"):
        os.system("rm -rf stage1_testing")
    os.mkdir("stage1_testing")
    os.chdir("stage1_testing")
    
    setup()
    test_stage_1(100, "multi_digit")
    test_stage_1(0, "newlines")
    test_stage_1(0, "no_newlines")
    test_stage_1(0, "return_0")
    test_stage_1(2, "return_2")
    test_stage_1(0, "spaces")

    # Teardown
    os.system("rm -rf stage1_testing")


if __name__ == "__main__":
    main()