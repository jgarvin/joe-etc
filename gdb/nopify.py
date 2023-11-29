import gdb
from gdb import Command

class NopifyCommand(Command):
    """Nopify the instruction at the current instruction pointer (rip/eip)."""

    def __init__(self):
        super(NopifyCommand, self).__init__("nopify", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        # Check SIGSEGV signal handling
        sigsegv_settings = gdb.execute("info signals", to_string=True)
        for line in sigsegv_settings.split("\n"):
            columns = line.split()
            if not columns:
                continue
            if columns[0] != "SIGSEGV":
                continue
            stop = columns[1]
            pass_ = columns[3]
            if stop != "Yes" or pass_ != "No":
                print("ERROR: SIGSEGV is not set to stop and nopass. "
                      "Please run `handle SIGSEGV stop nopass` before using nopify. "
                      "You will have to restart your process after for nopify to work!")
                return

        # Get the current instruction pointer
        rip = int(gdb.parse_and_eval("$rip"))

        # Disassemble the current and the next instruction
        asm_output = gdb.execute("x/2i $rip", to_string=True)

        # Extract the address of the next instruction
        lines = asm_output.strip().split('\n')
        if len(lines) > 1:
            next_instr_line = lines[1]
            next_addr_str = next_instr_line.split()[0]
            next_addr = int(next_addr_str, 16)
            instr_length = next_addr - rip

            # Replace the instruction with NOPs
            for i in range(instr_length):
                cmd = f"set *((char*)({rip}+{i})) = 0x90"
                print(f"Running: {cmd}")
                gdb.execute(cmd)

            print(f"Replaced {instr_length} bytes with NOPs")
        else:
            print("Could not determine the length of the instruction.")

# Register the command
NopifyCommand()
