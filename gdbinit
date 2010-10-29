echo \nReading ~/.gdbinit...\n\n
set print asm-demangle on

source ~/etc/stl-views-1.0.3.gdb

set history save on

define gdbkill
kill
end

define gdbquit
quit
end
