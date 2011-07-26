echo \nReading ~/.gdbinit...\n\n
set print asm-demangle on

source ~/etc/stl-views-1.0.3.gdb

set history size 9999999
set history filename ~/.gdbhistory
set history save on

define gdbkill
kill
end

define gdbquit
quit
end

set script-extension soft
source ~/etc/rwtime.py

echo Turning off listening for SIGUSR1...\n
handle SIGUSR1 nostop
