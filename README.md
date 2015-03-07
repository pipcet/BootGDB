# BootGDB
Bootstrapping Perl bindings to GDB using FFI::Platypus

You probably don't want this repository. As of March 2015, it's an
experiment to see how we can get FFI::Platypus to play with GNU gdb,
to help automate the building of Perl bindings for modules written in
languages that gdb can debug, with some emphasis on supporting
programs that aren't in C.

How would you use it if you would want to use it?

Which you don't. Just let me be clear about that. The "bootstrapping"
in the repository name is a multi-stage process, most of which exists
only in my head at present.

 - clone my GDB repository at
   https://github.com/pipcet/binutils-gdb/tree/dirty (FIXME: create a
   bootgdb branch)

 - configure GDB with CFLAGS="-rdynamic -g3 -O0" ./configure
   --enable-target=x86_64-pc-linux-gnu or a similar command.

 - obtain FFI::Platypus from CPAN

 while sleep 1; do ../binutils-gdb/gdb/gdb --args ../binutils-gdb/gdb/gdb --ex 'perl use Boot0GDB' --args ../binutils-gdb/gdb/gdb ; done
