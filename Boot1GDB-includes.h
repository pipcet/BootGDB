typedef int sint32_t;
typedef short int sint16_t;
typedef unsigned int uint_t;

#include "defs.h"
#include "extension-priv.h"
#include "defs.h"
#include "arch-utils.h"
#include "command.h"
#include "ui-out.h"
#include "cli/cli-script.h"
#include "gdbcmd.h"
#include "progspace.h"
#include "objfiles.h"
#include "value.h"
#include "language.h"
#include "event-loop.h"
#include "serial.h"
#include "readline/tilde.h"
#include "extension-priv.h"
#include "cli/cli-utils.h"
#include <ctype.h>
#include <stdio.h>

#include "ada-lang.h"
#include "addrmap.h"
#include "amd64bsd-nat.h"
#include "annotate.h"
#include "arch-utils.h"
#include "auto-load.h"
#include "auxv.h"
#include "ax-gdb.h"
#include "ax.h"
#include "bcache.h"
#include "bfd-target.h"
#include "block.h"
#include "breakpoint.h"
#include "bsd-kvm.h"
#include "bsd-uthread.h"
#include "btrace.h"
#include "build-id.h"
#include "buildsym.h"
#include "charset.h"
#include "charset-list.h"
#include "c-lang.h"
#include "cli-out.h"
#include "coff-pe-read.h"
#include "command.h"
#include "complaints.h"
#include "completer.h"
#include "config.h"
#include "continuations.h"
#include "cp-abi.h"
#include "cp-support.h"
#include "ctf.h"
#include "dcache.h"
#include "defs.h"
#include "dfp.h"
#include "dictionary.h"
#include "disasm.h"
#include "d-lang.h"
#include "doublest.h"
#include "dummy-frame.h"
#include "dwarf2expr.h"
#include "dwarf2-frame.h"
#include "dwarf2-frame-tailcall.h"
#include "dwarf2loc.h"
#include "environ.h"
#include "event-loop.h"
#include "event-top.h"
#include "exceptions.h"
#include "exec.h"
#include "expression.h"
#include "extension.h"
#include "extension-priv.h"
#include "fbsd-nat.h"
#include "filesystem.h"
#include "f-lang.h"
#include "frame-base.h"
#include "frame.h"
#include "frame-unwind.h"
#include "gcore.h"
#include "gdbarch.h"
#include "gdb_bfd.h"
#include "gdbcmd.h"
#include "gdbcore.h"
#include "gdb-demangle.h"
#include "gdb-dlfcn.h"
#include "gdb.h"
#include "gdb_obstack.h"
#include "gdb_proc_service.h"
#include "gdb_ptrace.h"
#include "gdb_regex.h"
#include "gdb_select.h"
#include "gdb-stabs.h"
#include "gdbthread.h"
#include "gdbtypes.h"
#include "gdb_usleep.h"
#include "gdb_vfork.h"
#include "gdb_wchar.h"
#include "go-lang.h"
#include "gregset.h"
#include "hppa-linux-offsets.h"
#include "i386-linux-nat.h"
#include "infcall.h"
#include "inf-child.h"
#include "inferior.h"
#include "inf-loop.h"
#include "inflow.h"
#include "inf-ptrace.h"
#include "infrun.h"
#include "inline-frame.h"
#include "interps.h"
#include "jit.h"
#include "jit-reader.h"
#include "jv-lang.h"
#include "language.h"
#include "linespec.h"
#include "linux-fork.h"
#include "linux-nat.h"
#include "linux-record.h"
#include "m2-lang.h"
#include "macroexp.h"
#include "macroscope.h"
#include "macrotab.h"
#include "main.h"
#include "maint.h"
#include "mdebugread.h"
#include "memattr.h"
#include "memory-map.h"
#include "memrange.h"
#include "minsyms.h"
#include "monitor.h"
#include "nbsd-nat.h"
#include "objc-lang.h"
#include "objfiles.h"
#include "obsd-nat.h"
#include "observer.h"
#include "osabi.h"
#include "osdata.h"
#include "parser-defs.h"
#include "p-lang.h"
#include "ppc-ravenscar-thread.h"
#include "probe.h"
#include "procfs.h"
#include "progspace.h"
#include "prologue-value.h"
#include "psympriv.h"
#include "psymtab.h"
#include "ravenscar-thread.h"
#include "record-full.h"
#include "record.h"
#include "regcache.h"
#include "reggroups.h"
#include "registry.h"
#include "regset.h"
#include "remote-fileio.h"
#include "remote.h"
#include "remote-notif.h"
#include "sentinel-frame.h"
#include "ser-base.h"
#include "serial.h"
#include "ser-tcp.h"
#include "ser-unix.h"
#include "sim-regno.h"
#include "skip.h"
#include "solib-aix.h"
#include "solib-darwin.h"
#include "solib.h"
#include "solib-spu.h"
#include "solib-svr4.h"
#include "solib-target.h"
#include "solist.h"
#include "source.h"
#include "sparc-nat.h"
#include "sparc-ravenscar-thread.h"
#include "srec.h"
#include "stabsread.h"
#include "stack.h"
#include "stap-probe.h"
#include "symfile.h"
#include "symtab.h"
#include "target-dcache.h"
#include "target-descriptions.h"
#include "target.h"
#include "terminal.h"
#include "top.h"
#include "tracefile.h"
#include "tracepoint.h"
#include "trad-frame.h"
#include "tramp-frame.h"
#include "typeprint.h"
#include "ui-file.h"
#include "ui-out.h"
#include "user-regs.h"
#include "utils.h"
#include "valprint.h"
#include "value.h"
#include "varobj.h"
#include "varobj-iter.h"
#include "windows-nat.h"
#include "x86-linux-nat.h"
#include "x86-nat.h"
#include "xcoffread.h"
#include "xml-support.h"
#include "xml-syscall.h"
#include "xml-tdesc.h"