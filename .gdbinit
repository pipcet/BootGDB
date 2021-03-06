echo Setting up the environment for debugging gdb.\n

if !$gdb_init_done
  set variable $gdb_init_done = 1

  set complaints 1

  b internal_error

  b info_command
  commands
    silent
    return
  end

  # Commands below are not fully compatible with wrapping into an 'if' block.
end

shell echo "set prompt (gdb$GDB_LEVEL)" > .tmp.gdb-pid
shell echo "set environment GDB_LEVEL $[$GDB_LEVEL+1]" >> .tmp.gdb-pid
source .tmp.gdb-pid
shell rm .tmp.gdb-pid

define pdie
  if $argc == 1
    call dump_die ($arg0, 1)
  else
    if $argc == 2
      call dump_die ($arg0, $arg1)
    else
      printf "Syntax: pdie die [depth]\n"
    end
  end
end

document pdie
Pretty print a DWARF DIE.
Syntax: pdie die [depth]
end
