package PartialType;
use strict;
use warnings;
use 5.008001;
use Carp qw(croak);
use Carp::Always;

sub new {
  my($class, $type) = @_;
  my $self = bless {}, $class;

  return $self unless defined $type;

  $self->{type} = $type;

  return $self;
}

sub describe {
  my($self,$seen,$indent) = @_;
  my $ret = "";

  $indent = "" unless defined $indent;
  $seen = {} unless $seen;

  if($self->{name}) {
    $ret .= $indent . "partial type " . $self->{name};
  } else {
    $ret .= $indent . "anonymous partial type $self";
  }
  $ret .=  ($indent eq "" ? ":": ", which:") . "\n";

  $indent .= "  ";

  if($seen->{$self}++) {
    $ret .= $indent . "was seen before\n";
  } else {
    if($self->{is_pointer}) {
      $ret .= $indent . "is a pointer to:\n";

      $ret .= $self->{target}->describe($seen, $indent . "  ");
    }
    for my $field (keys %{$self->{fields}}) {
      $ret .= $indent . "has a field named $field of type:\n";
      $ret .= $self->{fields}->{$field}->describe($seen, $indent . "  ");
    }
    if($self->{type}) {
      $ret .= $indent . "is known exactly to be:\n";
      $ret .= $self->{type}->describe($seen, $indent . "  ");
    }
    for my $cast (values %{$self->{casts_to}}) {
      $ret .= $indent . "casts to:\n";
      $ret .= $cast->describe($seen, $indent . "  ");
    }
    for my $cast (values %{$self->{casts_from}}) {
      $ret .= $indent . "casts from:\n";
      $ret .= $cast->describe($seen, $indent . "  ");
    }
  }

  return $ret;
}

sub casts_to {
  my($self, $cast) = @_;

  $self->{casts_to}->{$cast} = $cast;
  $cast->{casts_from}->{$self} = $self;
}

sub intersection_type_cast {
  my($self,$other) = @_;
  my $ret = PartialType->new();

  $self->casts_to($ret);
  $other->casts_to($ret);

  return $ret;
}

sub intersection_type_nocast {
  my($self,$other) = @_;

  return $self unless $other;

  my $ret = PartialType->new();

  if($self->{type} and $other->{type}) {
    warn "I hope they match!";
    $ret->{type} = $self->{type};
  } else {
    $ret->{type} = $self->{type} if $self->{type};
    $ret->{type} = $other->{type} if $other->{type};
  }

  for my $cast (values %{$self->{casts_to}}) {
    $ret->{casts_to}->{$cast} = $cast;
  }

  for my $cast (values %{$self->{casts_from}}) {
    $ret->{casts_from}->{$cast} = $cast;
  }

  if ($self->{fields}) {
    for my $field (keys %{$self->{fields}}) {
      $ret->{fields}->{$field} = $self->{fields}->{$field};
    }
  }

  if ($other->{fields} and $self->{fields}) {
    for my $field (keys %{$other->{fields}}) {
      $ret->{fields}->{$field} = $other->{fields}->{$field}->intersection_type_nocast($self->{fields}->{$field});
    }
  }

  if (defined($self->{is_pointer})) {
    $ret->{is_pointer} = $self->{is_pointer};
  }

  if (defined($other->{is_pointer})) {
    $ret->{is_pointer} = $other->{is_pointer};
  }

  return $ret;
}

sub deref {
  my($self) = @_;

  if($self->{target}) {
    # nothing to do
  } elsif($self->{type}) {
    $self->{target} = PartialType->new($self->{type}->{target});
  } else {
    $self->{is_pointer} = 1;
    $self->{target} = PartialType->new();
  }

  return $self->{target};
}

sub field_must_exist
{
  my($self, $name) = @_;

  $self->{fields}->{$name} = PartialType->new($self->{type} ? $self->{type}->{fields}->{$name} : undef);
}

sub field_type
{
  my($self, $name) = @_;

  $self->field_must_exist($name);

  return $self->{fields}->{$name};
}

my $expr_ops = {
  TERNOP_COND => sub {
    my ($attr) = @_;
    my @subexps = @{$attr->{subexps}};

    # $subexps[0]->casts_to($bool);

    return $subexps[1]->intersection_type_cast($subexps[2]);
  },

  STRUCTOP_PTR => sub {
    my ($attr) = @_;
    my $value = $attr->{value};
    my @subexps = @{$attr->{subexps}};

    my $type = $subexps[0]->deref;

    $type->field_must_exist($value);
    return $type->field_type($value);
  },

  STRUCTOP_STRUCT => sub {
    my ($attr) = @_;
    my $value = $attr->{value};
    my @subexps = @{$attr->{subexps}};

    my $type = $subexps[0];

    $type->field_must_exist($value);
    return $type->field_type($value);
  },

  OP_INTERNALVAR => sub {
    my ($attr) = @_;
    my $value = $attr->{value};
    my $type = PartialType->new;

    return $type;
  },

  UNOP_CAST_TYPE => sub {
    my ($attr) = @_;
    my @subexps = @{$attr->{subexps}};
    my $cast_type = $subexps[0];
    my $type = $subexps[1];

    $type->casts_to($cast_type);

    return $cast_type;
  },

  OP_TYPE => sub {
    my ($attr) = @_;

    return PartialType->new($attr->{type});
  },

  OP_LONG => sub {
    my ($attr) = @_;

    return PartialType->new($attr->{type});
  },
};

sub ops {
  my($class, $internals) = @_;

  my %ops = %$expr_ops;

  $ops{OP_INTERNALVAR} = sub {
    my ($attr) = @_;
    my $value = $attr->{value};
    my $type = PartialType->new;

    $type->{name} = $value;
    $internals->{$value} = $type;

    return $type;
  };

  return \%ops;
}

sub match {
  my($self,$type,$mapping) = @_;

  if ($mapping->{$self}) {
    return $type->match($mapping->{$self});
  }

  $mapping->{$self} = $type;

  if($self->{type}) {
    my $matches = $self->{type}->match($type);

    return $matches if defined $matches;
  }

  if($self->{fields} and $type->{fields}) {
    for my $field (keys %{$self->{fields}}) {
      return 0 unless $type->{fields}->{$field};
    }
  }

  if($self->{is_pointer}) {
    if($type->{kind} eq 'name') {
      return;
    }

    if($type->{kind} ne 'PTR') {
      return 0;
    }

    my $matches = $self->{target}->match($type->{target});

    return $matches if defined $matches;
  }

  return;
}

package GDBType;

sub match {
  my($self, $other, $mapping) = @_;

  return 1 if $self == $other;

  return 0 if ($self->{kind} ne $other->{kind}) and not ($self->{kind} eq 'name' or $other->{kind} eq 'name');

  if ($mapping->{$self}) {
    return ($other == $mapping->{$self}) ? 1 : undef;
  }

  $mapping->{$self} = $other;

  if ($self->{fields} and $other->{fields}) {
    for my $field (keys %{$self->{fields}}, keys %{$other->{fields}}) {
      return 0 unless $other->{fields}->{$field}->match($self->{fields}->{$field});
    }
  }

  return;
}

sub describe {
  my($self,$seen,$indent) = @_;
  my $ret = "";

  $indent = "" unless defined $indent;
  $seen = {} unless $seen;

  if($self->{name}) {
    $ret .= $indent . "type " . $self->{name};
  } else {
    $ret .= $indent . "anonymous type $self";
  }
  $ret .=  ($indent eq "" ? ":": ", which:") . "\n";

  $indent .= "  ";

  if($seen->{$self}++) {
    $ret .= $indent . "was seen before\n";
  } else {
    if($self->{kind} eq 'PTR') {
      $ret .= $indent . "is a pointer to:\n";

      $ret .= $self->{target}->describe($seen, $indent . "  ");
    } elsif($self->{kind} eq 'INT') {
      $ret .= $indent . "is a " . ($self->{sign} ? "signed" : "unsigned") . " integer of size " . $self->{bits} . "\n";
    }
    for my $field (keys %{$self->{fields}}) {
      $ret .= $indent . "has a field named $field:\n";
      $ret .= $self->{fields}->{$field}->describe($seen, $indent . "  ");
    }
  }

  return $ret;
}

sub new_kind_target {
  my($class, $kind, $target) = @_;

  my $self = bless {}, $class;
  $self->{kind} = $kind;
  $self->{target} = $target;

  return $self;
}

sub new_int {
  my($class, $bits, $sign) = @_;

  $sign = 1 unless defined $sign;

  my $self = bless {}, $class;
  $self->{kind} = 'INT';
  $self->{bits} = $bits;
  $self->{sign} = $sign;

  return $self;
}

sub new_pointer {
  my($class, $target) = @_;

  return GDBType->new_kind_target('PTR', $target);
}


my $type_ops = {
  PTR => sub {
    my($attr) = @_;
    my($self) = GDBType->new_pointer($attr->{target});

    return $self;
  },
  INT => sub {
    my($attr) = @_;
    my($self) = GDBType->new_int($attr->{sizeof} * 8); # bits per byte;

    return $self;
  },
  STRUCT => sub {
    my($attr) = @_;
    my($self) = GDBType->new_kind_target('struct', undef);

    for my $field (@{$attr->{fields}}) {
      $self->{fields}->{$field->{name}} = $field->{type};
    }

    return $self;
  },
  UNION => sub {
    my($attr) = @_;
    my($self) = GDBType->new_kind_target('struct', undef);

    for my $field (@{$attr->{fields}}) {
      $self->{fields}->{$field->{name}} = $field->{type};
    }

    return $self;
  },
  ENUM => sub {
    my($attr) = @_;
    my($self) = GDBType->new_kind_target('enum', undef);

    for my $field (@{$attr->{fields}}) {
      $self->{fields}->{$field->{name}} = $field->{type};
    }

    return $self;
  },
  name => sub {
    my($name) = @_;
    my $ret = GDBType->new_kind_target('name', undef);
    $ret->{name} = $name;
    return $ret;
  }
};

sub ops {
  my ($class, $types) = @_;
  $types = {} if !$types;
  my %ops = %$type_ops;

  $ops{name} = sub {
    my($name) = @_;

    return $types->{$name} if $types->{$name};

    my $ret = GDBType->new_kind_target('name', undef);
    $ret->{name} = $name;

    $types->{$name} = $ret;

    return $ret;
  };

  return \%ops;
}

package FFI::Platypus::GDB::GDBExpression;

sub new {
  my($class, $opcode, @children) = @_;

  my $self = bless { opcode => $opcode, subexps => \@children }, $class;

  return $self;
}

sub n {
  my($self) = @_;
  return scalar @{$self->{subexps}};
}

sub subexp {
  my($self, $i) = @_;

  return $self->{subexps}->[$i];
}

sub c_paren {
  return ['(',')'];
}

sub c_emit {
  my($self) = @_;

  my $prefix = $self->c_prefix;
  my $suffix = $self->c_suffix;

  my $paren = $self->c_paren;
  my ($open,$close) = ('','');
  ($open,$close) = @$paren if $paren;

  $prefix = $open . $prefix if defined $prefix;
  $suffix = $suffix . $close if defined $suffix;

  if($self->n == 0 and
     (defined($prefix) or defined($suffix))) {
    return (defined($prefix) ? $prefix : "") . (defined($suffix) ? $suffix : "");
  }

  if($self->n == 1 and
     defined($prefix) and
     defined($suffix)) {
    return $prefix . $self->child(0)->c_emit . $suffix;
  }

  return;
}

sub emit {
  my($self, @languages) = @_;

  for my $language (@languages) {
    my $ret = $self->emit($language);

    return $ret if defined $ret;
  }

  return undef;
}

sub fragment($) {
}

my %gdb_opcode_to_code;
my @gdb_opcodes;

sub init {
  my($class, $gdb) = @_;

  my $output = $gdb->run_command('py print gdb.opcodes');
  $output =~ s/, <NULL>//g;
  my $i = 0;
  my @opcodes = @{eval fragment $output};

  for my $opcode (@opcodes) {
    my $frag = qq{
package FFI::Platypus::GDB::GDBExpression::$opcode;
use parent -norequire, 'FFI::Platypus::GDB::GDBExpression';
};

    if ($opcode =~ /^UNOP/) {
      $frag .= "use parent -norequire, 'FFI::Platypus::GDB::GDBExpression::UnaryOperator';\n";
    } elsif ($opcode =~ /^BINOP/) {
      $frag .= "use parent -norequire, 'FFI::Platypus::GDB::GDBExpression::BinaryOperator';\n";
    } elsif ($opcode =~ /^TERNOP/) {
      $frag .= "use parent -norequire, 'FFI::Platypus::GDB::GDBExpression::TernaryOperator';\n";
    }

    $frag .= "\n";

    $frag .= qq{
sub c_prefix {
}

sub c_suffix {
}

};

    if ($opcode =~ /^BINOP/) {
      $frag .= qq{
sub c_infix {
}

};
    } elsif ($opcode =~ /^TERNOP/) {
      $frag .= qq{
sub c_infix {
}

sub c_infix2 {
}

};
    }

    $frag .= qq{
sub opcode {
  return $i; # expanded before we eval
}
};


    eval fragment $frag;

    $gdb_opcodes[$i] = $opcode;
    $gdb_opcode_to_code{$opcode} = $i;

    $i++;
  }
}

package FFI::Platypus::GDBExpression::UnaryOperator;
use parent -norequire, 'FFI::Platypus::GDBExpression';

sub c_emit {
  my($self) = @_;

  if($self->n == 1 and
     $self->can('c_prefix') and
     $self->can('c_suffix')) {
    return $self->c_prefix . $self->child(0)->c_emit . $self->c_suffix;
  }
}

package FFI::Platypus::GDBExpression::BinaryOperator;
use parent -norequire, 'FFI::Platypus::GDBExpression';

sub c_emit {
  my($self) = @_;

  if($self->n == 1 and
     $self->can('c_prefix') and
     $self->can('c_infix') and
     $self->can('c_suffix')) {
    return $self->c_prefix . $self->child(0)->c_emit . $self->c_infix . $self->child(1)->c_emit . $self->c_suffix;
  }
}

package FFI::Platypus::GDBExpression::TernaryOperator;
use parent -norequire, 'FFI::Platypus::GDBExpression';

sub c_emit {
  my($self) = @_;

  if($self->n == 1 and
     $self->can('c_prefix') and
     $self->can('c_infix') and
     $self->can('c_suffix')) {
    return $self->c_prefix . $self->child(0)->c_emit . $self->c_infix . $self->child(1)->c_emit . $self->c_infix2 . $self->child(2)->c_emit . $self->c_suffix;
  }
}

package Boot0GDB::Subexpression;

sub new {
  my($class, $gdb, $address, $i) = @_;
  my $self = bless {}, $class;
  $self->{gdb} = $gdb;
  $self->{address} = $address;
  $self->{i} = 0;

}

sub subexp {
  my ($self, $i) = @_;

  my $ret = FFI::Platypus::GDB::Bootstrap::NativeGDBSubexpression->new($self->{gdb}, $self->{address}, $i);

  $ret->{expression} = $self->{expression};
  $ret->{expression} = $self unless defined $ret->{expression};

  return $ret;
}

sub print_with_callback {
  my ($self, $cb, $prec) = @_;
  $prec = 0 if !defined $prec;
  my $stream = $self->{mem_fileopen}->();

  my $repl = $cb->($self);

  if (defined $repl) {
    return $repl;
  } else {
    my $gdb_closure = $self->{gdb}->closure(sub {
      my ($stream, $ip, $address, $prec) = @_;
      my $subexp = $self->subexp($$ip);
      my $string = $self->print_with_callback($cb, $prec);

      $self->{ui_file_write}->($stream, $string, length($string));
    });

    my $i = $self->{i};
    $self->{print_subexp_callback}->($stream, \$i, $self->{address}, $prec, $gdb_closure);

    my $ret = $self->{ui_file_xstrdup}->($stream, undef);

    $self->{ui_file_delete}->($stream);

    return $ret;
  }
}

package Boot0GDB::Expression;
use parent -norequire, 'Boot0GDB::SubExpression';

sub new {
  my($class, $gdb, $string) = @_;
  my $address;
  my $by_index = {};

  $gdb->ensure_python_initialized;
  my $python_string = $gdb->execute_command_to_string(qq{py print print_expression('$string')}, 0);

  {
    my $e = sub {
      my ($opcode, $rest) = @_;

      if (defined $rest->{index}) {
	$by_index->{$rest->{index}} = $rest;
      }

      return $rest;
    };

    my $t = sub {
      my ($kind, $rest) = @_;

      return $rest;
    };

    my $expr = sub {
      my ($expression, $rest, $address) = @_;

      $address = $gdb->cast('long' => 'expression', $address)
	  if defined $address;
    };

    eval $python_string;

    die $@ if $@;
  }

  my $ret = $class->SUPER::new($gdb, $address, 0);

  $ret->{string} = $string;
  $ret->{python_string} = $string;
  $ret->{by_index} = $by_index;

  return $ret;
}

package Boot0GDB;
use FFI::Platypus;
use Carp qw(croak);

sub opaque {
  return 'opaque';
}

sub expression {
  return 'expression';
}

sub ui_file {
  return 'ui_file';
}

sub void {
  return 'void';
}

sub closure {
  my($self, @args) = @_;

  return $self->{ffi}->closure(@args);
}

sub cast {
  my($self, @args) = @_;

  return $self->{ffi}->cast(@args);
}

sub method($) {
  my ($self) = @_;

  return sub {
    my($xsub, $self2, @args) = @_;

    croak unless $self == $self2;

    return $xsub->(@args);
  };
}

sub ensure_python_initialized {
  my($self) = @_;

  return 1 if $self->{python_initialized};

  warn;
  my $result = $self->execute_command_to_string(qq{py import gdb});
  warn $result;
  $result = $self->execute_command_to_string(qq{py exec file('./perlify-expressions.py')});
  warn $result;

  $self->{python_initialized} = 1;

  return 1;
}

sub new {
  my($class) = @_; # no other arguments, since we're only run by one gdb process
  my $self = bless {}, $class;

  my $ffi = $self->{ffi} = FFI::Platypus->new;
  $ffi->lib(undef); # we're the gdb process. The -rdynamic option to gcc is necessary for this to work.
  $ffi->type(opaque => expression);
  $ffi->type(opaque => ui_file);

  $ffi->attach('parse_expression', ['string'] => 'expression', method($self));
  $ffi->attach('mem_fileopen' => [] => opaque, method($self));
#  $ffi->attach('print_subexp_callback' => [ui_file, 'int*', expression, 'int', '(ui_file, int*, opaque, int)->void'] => void);
  $ffi->attach('print_subexp_callback' => [ui_file, 'int*', expression, 'int', '(ui_file, opaque, opaque, int)->void'] => void, method($self));
  $ffi->attach('ui_file_xstrdup', ['ui_file', 'opaque'] => 'string', method($self));
  $ffi->attach('ui_file_write', ['ui_file', 'string', 'size_t'] => 'void', method($self));
  $ffi->attach('ui_file_delete', ['ui_file'] => 'void', method($self));
  $ffi->attach('execute_command_to_string', ['string', 'int'] => 'string', method($self));

  warn execute_command_to_string("p *(int *)0", 0);
}

package FFI::Platypus::GDB;

use strict;
use warnings;
use 5.008001;
use Carp qw(croak);
#use Carp::Always;


=head1 SYNOPSIS

 use FFI::Platypus::GDB;
 my

Why GDB?
 - it's language-agnostic, to a certain extent
 - it uses the preprocessed source code, but also gives access to macro definitions
 - it has a type model that's already working and well-tested, so modelling ours after GDB's is good

Why not GDB?
 - it's slow
 - you need to figure out how to build with debug flags
 - -g3 debug data is huge

Unfortunately, there are obvious but debilitating bugs in both the
python and the guile bindings for GDB
(https://sourceware.org/bugzilla/show_bug.cgi?id=18070 and
https://sourceware.org/bugzilla/show_bug.cgi?id=18073), so we need to
use a hacked/fixed version of GDB for now.

=cut

use IPC::Run qw(start);
use FFI::Platypus;

sub fragment($) {
}

sub wait_for_prompt {
  my ($self) = @_;
  until ($self->{out} =~ /\(gdb\) $/) {
    $self->{ipc}->pump;
  }
}

sub run_command {
  my ($self, $command) = @_;

  warn "running command $command";
  $self->{in} = "$command\n";

  $self->wait_for_prompt;

  my $res = $self->{out};
  $self->{out} = "";
  $res =~ s/\(gdb\) $//;
  warn "result is $res";
  return $res;
}

sub handle_type {
  my ($self, $name) = @_;

  my $eval = $self->run_command("py print print_type('$name')");
  my $ffi = $self->{ffi};

  eval $eval;

  die $@;

  return $self;
}

use Data::Dumper;


sub pretype_set_name {
  my ($self, $pretype, $name) = @_;

  return if (!ref $pretype);
  return if (exists $pretype->{name});

  $pretype->{name} = $name;
  $self->{pretypes_by_name}->{$name} = $pretype;

  if ($pretype->{type} eq 'func') {
    my $arg = 0;

    for my $field (@{$pretype->{fields}}) {
      pretype_set_name($self, $field->{type}, $name . ".arg$arg");
      $arg++;
    }

    pretype_set_name($self, $pretype->{target}, $name . ".return");
  } elsif ($pretype->{type} eq 'ptr') {
    pretype_set_name($self, $pretype->{target}, $name . ".target");
  } elsif ($pretype->{type} eq 'array') {
    pretype_set_name($self, $pretype->{target}, $name . ".target");
  } elsif ($pretype->{type} eq 'struct') {
    for my $field (@{$pretype->{fields}}) {
      pretype_set_name($self, $field->{type}, $name . "." . $field->{name});
    }
  }
}

my $last_unknown_type;

sub realize_pretype {
  my ($self, $name, $attr) = @_;

  if (ref $attr) {
    if ($attr->{type} eq 'struct' or
	$attr->{type} eq 'record') {
      my @record_definition;
      for my $field (@{$attr->{fields}}) {
	if (exists $field->{bitsize}) {
	  croak unless $field->{bitsize} == 1;
	  croak unless $field->{type} eq 'unsigned int';

	  push @record_definition, 'int' => $field->{name};
	} elsif (ref $field->{type}) {
	  $self->realize_pretype($field->{type}->{name}, $field->{type});
	  push @record_definition, $field->{type}->{name} => $field->{name};
	} elsif ($field->{type} and !$self->{ffi}->{types}->{$field->{type}}) {
	  if ($self->{ffi}->{pretypes}->{$field->{type}}) {
	    $field->{type} = $self->{ffi}->{pretypes}->{$field->{type}};
	    redo;
	  }
	  warn "unknown type " . $field->{type};
	  $last_unknown_type = \$field->{type};
	  warn $last_unknown_type;
	  return;
	}
      }
      warn "record definition is " . Dumper(\@record_definition);
      my $qname = $name;
      $qname =~ s/ //g;
      if ($self->{ffi}->{types}->{$name} ||
	  "Record::$qname"->can('_ffi_record_size')) {
	return "record(Record::$qname)";
      } else {
	warn "{ package Record::$qname; use FFI::Platypus::Record; " . 'record_layout($self->{ffi}, @record_definition); }';
	eval fragment "{ package Record::$qname; use FFI::Platypus::Record; " . 'record_layout($self->{ffi}, @record_definition); }';

	die $@ if $@;

	my $size = "Record::$qname"->_ffi_record_size;

	warn "size is $size";

	$self->{ffi}->type("record(Record::$qname)", $name);

	return "record(Record::$qname)";
      }
    } elsif ($attr->{type} eq 'typedef') {
      my $new = ref($attr->{target}) ? $attr->{target}->{name} : $attr->{target};

      if ($self->{ffi}->{types}->{$new}) {
	warn Dumper($self->{ffi});
	$self->{ffi}->type($new, $attr->{type});
	return 1;
      } else {
	$last_unknown_type = \$new;
	warn $$last_unknown_type;
	return;
      }
    } elsif ($attr->{type} eq 'ptr') {
      if ($self->{ffi}->{types}->{$name}) {
	return 1;
      } else {
	$self->{ffi}->type('opaque', $name);
	return 1;
      }
    }
  }

  return 1;
}

# we rewrite types as follows:
#  - a pointer to a struct is a record
#  - a pointer to a function is a closure
#  - it doesn't make sense to translate pointers to unions to unions of pointers, since most unions appear at an offset in a structure.

sub handle_symbol {
  my ($self, $name, $symbol) = @_;

  my $attr = $self->{pretypes_by_name}->{$name};
  my $ffi = $self->{ffi};

  if ($attr) {
    warn "defining $name to " . Dumper($attr);
    $ffi->{pretypes}->{$name} = $attr;
    while (!$self->realize_pretype($name, $attr)) {
      $self->handle_symbol($$last_unknown_type, 0);
    }
  }

  my $eval = $self->run_command($symbol ? "py print print_symbol_type('$name')" : "py print print_type('$name')");


  my $def = sub {
    my($name, $attr) = @_;

    pretype_set_name($self, $attr, $name);

    warn "defining $name to " . Dumper($attr);
    $ffi->{pretypes}->{$name} = $attr;
    while (!$self->realize_pretype($name, $attr)) {
      $self->handle_symbol($$last_unknown_type, 0);
    }
  };

  my $ptr = sub {
    my ($attr) = @_;
    if (ref($attr->{target}) and $attr->{target}->{type} eq 'func') {
      my %h = %{$attr->{target}};

      $h{type} = 'closure';
      return \%h;
    } elsif (ref($attr->{target}) and $attr->{target}->{type} eq 'struct') {
      my %h = %{$attr->{target}};

      $h{type} = 'record';
      return \%h;
    }

    my %h = %$attr;
    $h{type} = 'ptr';
    delete $h{sizeof};
    return \%h;
  };

  my $func = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'func';
    return \%h;
  };

  my $typedef = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'typedef';
    delete $h{sizeof};
    delete $h{fields};

    return \%h;
  };

  my $void = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'void';
    return \%h;
  };

  my $struct = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'struct';
    return \%h;
  };

  my $union = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'union';
    return \%h;
  };

  my $int = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'int';
    return \%h;
  };

  my $bool = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'bool';
    return \%h;
  };

  my $array = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'array';
    return \%h;
  };

  my $range = sub {
    my ($attr) = @_;
    my %h = %$attr;
    $h{type} = 'range';
    return \%h;
  };

  eval $eval;

  die $@ if $@;

  return $self;
}

use Data::Dumper;

sub read_type
{
  my($self, $name) = @_;

  $self->{types} = {} unless $self->{types};

  my %t = %{GDBType->ops($self->{types})};
  my $def = sub { $_[1]->{name} = $_[0]; $self->{types}->{$_[0]} = $_[1]; return $_[1]; };

  my $eval = $self->run_command("py print print_type('$name')");

  return $self if $eval eq "";

  my $t = eval fragment $eval;

  die $@ if $@;

  print $t->describe;

  return $self;
}

sub read_type_expression
{
  my($self, $name) = @_;

  $self->{types} = {} unless $self->{types};

  my $expr = sub { $_[1]->{name} = $_[0]; $self->{types}->{$_[0]} = $_[1]; return $_[1]; };

  my %e = (OP_TYPE => sub { $_[0]->{type} });
  my %t = %{GDBType->ops($self->{types})};
  my $def = sub { $_[1]->{name} = $_[0]; $self->{types}->{$_[0]} = $_[1]; return $_[1]; };

  my $eval = $self->run_command("py print print_expression('$name')");

  return $self if $eval eq "";

  my $t = eval fragment $eval;

  die $@ if $@;

  print $t->describe;

  return $self;
}

sub guess_macro_type
{
  my ($self, $macro, $linespec, $input_type) = @_;
  my $internals = {};

  my %e = %{PartialType->ops($internals)};
  my %t = %{GDBType->ops($self->{types})};

  my $expr = $self->run_command("py print print_macro('$macro', '$linespec')");

  return $self if $expr eq "";

  my $partial_type = eval fragment $expr;

  die $@ if $@;

  print Dumper($partial_type);
  print $partial_type->describe;

  for my $name (keys %{$internals}) {
    print "\n\n";

    print Dumper($internals->{$name});
    print $internals->{$name}->describe;
  }

  for my $internal (keys %{$internals}) {
    my @potential_types;
    for my $type (keys %{$self->{types}}) {
      next if $self->{types}->{$type}->{kind} eq 'name';

      my $matches =  $internals->{$internal}->match($self->{types}->{$type});

      $matches = 'undef' unless defined $matches;

      print "$internal matches $type: " . $matches . "\n";

      push @potential_types, $self->{types}->{$type} if $matches eq 'undef';
    }

    for my $type (@potential_types) {
      # this looks C-specific, but it's actually not, as long as we
      # set language to c and back afterwards. A better solution is on
      # its way, though.
      $self->run_command("p \$"."$internal = typeof(" . $type->{name}. ")");

      my $expr = $self->run_command("py print print_macro_type('$macro', '$linespec')");

    }
  }

  return $self;
}

sub handle_macro
{
  my ($self, $macro, $linespec, $input_type) = @_;

  my $expr = $self->run_command("py print print_macro('$macro', '$linespec')");

  return $self if $expr eq "";

  eval fragment $expr;
}

sub new {
  my ($class) = @_;
  my $self = bless {}, $class;

  $self->{in} = "";
  $self->{out} = "";

  $self->{ffi} = FFI::Platypus->new();

  $self->{ipc} = start(['/home/pip/git/binutils-gdb/gdb/gdb', '/home/pip/git/binutils-gdb/gdb/gdb'], \$self->{in}, \$self->{out});

  $self->wait_for_prompt;
  $self->{out} = "";
  $self->run_command("py exec file('/home/pip/git/FFI-Platypus/share/gdb/perlify-expressions.py')");

  FFI::Platypus::GDB::GDBExpression->init($self);

  return $self;
}

if (0) {
  my $gdb = FFI::Platypus::GDB->new;
  $gdb->run_command("b main");
  $gdb->run_command("run");
  $gdb->read_type('struct type');
  $gdb->read_type('struct main_type');
  $gdb->read_type_expression('struct type *');
  $gdb->read_type_expression('struct main_type *');
  my @types = keys %{$gdb->{types}};
  for my $type (@types) {
    $gdb->read_type($type);
  }
  $gdb->guess_macro_type('TYPE_OBJFILE', $main::PERL_LINESPEC);

  # $gdb->handle_symbol('ffi_pl_closure_call', 1);
  # $gdb->handle_symbol('ffi_pl_record_accessor_uint8', 1);
  # $gdb->handle_symbol('ffi_pl_closure');

  FFI::Platypus::GDB::Fragment::show_fragments();
}

package Boot0GDB;

$Boot0GDB::gdb = Boot0GDB->new;

1;
