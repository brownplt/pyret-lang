macro checkArityM {
  rule { ($rt, $expected, $source) } => {
    if (arguments.length !== $expected) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw $rt.ffi.throwArityErrorC([$source], $expected, $a); }
  }
}

export checkArityM;