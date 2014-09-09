# FFI binding tools

The goal of this tools is to make FFI binding creation easier means it
read C headers and generate binding definitions automaticall. I think
writing those file is not something human should do.

# How to use

The `genbind` is the script to generate a binding files. Following is the
example command.

    $ genbind -o "(foo)" -t intptr_t=int -t uintptr_t -s shared.txt \
      test_files/includes/defs.h

This creates 4 files with following structure

- generated
   - foo.scm
      - apis.scm
      - constants.scm
      - types.scm

Command line options are followings;

    -o,--output   Top most output library name.
    -s,--shared   Shared object location definition file. 
    -t,--typedef  Predefined typedef.
      This can have following forms;
        _name_
        _name_=_value_
      _name_ is the defined name. If _value_ is given then following form
      will be emit in the `type.scm` file.
      (define-c-typedef _value_ _name_)
    -D,--define   Preprocessor definition.
    -c,--convert  C function name conversion type.
      scheme: convert camel to snake and '_' to '-'.
      snake:  convert camel to snake.
      under:  convert '_' to '-'
      none:   no conversion.

# TODO

- resolving preprocessor should be done in C parser.
- creating better AST representation.
- better error reporting.
- better source tracking.

Your contribution is always welcome :)