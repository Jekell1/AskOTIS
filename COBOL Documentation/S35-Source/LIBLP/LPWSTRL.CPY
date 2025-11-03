      ******************************************************************
      *    LIBLP/LPWSTRL: TRLFILE (DIRECT LOAN TRFILE ) PATH WORKERS
      *
      * TRL-PATH = /USR/DATA/R#/XX####/ED/TRLFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRL-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRL-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRL-PATH.
           03  TRL-PATH-BEG           PIC X(13).
           03  TRL-PATH-OWNST         PIC X(02).
           03  TRL-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRL-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRL-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRL-PATH-SQL       PIC X(12).

       01  TRL-ALL-PATH.
           03  TRL-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRL-ALL-BASE.
               05  TRL-ALL-BASE-USR   PIC X(05).
               05  TRL-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRL-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRL-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRLFILE".

       01  TRL-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRL-TRL1-KEY.
           03  LPWSTRL-TL-BRNO        PIC 9(4).
           03  LPWSTRL-TL-DATE        PIC 9(8).
           03  LPWSTRL-TL-CLASS       PIC 9(3).
           03  LPWSTRL-TL-ACCTNO      PIC 9(8).
           03  LPWSTRL-TL-LAWCODE     PIC 9(2).
           03  LPWSTRL-TL-LINE        PIC 9(3).

       01  TRLSUB                     PIC 9(3).
