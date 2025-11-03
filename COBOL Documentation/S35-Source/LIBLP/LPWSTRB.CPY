      ******************************************************************
      *    LIBLP/LPWSTRB: TRBFILE (BRANCH TRANSFER TRFILE ) PATH WORKERS
      *
      * TRB-PATH = /USR/DATA/R#/XX####/ED/TRBFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRB-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRB-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRB-PATH.
           03  TRB-PATH-BEG           PIC X(13).
           03  TRB-PATH-OWNST         PIC X(02).
           03  TRB-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRB-PATH-FILE          PIC X(15).
           03  FILLER              REDEFINES TRB-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRB-PATH-SQL       PIC X(12).

       01  TRB-ALL-PATH.
           03  TRB-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRB-ALL-BASE.
               05  TRB-ALL-BASE-USR   PIC X(05).
               05  TRB-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRB-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRB-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRBFILE".

       01  TRB-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRB-TRB1-KEY.
           03  LPWSTRB-TB-BRNO        PIC 9(4).
           03  LPWSTRB-TB-DATE        PIC 9(8).
           03  LPWSTRB-TB-CLASS       PIC 9(3).
           03  LPWSTRB-TB-ACCTNO      PIC 9(8).
           03  LPWSTRB-TB-LAWCODE     PIC 9(2).
           03  LPWSTRB-TB-LINE        PIC 9(3).


       01  TRBSUB                     PIC 9(3).
