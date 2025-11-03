      ******************************************************************
      *    LIBLP/LPWSTRC: TRCFILE (CHECKS/CASH TRFILE ) PATH WORKERS
      *
      * TRC-PATH = /USR/DATA/R#/XX####/ED/TRCFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRC-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRC-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRC-PATH.
           03  TRC-PATH-BEG           PIC X(13).
           03  TRC-PATH-OWNST         PIC X(02).
           03  TRC-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRC-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRC-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRC-PATH-SQL       PIC X(12).

       01  TRC-ALL-PATH.
           03  TRC-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRC-ALL-BASE.
               05  TRC-ALL-BASE-USR   PIC X(05).
               05  TRC-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRC-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRC-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRCFILE".

       01  TRC-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRC-TRC1-KEY.
           03  LPWSTRC-TC-BRNO        PIC 9(4).
           03  LPWSTRC-TC-KEY-DATE    PIC 9(8).
           03  LPWSTRC-TC-CKNO        PIC 9(6).
           03  LPWSTRC-TC-CLASS       PIC 9(3).
           03  LPWSTRC-TC-SEQ         PIC 9(5).


       01  TRCSUB                     PIC 9(3).
