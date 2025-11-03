      * COPYMEMBER: LIBLP/LPWSCH
      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LPWSCH
      *
      * DESCRIPTION   : FILE PATH WORKERS FOR CHANGE FILE (CHFILE)
      *
      *=================================================================
      *        CH-PATH = /USR/DATA/R#/XX####/LP/CHFILE
      *                  |----BEG----||-MID-||-----FILE----|
      *
      *=================================================================
      * REV:
      * KEC 2019-0401 ***NEW*** MOVED THE CH FILE PATH WORKERS FROM
      *          LIBLP/LPWKPATH SO TO MAKE IT STANDARD WITH OTHER FILES.
      *
      ******************************************************************
       01  CH-PATH-OVERRIDE            PIC X(01)   VALUE SPACES.

       01  CH-PATH                     PIC X(35)   VALUE SPACES.
       01  FILLER                      REDEFINES CH-PATH.
           03  CH-PATH-BEG             PIC X(13).
           03  CH-PATH-OWNST           PIC X(02).
           03  CH-PATH-OWNBR           PIC 9(04).
           03  FILLER                  PIC X(01).
           03  CH-PATH-FILE            PIC X(15).
           03  FILLER                  REDEFINES CH-PATH-FILE.
               05  FILLER              PIC X(03).
               05  CH-PATH-SQL         PIC X(12).

       01  CH-ALL-PATH.
           03  CH-ALL-BASE             PIC X(09)     VALUE SPACES.
           03  FILLER                  REDEFINES CH-ALL-BASE.
               05  CH-ALL-BASE-USR     PIC X(05).
               05  CH-ALL-BASE-DIR     PIC X(04).
           03  FILLER                  PIC X(01)     VALUE "/".
           03  CH-ALL-MACHINE          PIC X(02).
           03  FILLER                  PIC X(01)     VALUE "/".
           03  CH-ALL-DATA             PIC X(06).
           03  FILLER                  PIC X(10)     VALUE "/LP/CHFILE".
