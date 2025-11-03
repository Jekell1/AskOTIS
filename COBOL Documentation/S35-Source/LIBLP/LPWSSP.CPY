      * COPYMEMBER: LIBLP/LPWSSP
      ******************************************************************
      *
      * DESCRIPTION: SPFILE PATH WORKERS
      * VISION FILE: /USR/DATA/XX/LP/SPFILE     (A15)
      * SQL TABLES : SPAFILE, SPBFILE, SPCFILE  (A34)
      *
      *=================================================================
      * REV:
      *  CS 2016-0824 ADDED LPWSSP-SP1-KEY (REPLACE WORKING STORAGE
      *            HOLD-SP1-KEY
      *
      * BAH 2019-0211 SPLIT SPFILE INTO SPAFILE SPBFILE SPCFILE
      *
      * JKC 2021-0521 COMMENTED OUT LP01SP SINCE NOT NEEDED FOR ACUSQL
      *
      * JKC 2023-0606 COMMENTED OUT THE SPLIT SPFILE INTO SPAFILE,
      *         SPBFILE, SPCFILE; USING THE A30 VERSION FOR FULL FILE.
      *
      ******************************************************************

       01  LPWSSP-SP1-KEY.
           03  LPWSSP-ORGST          PIC X(2)    VALUE SPACES.
           03  LPWSSP-SPRCLASS       PIC 9(3)    VALUE ZERO.
           03  LPWSSP-SUBCLASS       PIC 9(2)    VALUE ZEROS.
           03  LPWSSP-LAWCODE        PIC 9(2)    VALUE ZEROS.

       01  SP-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  SP-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES SP-PATH.
           03  SP-PATH-BEG           PIC X(13).
           03  SP-PATH-FILE          PIC X(22).
           03  FILLER REDEFINES SP-PATH-FILE.
               05  FILLER            PIC X(3).
               05  SP-PATH-SQL       PIC X(19).

      ******************************************************
      * SPAFILE
      ******************************************************
       01  SPA-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                     REDEFINES SPA-PATH.
           03  SPA-PATH-BEG           PIC X(13).
           03  SPA-PATH-FILE          PIC X(22).
           03  FILLER REDEFINES SPA-PATH-FILE.
               05  FILLER             PIC X(3).
               05  SPA-PATH-SQL       PIC X(19).

       01  SPA-ALL-PATH.
           03  SPA-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES SPA-ALL-BASE.
               05  SPA-ALL-BASE-USR  PIC X(05).
               05  SPA-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  SPA-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/LP/SPAFILE".

      ******************************************************
      * SPBFILE
      ******************************************************
       01  SPB-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                     REDEFINES SPB-PATH.
           03  SPB-PATH-BEG           PIC X(13).
           03  SPB-PATH-FILE          PIC X(22).
           03  FILLER REDEFINES SPB-PATH-FILE.
               05  FILLER             PIC X(3).
               05  SPB-PATH-SQL       PIC X(19).

       01  SPB-ALL-PATH.
           03  SPB-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES SPB-ALL-BASE.
               05  SPB-ALL-BASE-USR  PIC X(05).
               05  SPB-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  SPB-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/LP/SPBFILE".

      ******************************************************
      * SPCFILE
      ******************************************************
       01  SPC-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                     REDEFINES SPC-PATH.
           03  SPC-PATH-BEG           PIC X(13).
           03  SPC-PATH-FILE          PIC X(22).
           03  FILLER REDEFINES SPC-PATH-FILE.
               05  FILLER             PIC X(3).
               05  SPC-PATH-SQL       PIC X(19).

       01  SPC-ALL-PATH.
           03  SPC-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES SPC-ALL-BASE.
               05  SPC-ALL-BASE-USR  PIC X(05).
               05  SPC-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  SPC-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/LP/SPCFILE".

      ******************************************************************
      *01  LPWSSP-SP1-KEY.
      *    03  LPWSSP-ORGST          PIC X(2)    VALUE SPACES.
      *    03  LPWSSP-SPRCLASS       PIC 9(3)    VALUE ZERO.
      *    03  LPWSSP-SUBCLASS       PIC 9(2)    VALUE ZEROS.
      *    03  LPWSSP-LAWCODE        PIC 9(2)    VALUE ZEROS.

      *01  SP-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
      *01  SP-PATH                   PIC X(35)   VALUE SPACES.
      *01  SP-ALL-PATH.
      *    03  SP-ALL-BASE           PIC X(09).
      *    03  FILLER                REDEFINES SP-ALL-BASE.
      *        05  SP-ALL-BASE-USR   PIC X(05).
      *        05  SP-ALL-BASE-DIR   PIC X(04).
      *    03  FILLER                PIC X(01)   VALUE "/".
      *    03  SP-ALL-MACHINE        PIC X(02).
      *    03  FILLER                PIC X(23)   VALUE "/LP/SPFILE".
