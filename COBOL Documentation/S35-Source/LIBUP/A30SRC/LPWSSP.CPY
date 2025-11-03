      **********************************************************************
      **********************************************************************
      **********************************************************************
      *   NOTE: NEED TO ADD "O" IN FRONT OF "SP" PREFIX;
      *   NOTE: COPY REPLACING LEADING WILL ADD "O" TO WORD "SPACES".
      **********************************************************************
      **********************************************************************
      **********************************************************************
      * COPYMEMBER: LIBLP/LPWSSP
      *********************************************************
      *    LIBLP/LPWSSP: SPFILE PATH WORKERS
      *    /USR/DATA/XX/LP/SPFILE
      *  CS 160824 ADDED LPWSSP-SP1-KEY (REPLACE WORKING STORAGE HOLD-SP1-KEY
      ******************************************************************
      *01  LPWSSP-SP1-KEY.
      *    03  LPWSSP-ORGST          PIC X(2)    VALUE SPACES.
      *    03  LPWSSP-SPRCLASS       PIC 9(3)    VALUE ZERO.
      *    03  LPWSSP-SUBCLASS       PIC 9(2)    VALUE ZEROS.
      *    03  LPWSSP-LAWCODE        PIC 9(2)    VALUE ZEROS.

       01  OSP-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  OSP-PATH                   PIC X(35)   VALUE SPACES.
       01  OSP-ALL-PATH.
           03  OSP-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES OSP-ALL-BASE.
               05  OSP-ALL-BASE-USR   PIC X(05).
               05  OSP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  OSP-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/LP/SPFILE".
