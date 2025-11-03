      * COPYMEMBER: LIBLP/LPBPPATH
      **************************************************************************
      *        LPBPPATH
      *    BATCH PAYMENT PATH
      *
      * JTG 021011 ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * JTG 021213 ADDED BP-WK-GB-FILE-NAME       PIC X(5) VALUE "BTFIL".
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      **************************************************************************
       01  BP-PATH-OVERRIDE          PIC X(01) VALUE SPACES.
       01  BP-PATH                   PIC X(40) VALUE " ".
       01  BP-ALL-PATH.
           03  BP-ALL-BASE           PIC X(09) VALUE SPACES.
           03  FILLER                PIC X    VALUE "/".
           03  BP-ALL-MACHINE        PIC XX.
           03  FILLER                PIC X    VALUE "/".
           03  BP-ALL-DATA           PIC X(6).
           03  FILLER                PIC X(5) VALUE "/ED/B".
           03  BP-ALL-SUBPATH.
               05  BP-ALL-BRANCH         PIC 9(4).
               05  BP-ALL-DATE           PIC 9(4).
               05  BP-ALL-BATCH-NO       PIC 999.
               05  BP-ALL-REFCD          PIC X(5).
           03  BP-ALL-SUBPATHX       REDEFINES BP-ALL-SUBPATH.
               05  BP-ALL-BRANCHX     PIC X(4).
               05  BP-ALL-DATEX       PIC X(4).
               05  BP-ALL-BATCH-NOX   PIC XXX.
               05  BP-ALL-REFCDX      PIC X(5).

      *=================================================================
      *    /USR/DATA/R#/ED/++FIL####&&&  (#### = MMDD) (&&& = BATCH)
      *=================================================================
       78  BP-WK-BT-PATH                     VALUE "BTFIL".
       78  BP-WK-LB-PATH                     VALUE "LBFIL".

       01  BP-WK-GB-PATH-OVERRIDE         PIC X(01)  VALUE SPACES.
       01  BP-WK-GB-PATH.
           03  BP-WK-GB-FILENAME.
               05  BP-WK-GB-BASE          PIC X(09).
               05  FILLER                 REDEFINES BP-WK-GB-BASE.
                   07  BP-WK-GB-BASE-USR  PIC X(05).
                   07  BP-WK-GB-BASE-DIR  PIC X(04).
               05  FILLER                 PIC X(01)  VALUE "/".
               05  BP-WK-GB-MACHINE       PIC X(02).
               05  FILLER                 PIC X(04)  VALUE "/ED/".
               05  BP-WK-GB-FILE-NAME     PIC X(05)  VALUE "BTFIL".
           03  BP-WK-GB-SUBPATH.
               05  BP-WK-GB-DATE          PIC 9(04).
               05  BP-WK-GB-BATCH-NO      PIC 9(03).
           03  BP-WK-GB-SUBPATHX          REDEFINES BP-WK-GB-SUBPATH.
               05  BP-WK-GB-DATEX         PIC X(04).
               05  BP-WK-GB-BATCH-NOX     PIC X(03).
