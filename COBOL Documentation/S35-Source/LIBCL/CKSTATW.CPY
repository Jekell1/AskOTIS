      * COPYMEMBER: LIBCL/CKSTATW
      * BAH 170316 CHANGED DATE1 AND DATE2 TO 9(8), CHANGED RETURN-DATE
      *            TO 9(8)
       01  CKSTAT-BUF.
           03  CKSTAT-ACTION          PIC X.
           03  CKSTAT-LOW-STATUS      PIC XX.
           03  CKSTAT-HIGH-STATUS     PIC XX.
           03  CKSTAT-BEG-BRANCH      PIC 9(4).
           03  CKSTAT-GROUP REDEFINES CKSTAT-BEG-BRANCH PIC X(4).
           03  CKSTAT-END-BRANCH      PIC 9(4).
           03  CKSTAT-DATE1           PIC 9(8).
           03  CKSTAT-DATE2           PIC 9(8).
           03  CKSTAT-RETURN-STAT     PIC X.
           03  CKSTAT-RETURN-DATE     PIC 9(8).
