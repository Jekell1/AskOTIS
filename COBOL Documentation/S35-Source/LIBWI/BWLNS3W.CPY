      * COPYMEMBER: LIBWI/BWLNS3W
      ************************************************************
      * REV:
      *  JFF 112295 ADDED BWLNS3-BR-MACHINE & BWLNS3-BR-DATA-PATH
      *   CS 161115 CHANGED COLUMNS TO 78, BEGIN-X TO 2
      *  BAH 2024.0415 REMOVED BWLNS3-BR-MACHINE, BWLNS3-BR-DATA-PATH
      *  2024.0503 BAH MADE LEVEL 01, CALL NOT WINDOW
      ************************************************************
       01  BWLNS3-BUF.
           03  BWLNS3-KEY           PIC 9(9).
           03  BWLNS3-ACCTNO        PIC 9(8).
           03  BWLNS3-SYSDATE       PIC X(8).
           03  BWLNS3-SCAN-ONLY     PIC X     VALUE " ".
           03  BWLNS3-CUR-ONLY      PIC X     VALUE " ".
           03  BWLNS3-ALL-LOANS     PIC X     VALUE " ".
           03  BWLNS3-STATUS        PIC XX.
