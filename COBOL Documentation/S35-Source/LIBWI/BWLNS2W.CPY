      * COPYMEMBER: LIBWI/BWLNS2W
      ************************************************************
      * REV:
      *  JFF 112295 ADDED BWLNS2-BR-MACHINE & BWLNS2-BR-DATA-PATH
      *  BLF 070731 CHANGED BWLNS2-CUR-ONLY TO BWLNS2-ARCHIVE-FG,
      *             WORLD PR# 547
      *  BLM 160616 REMOVE BWLNS2-SEQNO, CHANGE BWLNS2-NUMBER TO
      *             PIC 9(8), PD0003
      *   CS 161114 CHANGED BWLNS2-BEGIN-X TO 2, COLUMNS TO 78
      *  2024.0502 BAH MADE LEVEL 01, CALL NOT WINDOW
      ************************************************************
       01  BWLNS2-BUF.
           03  BWLNS2-KEY           PIC 9(9).
           03  BWLNS2-ACCTNO        PIC 9(8).
      * MM/DD/YY
           03  BWLNS2-SYSDATE       PIC X(8).
           03  BWLNS2-SCAN-ONLY     PIC X     VALUE " ".
           03  BWLNS2-ALL-LOANS     PIC X     VALUE " ".
           03  BWLNS2-BR-MACHINE    PIC XX    VALUE " ".
           03  BWLNS2-BR-DATA-PATH  PIC X(6)  VALUE " ".
           03  BWLNS2-STATUS        PIC XX.
