      * COPYMEMBER: LIBEX/EX01XLSM
      ******************************************************************
      *    LOAN F9 MISC FIELDS TRAILER
      *
      * REV:
      *  141121 BAH NEW SOAUTO PR# 4651
      *  2022.0322 ADDED XLS-M-TRANS-FROM-BRNO, XLS-M-TRANS-FROM-ACCTNO,
      *            XLS-M-TRANS-FROM-DATE, #1547
      *  2022.0923 ADDED XLS-M-MISPAY-90-DATE #1570
      *  2024.0109 ADDED XLS-M-SECOND-BEN FIELDS #1637
      *  2024.0212 ADDED FILLER #1637
      ******************************************************************
       78  XLSM-SIZE                     VALUE 907.
       01  XLSM-REC.
           03  XLS-M-BRACCTNO             PIC 9(10).
           03  XLS-M-ACCTNO               PIC 9(8).
           03  XLS-M-CODE                 PIC X.
           03  XLS-M-SEQNO                PIC 9.

           03  XLS-MISC-AREA.
               05  XLS-M-MISC-FIELD       PIC X(20) OCCURS 32.

           03  XLS-M-TRANS-FROM-BRNO      PIC 9(4).
           03  XLS-M-TRANS-FROM-ACCTNO    PIC 9(8).
           03  XLS-M-TRANS-FROM-DATE      PIC 9(8).
           03  XLS-M-MISPAY-90-DATE       PIC 9(8).
           03  XLS-M-SECOND-BEN-NAME      PIC X(32).
           03  XLS-M-SECOND-BEN-ADR       PIC X(32).
           03  XLS-M-SECOND-BEN-CITY      PIC X(16).
           03  XLS-M-SECOND-BEN-STATE     PIC X(2).
           03  XLS-M-SECOND-BEN-ZIP       PIC X(10).
           03  XLS-M-SECOND-BEN-PHONE     PIC 9(10).
           03  XLS-M-SECOND-BEN-RELATION  PIC X(15).

           03  FILLER                     PIC X(100).

           03  XLS-M-CR                    PIC X.
           03  XLS-M-LF                    PIC X.
