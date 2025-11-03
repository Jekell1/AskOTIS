      * COPYMEMBER: LIBSP/SPMEMO.CMD
      ***************************************************
      *              SPMEMO.CMD
      *                                                 
      *   WORKERS FOR SPINQ1 MEMO ENTRY
      * REV:
      * GLG 101698 ADDED NOTE MEMOS STUFF
      * BAH 071226 CHANGED 'RETURN' TO 'ENTER'
      ***************************************************
       01  LM-MSG1.
           03  FILLER            PIC X(12) VALUE
           "LOAN MEMOS: ".
           03  FILLER            PIC X(32) VALUE
           "F7-EXIT, F3-PRIORITY, ^/v-SCROLL".
           03  FILLER            PIC X(13) VALUE
           ", ENTER DATE:".
       01  LM-MSG2.
           03  FILLER            PIC X(12) VALUE
           "LOAN MEMOS: ".
           03  FILLER            PIC X(42) VALUE
           "^-BACK, <ENTER>-NEXT SEQNO: ".
       01  LM-MSG3.
           03  FILLER            PIC X(12) VALUE
           "LOAN MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  LM-MSG3-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "LOAN MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-BOR MEMO, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  LM-MSG3-AUDIT.
           03  FILLER            PIC X(12) VALUE
           "LOAN MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE'  ".
       01  LM-MSG3-AUDIT-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "LOAN MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-BOR MEMO, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE'  ".
       01  LM-MSG3-ARCHIVED.
           03  FILLER            PIC X(12) VALUE
           "LOAN MEMOS: ".
           03  FILLER            PIC X(21) VALUE
           "F7-EXIT, ^/v-SCROLL. ".
           03  FILLER            PIC X(45) VALUE
           "  (ARCHIVED LOAN: NO MODIFICATIONS ALLOWED)  ".
       01  BM-MSG1.
           03  FILLER            PIC X(12) VALUE
           "BORR MEMOS: ".
           03  FILLER            PIC X(32) VALUE
           "F7-EXIT, F3-PRIORITY, ^/v-SCROLL".
           03  FILLER            PIC X(13) VALUE
           ", ENTER DATE:".
       01  BM-MSG2.
           03  FILLER            PIC X(12) VALUE
           "BORR MEMOS: ".
           03  FILLER            PIC X(42) VALUE
           "^-BACK, <ENTER>-NEXT SEQNO: ".
       01  BM-MSG3.
           03  FILLER            PIC X(12) VALUE
           "BORR MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  BM-MSG3-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "BORR MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-LN MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  BM-MSG3-DL-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "BORR MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-DL MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  BM-MSG3-AUDIT.
           03  FILLER            PIC X(12) VALUE
           "BORR MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".
       01  BM-MSG3-AUDIT-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "BORR MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-LN MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".

       01  DM-MSG1.
           03  FILLER            PIC X(12) VALUE
           "DELR MEMOS: ".
           03  FILLER            PIC X(32) VALUE
           "F7-EXIT, F3-PRIORITY, ^/v-SCROLL".
           03  FILLER            PIC X(13) VALUE
           ", ENTER DATE:".
       01  DM-MSG2.
           03  FILLER            PIC X(12) VALUE
           "DELR MEMOS: ".
           03  FILLER            PIC X(42) VALUE
           "^-BACK, <ENTER>-NEXT SEQNO:".
       01  DM-MSG3.
           03  FILLER            PIC X(12) VALUE
           "DELR MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  DM-MSG3-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "DELR MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-LN MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  DM-MSG3-AUDIT.
           03  FILLER            PIC X(12) VALUE
           "DELR MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".
       01  DM-MSG3-AUDIT-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "DELR MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-LN MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".
       01  AM-MSG1.
           03  FILLER            PIC X(12) VALUE
           "APPL MEMOS: ".
           03  FILLER            PIC X(32) VALUE
           "F7-EXIT, F3-PRIORITY, ^/v-SCROLL".
           03  FILLER            PIC X(13) VALUE
           ", ENTER DATE:".
       01  AM-MSG2.
           03  FILLER            PIC X(12) VALUE
           "APPL MEMOS: ".
           03  FILLER            PIC X(42) VALUE
           "^-BACK, <ENTER>-NEXT SEQNO:".
       01  AM-MSG3.
           03  FILLER            PIC X(12) VALUE
           "APPL MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  AM-MSG3-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "APPL MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-LN MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  AM-MSG3-AUDIT.
           03  FILLER            PIC X(12) VALUE
           "APPL MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".
       01  AM-MSG3-AUDIT-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "APPL MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-LN MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".

       01  NM-MSG1.
           03  FILLER            PIC X(12) VALUE
           "NOTE MEMOS: ".
           03  FILLER            PIC X(32) VALUE
           "F7-EXIT, F3-PRIORITY, ^/v-SCROLL".
           03  FILLER            PIC X(13) VALUE
           ", ENTER DATE:".
       01  NM-MSG2.
           03  FILLER            PIC X(12) VALUE
           "NOTE MEMOS: ".
           03  FILLER            PIC X(42) VALUE
           "^-BACK, <ENTER>-NEXT SEQNO:".
       01  NM-MSG3.
           03  FILLER            PIC X(12) VALUE
           "NOTE MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  NM-MSG3-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "NOTE MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-INV MEMO, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  NM-MSG3-AUDIT.
           03  FILLER            PIC X(12) VALUE
           "NOTE MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE'  ".
       01  NM-MSG3-AUDIT-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "NOTE MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-INV MEMO, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE'  ".

       01  IM-MSG1.
           03  FILLER            PIC X(12) VALUE
           "INV MEMOS:  ".
           03  FILLER            PIC X(32) VALUE
           "F7-EXIT, F3-PRIORITY, ^/v-SCROLL".
           03  FILLER            PIC X(13) VALUE
           ", ENTER DATE:".
       01  IM-MSG2.
           03  FILLER            PIC X(12) VALUE
           "INV MEMOS:  ".
           03  FILLER            PIC X(42) VALUE
           "^-BACK, <ENTER>-NEXT SEQNO:".
       01  IM-MSG3.
           03  FILLER            PIC X(12) VALUE
           "INV MEMOS:  ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  IM-MSG3-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "INV MEMOS:  ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-NT MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  IM-MSG3-AUDIT.
           03  FILLER            PIC X(12) VALUE
           "INV MEMOS:  ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".
       01  IM-MSG3-AUDIT-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "INV MEMOS:  ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-NT MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".

       01  VM-MSG1.
           03  FILLER            PIC X(12) VALUE
           "VEND MEMOS: ".
           03  FILLER            PIC X(32) VALUE
           "F7-EXIT, F3-PRIORITY, ^/v-SCROLL".
           03  FILLER            PIC X(13) VALUE
           ", ENTER DATE:".
       01  VM-MSG2.
           03  FILLER            PIC X(12) VALUE
           "VEND MEMOS: ".
           03  FILLER            PIC X(42) VALUE
           "^-BACK, <ENTER>-NEXT SEQNO:".
       01  VM-MSG3.
           03  FILLER            PIC X(12) VALUE
           "VEND MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  VM-MSG3-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "VEND MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-XX MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, ENTER MEMO".
       01  VM-MSG3-AUDIT.
           03  FILLER            PIC X(12) VALUE
           "VEND MEMOS: ".
           03  FILLER            PIC X(22) VALUE
           "F3-PRIORITY, F4-DATE, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".
       01  VM-MSG3-AUDIT-TOGGLE.
           03  FILLER            PIC X(12) VALUE
           "VEND MEMOS: ".
           03  FILLER            PIC X(35) VALUE
           "F3-PRIORITY, F4-DATE, F6-XX MEMOS, ".
           03  FILLER            PIC X(31) VALUE
           "F7-EXIT, ^/v-SCROLL, 'DELETE' ".
