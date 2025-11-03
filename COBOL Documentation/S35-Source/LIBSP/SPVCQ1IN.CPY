      * COPYMEMBER: LIBSP/SPVCQ1IN
      ******************************************************************
      *
      * DESCRIPTION: IO ROUTINES - VCQFILE - KEY 1
      *
      *=================================================================
      * REV:
      * SMR 2025-0307 FIXED GO TO IN OPEN OUTPUT S35Q-172
      * JKC 2025-0424 S35Q-200 ADDED UPDATE-VCQ1-FILE-VCQUPD.
      ******************************************************************
       OPEN-VCQ1-FILE.
           PERFORM OPEN-IT.
           MOVE VCQ-PATH       TO E-FILE.
           OPEN I-O VCQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-VCQ1-FILE.
           IF IO-FG = 7
             CLOSE VCQ-FILE
             GO TO OPEN-VCQ1-FILE.
           UNLOCK VCQ-FILE.

       OPEN-VCQ1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE VCQ-PATH       TO E-FILE.
           OPEN OUTPUT VCQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-VCQ1-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE VCQ-FILE
             GO TO OPEN-VCQ1-FILE-OUTPUT.
           UNLOCK VCQ-FILE.

       READ-VCQ1-FILE.
           PERFORM READ-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ1-KEY        TO E-KEYX.
           READ VCQ-FILE.
           IF IO-FG = 8
              GO TO READ-VCQ1-FILE.

       READ-VCQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ1-KEY        TO E-KEYX.
           READ VCQ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VCQ1-FILE-NEXT.

       START-VCQ1-FILE.
           PERFORM START-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ1-KEY        TO E-KEYX.
           START VCQ-FILE KEY NOT < VCQ1-KEY.
           UNLOCK VCQ-FILE.

       START-VCQ1-FILE-GREATER.
           PERFORM START-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ1-KEY        TO E-KEYX.
           START VCQ-FILE KEY > VCQ1-KEY.
           UNLOCK VCQ-FILE.

       WRITE-VCQ1-FILE.
           PERFORM WRITE-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ1-KEY        TO E-KEYX.
           WRITE VCQ-REC.
           IF IO-FG = 8
             GO TO WRITE-VCQ1-FILE.
           UNLOCK VCQ-FILE.

       REWRITE-VCQ1-FILE.
           PERFORM REWRITE-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ1-KEY        TO E-KEYX.
           REWRITE VCQ-REC.
           IF IO-FG = 8
             GO TO REWRITE-VCQ1-FILE.
           UNLOCK VCQ-FILE.

      *-----------------------------------------------------------------
      * IN  : VCQUPD-ACCTNO
      *       VCQUPD-ACTION      [C=CREATE; R=REWRITE; W=WRITE]
      *       VCQUPD-BRNO
      *       VCQUPD-VCQ-REC
      * OUT : VCQUPD-ACCTNO
      *       VCQUPD-ACTION
      *       VCQUPD-BRNO
      *       VCQUPD-VCQ-REC
      *       VCQUPD-IO-FG
      * DESC: CALL TO VCQUPD PROGRAM TO UPDATE THE VCQFILE
      *-----------------------------------------------------------------
       UPDATE-VCQ1-FILE-VCQUPD.

           MOVE "CL/VCQUPD" TO FORM-NAM.
           CALL FORM-PROGX USING VCQUPD-LINK-BUF.
           CANCEL FORM-PROGX.

           MOVE VCQUPD-VCQ-REC TO VCQ-REC.
           MOVE VCQ-CQ-REC     TO CQ-REC.
           MOVE VCQUPD-IO-FG   TO IO-FG.

      *-----------------------------------------------------------------
       DELETE-VCQ1-FILE.
           PERFORM DELETE-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ1-KEY        TO E-KEYX.
           DELETE VCQ-FILE.
           IF IO-FG = 8
             GO TO DELETE-VCQ1-FILE.
           UNLOCK VCQ-FILE.

       CLOSE-VCQ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE VCQ-FILE.
