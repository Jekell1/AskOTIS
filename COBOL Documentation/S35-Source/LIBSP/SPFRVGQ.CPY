      * COPYMEMBER: LIBSP/SPFRVGQ
      ******************************************************************
      * IDENTIFICATION: LIBSP/SPFRVGQ
      * DESCRIPTION   : TEMPORARY VISION FILE CREATION - VGQFILE
      *                 VGQFILE IS TO RESOLVE DIFFICULTY WITH THE 
      *                 GQ2-KEY WORKING WITH A SCAN WINDOW AND/OR
      *                 USING A START-GREATER IO PARAGRAPH.  USING A
      *                 TEMPORARY VISION GQFILE HELPS KEEP THE SAME
      *                 EXPECT RESULTS FROM A15.
      *
      *=================================================================
      * REV :
      * SMR 2024-0205 ***NEW***
      *
      ******************************************************************
      *
      *   C R E A T E - V C Q - F I L E
      *
      *=================================================================
      * IN  : GQ-PATH-OVERRIDE
      *       FILPATH-BASE
      *       FILPATH-MACHINE
      *       FILPATH-DATA
      * OUT : VGQ-FILE                                    
      * DESC: THIS LOGIC LOOPS THRU THE GQ-FILE AND WRITES TO THE
      *       VGQ-FILE.
      ******************************************************************
       CREATE-VGQ-FILE SECTION.

           IF (VGQ-PATH = SPACES)
              MOVE MKTEMP-DEFAULT      TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              MOVE MKTEMP-BUF          TO VGQ-PATH.

           PERFORM OPEN-VGQ1-FILE-OUTPUT.
           PERFORM CLOSE-VGQ1-FILE.
           PERFORM OPEN-VGQ1-FILE.

           PERFORM OPEN-GQ1-FILE.

           MOVE 0                      TO GQ-BRNO.  *> REPLACED IN START
           MOVE 0                      TO GQ-ACCTNO.
 
           PERFORM START-GQ1-FILE.

      *=================================================================
       CREATE-VGQ-NEXT-GQ1.

           PERFORM READ-GQ1-FILE-NEXT.

           IF IO-FG NOT = 0
              GO TO CREATE-VGQ1-FILE-END.

      *    IF GQ-BRNO NOT = GQ-PATH-OWNBR
      *       GO TO CREATE-VGQ1-FILE-END.

           PERFORM CLEAR-VGQ1-GQ2-KEY.
           MOVE SPACES                 TO VGQ-GQ-REC.

           PERFORM LOAD-VGQ1-GQ2-KEY.
           MOVE GQ-REC                 TO VGQ-GQ-REC. 
             
           PERFORM WRITE-VGQ1-FILE.

           GO TO CREATE-VGQ-NEXT-GQ1.

      *=================================================================
       CREATE-VGQ1-FILE-END.

           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-VGQ1-FILE. 

       CREATE-VGQ-FILE-EXIT.
           EXIT.

      ******************************************************************
       CLEAR-VGQ1-GQ2-KEY SECTION.

           MOVE SPACES                TO VGQ-COLLID.
           MOVE SPACES                TO VGQ-SEQ-FG.
           MOVE ZEROES                TO VGQ-DATE.
           MOVE ZEROES                TO VGQ-START-TIME.
           MOVE ZEROES                TO VGQ-REV-DEL.
           MOVE SPACES                TO VGQ-MISC-KEY-FLD.
           MOVE ZEROES                TO VGQ-REV-PDUE-DAYS.
           MOVE ZEROES                TO VGQ-REV-BAL.
           MOVE ZEROES                TO VGQ-SEQ.
           MOVE SPACES                TO VGQ-GQ-REC.

       CLEAR-VGQ1-GQ2-KEY-EXIT.
           EXIT.

      ******************************************************************
       LOAD-VGQ1-GQ2-KEY SECTION.

           MOVE GQ-COLLID             TO VGQ-COLLID.
           MOVE GQ-SEQ-FG             TO VGQ-SEQ-FG.
           MOVE GQ-DATE               TO VGQ-DATE.
           MOVE GQ-START-TIME         TO VGQ-START-TIME.
           MOVE GQ-REV-DEL            TO VGQ-REV-DEL.
           MOVE GQ-MISC-KEY-FLD       TO VGQ-MISC-KEY-FLD.
           MOVE GQ-REV-PDUE-DAYS      TO VGQ-REV-PDUE-DAYS.
           MOVE GQ-REV-BAL            TO VGQ-REV-BAL.
           MOVE GQ-SEQ                TO VGQ-SEQ.
           MOVE GQ-REC                TO VGQ-GQ-REC.

       LOAD-VGQ1-GQ2-KEY-EXIT.
           EXIT.
