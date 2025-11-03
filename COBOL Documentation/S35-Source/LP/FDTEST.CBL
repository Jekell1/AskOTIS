       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDTEST.
      *************************************************************************
      *   DIR :  LP
      *   DESC:  TEST VARIABLE FORM PRINT
      *          SCREEN INPUT TAKES PLACE HERE, THEN THIS CALLS LONPMB
      *          AND THIS IS ONLY PROGRAM THAT CALLS LONPMB
      *
      * REV:
      *  BAH 110914 REPLACED RRFORM WITH SCREEN
      *  BLM 120320 ADDED XFER OF SCREEN-BUF BEFORE CALLING LONPMB; BEG-FM
      *             HAD A SPACE IN FRONT ON IT WHEN WE GOT TO LONPMB SO
      *             FORM RANGE DIDN'T WORK
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/FDTEST.CBL S35 05/08/23-09:00:59 >".
       01  EXT-EOBUF-MESS        PIC X(60).

       01  LINK-BUF      VALUE " ".
           03  FM1-BUF     PIC 999.
           03  FM2-BUF     PIC 999.
           03  DYN-BUF     PIC X.
           03  TEST-ONLY   PIC X.
           03  LONPMB-WINDOW-HANDLE PIC X(10).


       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPMB_DEF.CPY".
       COPY "LIBLP/LONPMB_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPMB_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************
       MAIN-PROGRAM SECTION.
      **************************

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LP/LONPMB" TO FORM-NAM.
           MOVE "LONPMB" TO VDU-FORM-NAME.
           MOVE FORM-PATH TO FORM-PATHNAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
      *            CELL WIDTH 10
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS LONPMB-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       COPY "LIBGB/CHKSEC.CPY".

       SETRP.
           MOVE LONPMB-QUEUE-POS TO Q-POS.
           MOVE LONPMB-COPIES-POS TO C-POS.
           MOVE LONPMB-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       FM-01.
           MOVE "ENNN030FM1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-FM.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FM-01.
           MOVE VDUNUM0 TO FM1-BUF.

       FM-02.
           MOVE "ENNN030FM2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO FM-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FM-02.
           MOVE VDUNUM0 TO FM2-BUF.
           IF FM1-BUF > FM2-BUF
              GO TO FM-01.

           GO TO DESCYN.
       ALL-FM.
           MOVE "S  A030FM1." TO VDU.
           MOVE 001 TO FM1-BUF VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030FM2." TO VDU.
           MOVE "999" TO FM2-BUF VDUBUF.
           PERFORM SCREENIO.
       DESCYN.
           MOVE "E  A010DYN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO FM-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DESCYN.
           MOVE VDUBUF TO DYN-BUF.
           IF DYN-BUF = "N" GO TO CONT-PROG.
           IF DYN-BUF NOT = "Y" GO TO DESCYN.

       CONT-PROG.

           MOVE "LP/LONPMB" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LINK-BUF PRU-PATH.
           CANCEL FORM-PROGX.

           PERFORM PRU-OUT.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO LONPMB-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LONPMB-SYSDATE.
           DISPLAY LONPMB-SCREEN UPON LONPMB-WINDOW-HANDLE.
           MOVE LONPMB-FORM-FIELDS TO WR-BUF.
           MOVE LONPMB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE LONPMB-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPMB-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPMB_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************
       MISC SECTION.
      ************************
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *************************
       END-ROUTINE SECTION.
      *************************
       EXIT-PROG.
       ENDIT.
           MOVE "LP/FDMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPMB-SCREEN.
           DESTROY LONPMB-WINDOW-HANDLE.
           EXIT PROGRAM.
