       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EOSEQ.
       AUTHOR.          BLV.
       DATE-WRITTEN.    062600.
      *****************************************************************
      *                           (SP)
      *  SP/EOSEQ.C
      *  -----------
      *
      *  DESCRIPTION:  RENUMBERS STEPS IN A BATCH PROCESS
      *
      *=================================================================
      * REV:
      * KEC 2016.0226 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR EO-REC (HOLD/ORIG/SAVE/WK).
      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *=================================================================
      *COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

      ***************************************************
      *   EO TEMPORARY WORK RECORD
      ***************************************************
       FD  WK-FILE   LABEL RECORDS ARE STANDARD.
      *=================================================================
      *   WK-REC FROM 'REPLACING LEADING' ON EO-REC.
      *=================================================================
       COPY "LIBGB/GB01EO.CPY" REPLACING LEADING "EO" BY "WK".

      *****************************************************************
      *      WORKING STORAGE SECTION
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)SP/EOSEQ.CBL S34 06/19/21-12:05:32 >".
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".

       01  WK-IFN            PIC X(2)  VALUE "WK".

       01  NEW-STEP          PIC 9(5)  VALUE 1.

       01  ERRCD             PIC X     VALUE " ".

       01  ACCEPT-FG         PIC X     VALUE " ".

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".

      *-----------------------------------------------------------------
      * SEND LEGEND MESSAGES
      *-----------------------------------------------------------------
       01  PCODE-MESS.
           03  FILLER        PIC X(60)  VALUE
           "ENTER PROCEDURE CODE TO RE-SEQUENCE:".
       01  START-STEP-MESS.
           03  FILLER        PIC X(60)  VALUE
           "ENTER NEW STARTING STEP NUMBER:".
       01  FACTOR1-MESS.
           03  FILLER        PIC X(60)  VALUE
           "ENTER FACTOR TO INCREMENT NEW STEPS:".
       01  ACCEPT-MESS.
           03  FILLER        PIC X(60)  VALUE
           "ENTER ACCEPT ( Y / N ):".

      *-----------------------------------------------------------------
      * SCREEN INPUT FIELDS
      *-----------------------------------------------------------------
       01  PCODE             PIC X(3)      VALUE SPACES.
       01  START-STEP        PIC 9(4)      VALUE 0.
       01  FACTOR1           PIC 9(4)      VALUE 0.

      *-----------------------------------------------------------------
      * COPY MEMBERS & WORKERS
      *-----------------------------------------------------------------
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  BRXCPY-SEQ                   VALUE 0.
       01  EOSEQ-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/EOSEQ_DEF.CPY".
       COPY "LIBSP/EOSEQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/EOSEQ_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-EO1-FILE.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *       MAIN PROGRAM CONTROL
      *****************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE "EOSEQ" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS EOSEQ-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIAL-ROUTINE.
           IF ERRCD NOT = " "
              GO TO END-ROUTINE.

           PERFORM OPEN-EO1-FILE.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE PCODE TO EO-PCODE.
           MOVE 0 TO EO-STEP.
           PERFORM START-EO1-FILE.

       READ-NEXT-EO.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO DELETE-ROUTINE.
           IF EO-PCODE NOT = PCODE
              GO TO DELETE-ROUTINE.

           MOVE EO-REC TO WK-REC.
           IF EO-STEP = 0
              MOVE 0 TO WK-STEP
              MOVE START-STEP TO NEW-STEP
           ELSE
              MOVE NEW-STEP TO WK-STEP.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF WK-STEP > 0
              ADD FACTOR1 TO NEW-STEP.

           IF NEW-STEP > 9999
              MOVE "RESEQUENCING ERROR: RETRY WITH LOWER INCREMENT!"
                  TO MESS
              PERFORM SEND-MESS
              GO TO PROCESS-COMPLETE.

           GO TO READ-NEXT-EO.

       DELETE-ROUTINE.
           MOVE PCODE TO EO-PCODE.
           MOVE 0 TO EO-STEP.
           PERFORM START-EO1-FILE.
       DELETE-NEXT-EO.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CREATE-NEW-SEQUENCES.

           IF EO-PCODE NOT = PCODE
              GO TO CREATE-NEW-SEQUENCES.

           PERFORM DELETE-EO1-FILE.

           GO TO DELETE-NEXT-EO.

       CREATE-NEW-SEQUENCES.
           MOVE PCODE TO WK-PCODE.
           MOVE 0 TO WK-STEP.
           PERFORM START-WK-FILE.
       READ-NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO PROCESS-COMPLETE.

           IF WK-PCODE NOT = PCODE
              GO TO PROCESS-COMPLETE.

           MOVE WK-REC TO EO-REC.
           PERFORM WRITE-EO1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO READ-NEXT-WK.

       PROCESS-COMPLETE.
           PERFORM CLOSE-EO1-FILE.
           PERFORM CLOSE-WK-FILE.
           GO TO MAIN-PROGRAM.

      *******************************************************
       INITIAL-ROUTINE SECTION.
      *******************************************************

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE TEMP FILE" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

       INIT-ACCEPT-PCODE.
           MOVE PCODE-MESS TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040PCODE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-PCODE.
           MOVE VDUBUF TO PCODE EO-PCODE.
           MOVE 0 TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM READ-EO1-FILE.
           PERFORM CLOSE-EO1-FILE.
           IF IO-BAD
              MOVE "INVALID BATCH PROCEDURE" TO MESS
              PERFORM SEND-MESS
              GO TO INIT-ACCEPT-PCODE.
           MOVE "S  A000PDESC." TO VDU.
           MOVE EO-COMMENT TO VDUBUF.
           PERFORM SCREENIO.

       INIT-ACCEPT-START-STEP.
           MOVE START-STEP-MESS TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040STARTSTEP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.
           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-PCODE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-START-STEP.
           MOVE VDUNUM0 TO START-STEP.

       INIT-ACCEPT-FACTOR1.
           MOVE FACTOR1-MESS TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040FACTOR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.
           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-START-STEP.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-FACTOR1.
           MOVE VDUNUM0 TO FACTOR1.

       INIT-ACCEPT-ACCEPT.
           MOVE ACCEPT-MESS TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.
           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-FACTOR1.
           IF VDUSTATUS NOT = "00"
              GO TO INIT-ACCEPT-ACCEPT.
           MOVE VDUBUF TO ACCEPT-FG.
           IF NOT (ACCEPT-FG = "Y" OR "N")
              GO TO INIT-ACCEPT-ACCEPT.

           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO ERRCD.
       INITIALIZE-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO EOSEQ-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EOSEQ-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EOSEQ-SCREEN UPON EOSEQ-WINDOW-HANDLE.
           MOVE EOSEQ-FORM-FIELDS TO WR-BUF.
           MOVE EOSEQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE EOSEQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOSEQ-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/EOSEQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-PARA SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *****************************************************************
       IO-ROUTINE SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/GBEO1IN.CPY".
       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
             GO TO OPEN-WK-FILE.
           IF IO-FG = 7
             CLOSE WK-FILE
             GO TO OPEN-WK-FILE.
           UNLOCK WK-FILE.
       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
             GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE WK-FILE
             GO TO OPEN-WK-FILE-OUTPUT.
           UNLOCK WK-FILE.
       READ-WK-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-WK-FILE-NEXT.
       START-WK-FILE.
           PERFORM START-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK1-KEY.
           UNLOCK WK-FILE.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
             GO TO WRITE-WK-FILE.
           UNLOCK WK-FILE.
       REWRITE-WK-FILE.
           PERFORM REWRITE-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************************************
       END-ROUTINE SECTION.
      *******************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EOSEQ-SCREEN.
           DESTROY EOSEQ-WINDOW-HANDLE.
           EXIT PROGRAM.
