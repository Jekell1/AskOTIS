       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENDMTH.
      ******************************************************************
      *                (GL)
      *      G/L END OF MONTH PROCESSING
      *
      * GLB 990614 ADD TAG FILE TO DOCUMENT PURGE, PR# 776
      *  MG 000201 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLV 040917 ADDED TIMER LOGIC
      * BLF 050505 DON'T ALLOW PURGING TRANSACTIONS < 12 MONTHS OLD.
      *            ONLY ALLOW PURGING TRANSACTIONS 12-23 MONTHS OLD WITH
      *            PASSWORD (GL, ENDMTH, OPTION 1).  PR# 2525
      * BLF 051116 IF GP-I-A-WORLD, ALLOW PURGING AS LONG AS TRANSACTIONS
      *            AREN'T FROM CURRENT FISCAL
      * KEC 2016.1201 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170327 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 190718 CHANGED THE CHECK/CREATE TAG FILES TO USE GL/GLLOGS
      *            NEW FILE
      *  CS 190726 ADD IO-CHECK TO WRITE OF GLLOGS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/ENDMTH.CBL J35 12/21/23-08:32:51 >".

       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GLLOGS.CPY".
       COPY "LIBGL/GL01GLLOGS_SQL.CPY".
       COPY "LIBGL/GLWSGLLOGS.CPY".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-DATE         PIC 9(8).
           03  END-DATE         PIC 9(8).

       01  VERSION-CONTROL    PIC X VALUE "A".

       01  SEQ                PIC 999 VALUE 0.
       01  MESS-LIST          PIC X(5)      VALUE "MESS.".

       01  TODAYS-DATE     PIC 9(8).
       01  TOTAL-RECS-PURGED    PIC 9(11) VALUE ZEROS.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/TIMERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/ENDMTH_DEF.CPY".
       COPY "LIBWI/TIMER_DEF.CPY".
       COPY "LIBGL/ENDMTH_WKS.CPY".
       COPY "LIBWI/TIMER_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/ENDMTH_SCN.CPY".
       COPY "LIBWI/TIMER_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *******************************************************
       MAIN-MODULE SECTION.
      *******************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ENDMTH" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF EXT-ROUTE-BUF = "00"
              MOVE "** GL TRANSACTION PURGE IN PROGRESS **" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM CREATE-GLLOGS-LOG-FILE.
           IF STAT-BAD
               GO TO END-PROGRAM.

           PERFORM CLEAR-GT-FILE.

           MOVE TOTAL-RECS-PURGED TO GLLOGS-RECS-PURGED.
           PERFORM REWRITE-GLLOGS1-FILE.
           GO TO END-PROGRAM.

      *******************************************************
       INITIALIZATION SECTION.
      *******************************************************
           MOVE 0 TO Q-POS
                     C-POS
                     P-POS.
           MOVE "V"  TO Q-BUF.
           MOVE "1"  TO C-BUF.
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM CHECK-PREVIOUSLY-RUN.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO SYS-DATE
              MOVE END-DATE TO NUM-DATE
              PERFORM TIM
              IF ELAPSED-MONTHS < 25
                 GO TO ENDIT.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *****************************************
       ENTER-PARAMETERS SECTION.
      *****************************************
       ENTER-PARM.

       REC-BEG-DATE.
           MOVE "00" TO VDUSTATUS.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO REC-END-DATE.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO REC-BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

       REC-END-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO REC-BEG-DATE.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO REC-END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF BEG-DATE > END-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO REC-BEG-DATE.

       CHECK-DATE-RANGE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.

           IF END-DATE >= GP-BEG-FISCAL-DATE
              MOVE "CANNOT PURGE TRANSACTIONS IN CURRENT FISCAL YEAR!"
                                           TO MESS
              PERFORM SEND-MESS
              GO TO REC-BEG-DATE.

       ENTER-ACCEPT-YN.
           MOVE "Y" TO NOACCEPT-BUF.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO REC-END-DATE.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ TIMER  }
           IF ( VDUBUF = "T" ) PERFORM TIMER-ROUTINE
                               MOVE "ENDMTH" TO VDU-FORM-NAME
                               MOVE TIMER-ACCEPT-YN TO VDUBUF.

           IF ( VDUBUF     = "N" ) GO TO ENTER-PARM.
           IF ( VDUBUF NOT = "Y" ) GO TO ENTER-ACCEPT-YN.

       EXIT-PARM.
           EXIT.

      ******************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************

      ******************************************
       CLEAR-GT-FILE SECTION.
      ******************************************

           PERFORM OPEN-GT2-FILE.

           MOVE BEG-DATE TO GT-DATE1 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1.

           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           PERFORM START-GT2-FILE.
             
       CLEAR-NEXT-GT.
           PERFORM READ-GT2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CLEAR-GT.

           IF GT-FLAG NOT = "P"
      *       UNLOCK GT-FILE
              GO TO CLEAR-NEXT-GT.

      * GT-DATE1 STORED AS CCYYMMDD
           IF GT-DATE1 < BEG-DATE OR GT-DATE1 > END-DATE
      *       UNLOCK GT-FILE
              GO TO CLEAR-NEXT-GT.

           PERFORM DELETE-GT2-FILE.

           ADD 1 TO TOTAL-RECS-PURGED.  

           GO TO CLEAR-NEXT-GT.
       EXIT-CLEAR-GT.
           PERFORM CLOSE-GT2-FILE.

      ****************************************************
       CHECK-PREVIOUSLY-RUN SECTION.
      ****************************************************

           PERFORM OPEN-GLLOGS1-FILE.
          
      * THIS IS BEING DONE, JUST TO GET THE NEXT SEQUENCE IF ENDMTH
      * HAS ALREADY BEEN RUN FOR THIS DATE.
           MOVE 0 TO SEQ.

           MOVE "ENDMTH"    TO GLLOGS-ID
                               QGLLOGS1-WBEG-ID
                               QGLLOGS1-WEND-ID.
           ACCEPT GLLOGS-SYSTEM-DATE FROM CENTURY-DATE
           MOVE GLLOGS-SYSTEM-DATE   TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS1-WBEG-SYSTEM-DATE
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS1-WEND-SYSTEM-DATE
           MOVE SEQ                  TO GLLOGS-SEQNO
                                        QGLLOGS1-WBEG-SEQNO.
           MOVE ALL "9"              TO QGLLOGS1-WEND-SEQNO.

           PERFORM START-GLLOGS1-FILE.

       CHECK-PREV-NEXT.

           PERFORM READ-GLLOGS1-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CHECK-PREVIOUSLY-RUN.

           IF GLLOGS-ID NOT = "ENDMTH"
              GO TO EXIT-CHECK-PREVIOUSLY-RUN.

           ADD 1 TO SEQ.
           IF SEQ > 99
              MOVE "LIMIT OF 99 SEQNO EXCEEDED!" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           GO TO CHECK-PREV-NEXT.

       EXIT-CHECK-PREVIOUSLY-RUN.
           EXIT.

      ********************************************
       CREATE-GLLOGS-LOG-FILE SECTION.
      ********************************************

           PERFORM CLEAR-GLLOGS-FILE.

           MOVE "ENDMTH"    TO GLLOGS-ID.
           ACCEPT GLLOGS-SYSTEM-DATE FROM CENTURY-DATE.
           MOVE SEQ         TO GLLOGS-SEQNO.

           MOVE BEG-DATE    TO GLLOGS-BEG-DATE.
           MOVE END-DATE    TO GLLOGS-END-DATE.
           PERFORM GET-TIME.
           MOVE HH-MM-SS    TO GLLOGS-TIME.
           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF    TO GLLOGS-USERID. 
           PERFORM WRITE-GLLOGS1-FILE.
           IF IO-BAD
              MOVE 99 TO STAT. 

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO ENDMTH-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ENDMTH-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ENDMTH-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ENDMTH-FORM-FIELDS TO WR-BUF.
           MOVE ENDMTH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE ENDMTH-HELP--FILE TO HELP-LINK-FILE.
           MOVE ENDMTH-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/ENDMTH_EVA.CPY".
              COPY "LIBWI/TIMER_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGL/GLGLLOGSCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/TIMER.CPY".
      
      ***********************************************
       MISC-ROUTINES SECTION.
      ***********************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      ***********************************************
       IO-ROUTINE SECTION.
      ***********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-GT2-FILE.
           PERFORM CLOSE-GLLOGS1-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLLOGSGS_SQL.CPY".
       COPY "LIBGL/GLGT2IN.CPY".
       COPY "LIBGL/GLGLLOGS1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***********************************************
       END-PROGRAM SECTION.
      ***********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/EMPUMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ENDMTH-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
