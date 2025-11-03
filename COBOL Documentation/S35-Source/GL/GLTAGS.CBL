       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLTAGS.
      ******************************************************************
      *  DIR : GL
      *  DESC: GL PURGE LOG LISTING               
      *
      * 20190716 *NEW*   LOG GL/ENDMTH ACTIVITY AND GL END OF YEAR 
      *                  REPLACES TAG FILES
      * 20190722 PER KEN, MAKE THE FIRST KEY BY SYSTEM DATE, I ADDED
      *          A 2ND KEY SO YOU COULD RUN BY PURGE DATE RANGE WHICH
      *          WOULD HELP DETERMINE IF A PURGE WAS RUN.
      *
      *          WHEN RUNNING BY SEQUENCE 1(SYSTEM DATE) YOU MUST RUN
      *          FOR THE DATE THE PURGE HAPPENED ON OR USE F3.
      *          FOR EXAMPLE: IF LOOKING TO SEE IF A PURGE OCCURED 
      *                       PURGING A SPECIFIC MONTH, YOU WOULD 
      *                       NEED TO USE F3 AT THE DATE UNLESS YOU
      *                       KNEW WHEN RUN.
      *
      *          USING SEQ2:
      *                       USE THE PURGE DATES YOU ARE LOOKING FOR
      *                       IN THE ABOVE YOU WOULD ENTER 5/1/17 
      *                       THRU 5/31 AND THOSE ARE THE PURGE 
      *                       ACTIVITY YOU WILL SEE 
      *   20190725 WASNT CHECKING TO SEE IF THE GLLOGS-ID WAS DIFFERENT
      *            THAN TAG-ID 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/GLTAGS.CBL J35 12/21/23-08:34:37 >".
       COPY "LIBGL/GL01GLLOGS.CPY".
       COPY "LIBGL/GL01GLLOGS_SQL.CPY".
       COPY "LIBGL/GLWSGLLOGS.CPY".

      *******************************
      *    PROGRAM WORKERS
      *******************************

       01  LN-WORKERS.
           03  LN-FEED           PIC 99       VALUE 0.
           03  LN-COUNT          PIC 99       VALUE 62.
           03  LN-WK             PIC 99       VALUE 0.
       01  PAGE-NUMBER           PIC 999      VALUE 0.

       01  MESS-LIST             PIC X(5)  VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  TAG-ID            PIC X(6).
           03  DATE-SEQ          PIC 9.
           03  BEG-DATE          PIC 9(8).
           03  END-DATE          PIC 9(8).
           03  ERRCD             PIC X.

       01 VERSION-CONTROL PIC X VALUE "A".

      **********************************
      *    PRINT BUFFERS
      **********************************

       01  HEAD1.
           03  HD-DATE           PIC X(8)     VALUE SPACES.
           03  FILLER            PIC XX       VALUE SPACES.
           03  HD-TIME           PIC X(8) VALUE SPACES.
           03  FILLER            PIC X(33)    VALUE SPACES.
           03  HD-NAME           PIC X(53)    VALUE SPACES.
           03  FILLER            PIC X(5)     VALUE "PAGE ".
           03  HD-PAGE-NO        PIC ZZ9      VALUE SPACES.

       01  HEAD2.
           03  FILLER            PIC X(10)    VALUE "USER ID:".
           03  HD-USERID         PIC X(28).
           03  H-TITLE           PIC X(57).
           03  FILLER            PIC X(17) VALUE SPACES.

      *01  HEAD3.
      *    03  FILLER            PIC X(50) VALUE
      *    "I.D         DATE SEQ  BEG DATE  END DATE  HHMMSS  ".  
      *    03  FILLER            PIC X(26) VALUE
      *    "USERID            # PURGED".
      
       01  HEAD3.
           03  FILLER            PIC X(55) VALUE
           "I.D.                     DATE   SEQ  BEG DATE  END DATE".
           03  FILLER            PIC X(36) VALUE
           "    TIME  USERID            # PURGED".
           
       01  HEAD4.
           03  FILLER            PIC X(49) VALUE
           "I.D.                 BEG DATE  END DATE      DATE".
           03  FILLER            PIC X(45) VALUE
           "   SEQ  HHMMSS   USERID           # PURGED".

       01  HEAD5.
           03  FILLER            PIC X(53) VALUE
           "I.D.         BRNO      DATE   SEQ  BEG DATE  END DATE".
           03  FILLER            PIC X(16) VALUE
           "    TIME  USERID".

       01  HEAD6.
           03  FILLER            PIC X(53) VALUE
           "I.D.         BRNO  BEG DATE  END DATE      DATE   SEQ".
           03  FILLER            PIC X(16) VALUE
           "    TIME  USERID".



       01  DETAIL-LINE           PIC X(132).

       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D1-ID             PIC X(6).
           03  D1-BEG-DATE1      PIC 9(6).
           03  D1-DASH           PIC X.
           03  D1-END-DATE1      PIC 9(6).
           03  FILLER            PIC X(2).
           03  D1-SYSTEM-DATE PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D1-SEQ            PIC 9(4).
           03  FILLER            PIC X(2).
           03  D1-BEG-DATE       PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D1-END-DATE       PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D1-TIME           PIC 9(6).
           03  FILLER            PIC X(2).
           03  D1-USERID         PIC X(10).
           03  FILLER            PIC X(2).
           03  D1-PURGED         PIC ZZ,ZZZ,ZZZ,ZZ9.

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D2-ID             PIC X(6).
           03  D2-END-DATE1      PIC 999999.
           03  D2-DASH           PIC X.
           03  D2-BRNO-RANGE     PIC 9(4).
           03  FILLER            PIC X(2).
           03  D2-DATE           PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D2-SEQ            PIC 9(4).
           03  FILLER            PIC X(2).
           03  D2-BEG-DATE       PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D2-END-DATE       PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D2-TIME           PIC 9(6).
           03  FILLER            PIC X(2).
           03  D2-USERID         PIC X(10).
           

       01  DETAIL-LINE3 REDEFINES DETAIL-LINE.
           03  D3-ID             PIC X(6).
           03  D3-BEG-DATE1      PIC 9(6).
           03  D3-DASH           PIC X.
           03  D3-END-DATE1      PIC 9(6).
           03  FILLER            PIC X(2).
           03  D3-BEG-DATE       PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D3-END-DATE       PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D3-SYSTEM-DATE PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D3-SEQ            PIC 9(4).
           03  FILLER            PIC X(2).
           03  D3-TIME           PIC 9(6).
           03  FILLER            PIC X(2).
           03  D3-USERID         PIC X(10).
           03  FILLER            PIC X(2).
           03  D3-PURGED         PIC ZZ,ZZZ,ZZZ,ZZ9.


       01  DETAIL-LINE4 REDEFINES DETAIL-LINE.
           03  D4-ID             PIC X(6).
           03  FILLER            PIC X(13).
           03  D4-BEG-DATE       PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D4-END-DATE       PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D4-DATE           PIC 99/99/99.
           03  FILLER            PIC X(2).
           03  D4-SEQ            PIC 9(4).
           03  FILLER            PIC X(2).
           03  D4-TIME           PIC 9(6).
           03  FILLER            PIC X(12).


       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03 FILLER             PIC X(35).
           03 RANGE-SELECTION.
              04  R-NUM          PIC 9(12).

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLTAGS_DEF.CPY".
       COPY "LIBGL/GLTAGS_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/GLTAGS_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GLLOGS2-FILE.
           PERFORM CLOSE-GLLOGS1-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************************
       MAIN-MODULE SECTION.
      ***********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLTAGS" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF DATE-SEQ = 1
              PERFORM DATE-SEQ1.
           
           IF DATE-SEQ = 2
              PERFORM DATE-SEQ2.
               
           GO TO END-PROGRAM.

      ********************************************
       DATE-SEQ1 SECTION.
      ********************************************
           MOVE TAG-ID   TO GLLOGS-ID
                            QGLLOGS1-WBEG-ID
                            QGLLOGS1-WEND-ID.
           MOVE BEG-DATE TO GLLOGS-SYSTEM-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGLLOGS1-WBEG-SYSTEM-DATE.
           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGLLOGS1-WEND-SYSTEM-DATE.
           MOVE ZEROS    TO GLLOGS-SEQNO
                            QGLLOGS1-WBEG-SEQNO.
           MOVE ALL "9"  TO QGLLOGS1-WEND-SEQNO.

           PERFORM START-GLLOGS1-FILE.
       NEXT-GLLOGS-KEY1.
           PERFORM READ-GLLOGS1-FILE-NEXT.
           IF IO-BAD
              PERFORM PRINT-RANGE-LINES
              GO TO END-PROGRAM.

           IF GLLOGS-SYSTEM-DATE > END-DATE
              GO TO END-PROG.      

           IF GLLOGS-ID NOT = TAG-ID
              GO TO NEXT-GLLOGS-KEY1.

           IF (GLLOGS-ID = "ENDMTH")
               MOVE GLLOGS-ID          TO D1-ID       
               MOVE GLLOGS-BEG-DATE    TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY        TO D1-BEG-DATE1
               MOVE "-"                TO D1-DASH
               MOVE GLLOGS-END-DATE    TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY        TO D1-END-DATE1
               MOVE GLLOGS-SYSTEM-DATE TO DATE-YYYYMMDD 
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
               MOVE DATE-MMDDYY        TO D1-SYSTEM-DATE 
               MOVE GLLOGS-SEQNO       TO D1-SEQ     

               MOVE GLLOGS-BEG-DATE TO DATE-YYYYMMDD 
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
               MOVE DATE-MMDDYY     TO D1-BEG-DATE 

               MOVE GLLOGS-END-DATE TO DATE-YYYYMMDD 
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
               MOVE DATE-MMDDYY     TO D1-END-DATE 

               MOVE GLLOGS-TIME     TO D1-TIME 
               MOVE GLLOGS-USERID   TO D1-USERID 
               MOVE GLLOGS-RECS-PURGED TO D1-PURGED  
               MOVE 1 TO LN-FEED 
               PERFORM WRITE-DETAIL-LINE 
               GO TO DATE-SEQ1-END.

           IF (GLLOGS-ID = "ENDYER")
               MOVE GLLOGS-ID           TO D2-ID       
               MOVE GLLOGS-END-DATE     TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY         TO D2-END-DATE1
               MOVE "-"                 TO D2-DASH
               MOVE GLLOGS-BRNO-RANGE   TO D2-BRNO-RANGE
               MOVE GLLOGS-SYSTEM-DATE  TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY         TO D2-DATE
               MOVE GLLOGS-SEQNO        TO D2-SEQ
               MOVE GLLOGS-BEG-DATE     TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY         TO D2-BEG-DATE
               MOVE GLLOGS-END-DATE     TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY         TO D2-END-DATE
               MOVE GLLOGS-TIME         TO D2-TIME
               MOVE GLLOGS-USERID       TO D2-USERID 
               PERFORM WRITE-DETAIL-LINE
               GO TO DATE-SEQ1-END.  

       DATE-SEQ1-END.
           GO TO NEXT-GLLOGS-KEY1.
       EXIT-DATE-SEQ1.
           EXIT.

      ********************************************
       DATE-SEQ2 SECTION.
      ********************************************

           MOVE TAG-ID   TO GLLOGS-ID 
                            QGLLOGS2-WBEG-ID
                            QGLLOGS2-WEND-ID.
           MOVE BEG-DATE TO GLLOGS-BEG-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS2-WBEG-BEG-DATE.

           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS2-WEND-BEG-DATE.

           MOVE ZEROS    TO GLLOGS-END-DATE   SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS2-WBEG-END-DATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGLLOGS2-WEND-END-DATE.

           PERFORM START-GLLOGS2-FILE.

      *    IF TAG-ID = "ENDYER"
      *       MOVE TAG-ID TO GLLOGS-ID
      *       MOVE ZEROS TO GLLOGS-BEG-DATE
      *       MOVE END-DATE TO GLLOGS-END-DATE
      *       PERFORM START-GLLOGS2-FILE.

       NEXT-GLLOGS-KEY2.
           PERFORM READ-GLLOGS2-FILE-NEXT.
           IF IO-BAD
              PERFORM PRINT-RANGE-LINES
              GO TO END-PROGRAM.

           IF GLLOGS-END-DATE  > END-DATE
              GO TO END-PROG.      

           IF GLLOGS-ID NOT = TAG-ID
              GO TO NEXT-GLLOGS-KEY2.

           IF GLLOGS-ID = "ENDMTH"
              MOVE GLLOGS-ID          TO D3-ID       
              MOVE GLLOGS-BEG-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY        TO D3-BEG-DATE1
              MOVE "-"                TO D3-DASH

              MOVE GLLOGS-END-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY        TO D3-END-DATE1

              MOVE GLLOGS-BEG-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY     TO D3-BEG-DATE 
              MOVE GLLOGS-END-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY     TO D3-END-DATE 
              MOVE GLLOGS-SYSTEM-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY        TO D3-SYSTEM-DATE 
              MOVE GLLOGS-SEQNO       TO D3-SEQ     
              MOVE GLLOGS-TIME     TO D3-TIME 
              MOVE GLLOGS-USERID   TO D3-USERID 
              MOVE GLLOGS-RECS-PURGED TO D3-PURGED  
              MOVE 1 TO LN-FEED 
              PERFORM WRITE-DETAIL-LINE 
              GO TO DATE-SEQ2-END.    

           IF (GLLOGS-ID = "ENDYER")
               MOVE GLLOGS-ID           TO D4-ID       
               MOVE GLLOGS-END-DATE     TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY         TO D2-END-DATE1
               MOVE "-"                 TO D2-DASH
               MOVE GLLOGS-BRNO-RANGE   TO D2-BRNO-RANGE
               MOVE GLLOGS-BEG-DATE     TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY         TO D4-BEG-DATE
               MOVE GLLOGS-END-DATE     TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY         TO D4-END-DATE 
               MOVE GLLOGS-SYSTEM-DATE  TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY         TO D4-DATE
               MOVE GLLOGS-SEQNO        TO D4-SEQ
               MOVE GLLOGS-TIME         TO D4-TIME
               MOVE GLLOGS-USERID       TO D2-USERID 
               PERFORM WRITE-DETAIL-LINE
               GO TO DATE-SEQ2-END.  
       DATE-SEQ2-END.
           GO TO NEXT-GLLOGS-KEY2.
       EXIT-DATE-SEQ1.
           EXIT.

      *************************************
       INITIALIZATION SECTION.
      *************************************
           MOVE GLTAGS-QUEUE-POS TO Q-POS.
           MOVE GLTAGS-COPIES-POS TO C-POS.
           MOVE GLTAGS-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
      *    MOVE EXT-CONAME TO HD-NAME.
      *    MOVE EXT-CONAME-SYSDATE TO HD-DATE.
      *    MOVE EXT-CONAME-USER-ID TO HD-USERID.

           IF EXT-ROUTE-BUF = "00"
               MOVE "GL LOG LISTING IN PROGRESS ....." TO WR-BUF
               MOVE MESS-LIST TO WR-LIST
               PERFORM CALL-WRFORM.

           PERFORM OPEN-GLLOGS1-FILE.
           PERFORM OPEN-PR-FILE.
           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.
       
      ****************************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.
           
      ****************************************************
       ENTER-PARAMETERS SECTION.
      ***************************************************

           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE-SEQ.

       ENTER-TAG-ID.
           MOVE "E  A060TAG." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY
               GO TO ENTER-TAG-ID.
           MOVE VDUBUF TO TAG-ID.
           IF NOT(TAG-ID = "ENDMTH" OR "ENDYER")
               MOVE "ENTER ENDMTH OR ENDYER"  TO MESS
               PERFORM SEND-MESS 
               GO TO ENTER-TAG-ID.
                
       ENTER-DATE-SEQ.    
           MOVE "ENNN010SEQ." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO ENTER-TAG-ID.
           MOVE VDUNUM0 TO DATE-SEQ.
           IF DATE-SEQ < 1 OR > 2
              GO TO ENTER-DATE-SEQ.   

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

       ENTER-BEG-DATE.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-DATE-SEQ.
           IF F3-KEY GO TO ALL-DATES.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENTER-BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       ENTER-END-DATE. 
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENTER-END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           GO TO EXIT-PARM.
       ALL-DATES.
           MOVE "S  8080DATE1."   TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.
       
      *****************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************
           MOVE "S  A060TAG." TO VDU.
           MOVE TAG-ID        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN010SEQ." TO VDU.
           MOVE DATE-SEQ      TO VDUNUM0
           PERFORM SCREENIO.

      *******************************
       WRITE-DETAIL-LINE SECTION.
      *******************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN 61
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE  TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       HEAD-PAGE SECTION.
      ****************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.
           MOVE EXT-CONAME-CONAME TO HD-NAME.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF TAG-ID = "ENDMTH" AND DATE-SEQ = 1
              MOVE HEAD3 TO PR-REC
           ELSE
           IF TAG-ID = "ENDMTH" AND DATE-SEQ = 2
              MOVE HEAD4 TO PR-REC 
           ELSE
           IF TAG-ID = "ENDYER" AND DATE-SEQ = 1
              MOVE HEAD5   TO PR-REC 
           ELSE
           IF TAG-ID = "ENDYER" AND DATE-SEQ = 2
              MOVE HEAD6   TO PR-REC.
           
           PERFORM WRITE-PR-REC.
       END-HEAD-PAGE.
           MOVE 7 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO GLTAGS-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLTAGS-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GLTAGS-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GLTAGS-FORM-FIELDS TO WR-BUF.
           MOVE GLTAGS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE GLTAGS-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLTAGS-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLTAGS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
   
      **********************************************************
       MISC SECTION.
      **********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       PRINT-RANGE-LINES.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           MOVE "DATE SEQ    " TO RANGE-LINE.
           MOVE DATE-SEQ       TO R-NUM.
           MOVE "BEG DATE   : " TO RANGE-LINE.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END DATE     : " TO RANGE-LINE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

      **********************************************************
       IO-ROUTINES SECTION.
      **********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGLLOGSGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGL/GLGLLOGS1IN.CPY".
       COPY "LIBGL/GLGLLOGS2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **********************************************************
       END-PROGRAM SECTION.
      **********************************************************
       END-PROG.  
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLTAGS-SCREEN.
           DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
