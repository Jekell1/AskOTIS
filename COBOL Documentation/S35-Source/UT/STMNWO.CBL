       IDENTIFICATION DIVISION.
       PROGRAM-ID.  STMNWO.
      *****************************************************
      *
      *  DESCRIPTION:   HEADLESS STMAIN LOL
      *
      * REV:
      *  20230323 BAH NEW FOR JEFF
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/STMNWO S35 03/31/23-09:24:36 >".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".


       01  ERR-FIELD       PIC X(20) VALUE SPACES.

       01  ERRCD           PIC X VALUE " ".
       01  FIRST-SCAN      PIC 9       VALUE 0.
       01  CHAN            PIC X(4)    VALUE "CHAN".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9       VALUE 0.
       01  HOLD-ST1-KEY.
           05  HOLD-ST-CODE            PIC X(02).
       01  BAD-DATA-FLAG   PIC X.
       COPY "LIBUT/WINOTISCORE.CPY".

      ******************************************
      ***** THERE ARE 3 POTENTIAL MESSAGES  ****               
      ***** THAT CAN BE SENT:               ****               
      *****   1) STMAIN-OUT-MESSAGE         ****               
      *****       (PCRQ-COMMAND = "R")      ****               
      *****   2) OTCS-OK                    ****               
      *****     (PCRQ-COMMAND = "W" OR "D") ****               
      *****   2) OTCS-ERROR                 ****               
      ******************************************
        01  STMAIN-OUT-MESSAGE.
           03  STOM-CODE                PIC X(02).
           03  STOM-DESC                PIC X(16).
           03  STOM-REINSTATE-WINDOW    PIC X(01).
           03  STOM-REPO-RENOTICE       PIC X(01).
           03  STOM-TITLE-RELEASE-TYPE  PIC X(01).
           03  STOM-AUCTION-DATE-REQ    PIC X(01).
           03  STOM-GEOGRAPHIC-CODE     PIC 9(04).
           03  STOM-MAX-LOAN-AVAIL      PIC 9(6)V99.
           03  STOM-MAX-TAX-LOAN-AMT    PIC S9(7)V99.
           03  STOM-NSF-FEE-AMT         PIC S9(5)V99.

        01  PC-REQUEST.
           03  PCRQ-COMMAND                 PIC X.
                88 PCRQ-READ                 VALUE "R".
                88 PCRQ-WRITE                VALUE "W".
                88 PCRQ-DELETE               VALUE "D".                 
           03  PCRQ-REC.
               05  PCRQ-CODE                PIC X(02).
               05  PCRQ-DESC                PIC X(16).
               05  PCRQ-REINSTATE-WINDOW    PIC X(01).
               05  PCRQ-REPO-RENOTICE       PIC X(01).
               05  PCRQ-TITLE-RELEASE-TYPE  PIC X(01).
               05  PCRQ-AUCTION-DATE-REQ    PIC X(01).
               05  PCRQ-GEOGRAPHIC-CODE     PIC 9(04).
               05  PCRQ-MAX-LOAN-AVAIL      PIC 9(6)V99.
               05  PCRQ-MAX-TAX-LOAN-AMT    PIC S9(7)V99.
               05  PCRQ-NSF-FEE-AMT         PIC S9(5)V99.

      *=================================================================
      *   STCH-REC FROM 'REPLACING LEADING' ON ST-REC. (CHANGED-REC)
      *   STOR-REC FROM 'REPLACING LEADING' ON ST-REC. (ORIGINAL-REC)
      *=================================================================
       COPY "LIBLP/LP01ST.CPY" REPLACING LEADING "ST" BY "STOR".
       COPY "LIBLP/LP01ST.CPY" REPLACING LEADING "ST" BY "STCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(14).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       LINKAGE SECTION.
       01  COMMAND-ID              PIC 9(4).
       01  MSG-BUFFER-LEN          PIC 9(4).
       01  MSG-BUFFER.          
           03 MSG-BUFFER-VALUE    PIC X(WO-BFR-MAX).
           03 MSG-BUFFER-ARRAY REDEFINES 
                 MSG-BUFFER-VALUE
                 OCCURS WO-BFR-MAX TIMES  PIC X. 


       PROCEDURE DIVISION USING
            COMMAND-ID
            MSG-BUFFER-LEN
            MSG-BUFFER.

      ****************************************
       MAIN-PROGRAM SECTION.
      ****************************************
      D    STOP MESS.
           PERFORM SQL-CONNECT.

      * XXX USER SECURITY ???

      *COPY "LIBGB/CHKSEC.CPY".

       MAIN-MODULE.
           MOVE MSG-BUFFER TO PC-REQUEST.
           IF PCRQ-COMMAND <> "R" AND 
              PCRQ-COMMAND <> "W" AND
              PCRQ-COMMAND <> "D"
                 MOVE "INVALID OPERATION" TO WO-ERR-DESC
                 PERFORM WINOTIS-SEND-ERROR
                 GO TO STOP-RUN.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           PERFORM OPEN-ST1-FILE.

           IF PCRQ-READ 
              MOVE PCRQ-CODE TO ST-CODE
              PERFORM READ-ST1-FILE
              IF ( IO-BAD )
                 MOVE "RECORD NOT FOUND" TO WO-ERR-DESC
                 PERFORM WINOTIS-SEND-ERROR
                 GO TO STOP-RUN
              ELSE
                 MOVE ST-CODE                TO STOM-CODE
                 MOVE ST-DESC                TO STOM-DESC              
                 MOVE ST-REINSTATE-WINDOW    TO STOM-REINSTATE-WINDOW   
                 MOVE ST-REPO-RENOTICE       TO STOM-REPO-RENOTICE      
                 MOVE ST-TITLE-RELEASE-TYPE  TO STOM-TITLE-RELEASE-TYPE 
                 MOVE ST-AUCTION-DATE-REQ    TO STOM-AUCTION-DATE-REQ   
                 MOVE ST-GEOGRAPHIC-CODE     TO STOM-GEOGRAPHIC-CODE    
                 MOVE ST-MAX-LOAN-AVAIL      TO STOM-MAX-LOAN-AVAIL     
                 MOVE ST-MAX-TAX-LOAN-AMT    TO STOM-MAX-TAX-LOAN-AMT   
                 MOVE ST-NSF-FEE-AMT         TO STOM-NSF-FEE-AMT 
                 MOVE OTCS-STMAIN-DATA TO WOS-MSG-ID
  		         MOVE STMAIN-OUT-MESSAGE TO  WOS-BUFFER-VALUE
  		         MOVE 52 TO WOS-BUFFER-LEN
		         PERFORM WINOTIS-SEND-MESSAGE
                 GO TO STOP-RUN.


           IF PCRQ-WRITE 
              PERFORM WRITE-MODULE
              GO TO STOP-RUN.

           IF PCRQ-DELETE 
              PERFORM DELETE-MODULE
              GO TO STOP-RUN.

           GO TO STOP-RUN.


      ***********************************
       VERIFY-DATA SECTION.
      ***********************************

           MOVE SPACES TO BAD-DATA-FLAG.

           MOVE PCRQ-DESC TO ST-DESC.
           
           IF PCRQ-REINSTATE-WINDOW = "Y" OR "N" OR " "
              MOVE PCRQ-REINSTATE-WINDOW TO ST-REINSTATE-WINDOW
           ELSE
              MOVE "REINSTATE-WINDOW" TO ERR-FIELD
              PERFORM BAD-DATA
              GO TO EXIT-VERIFY-DATA.

           IF PCRQ-REPO-RENOTICE = "Y" OR "N" OR " "
              MOVE PCRQ-REPO-RENOTICE TO ST-REPO-RENOTICE
           ELSE
              MOVE "REINSTATE-WINDOW" TO ERR-FIELD
              PERFORM BAD-DATA
              GO TO EXIT-VERIFY-DATA.

           MOVE PCRQ-TITLE-RELEASE-TYPE TO ST-TITLE-RELEASE-TYPE.

           IF PCRQ-AUCTION-DATE-REQ = "Y" OR "N"
              MOVE PCRQ-AUCTION-DATE-REQ TO ST-AUCTION-DATE-REQ
           ELSE
              MOVE "AUCTION-DATE-REQ" TO ERR-FIELD
              PERFORM BAD-DATA
              GO TO EXIT-VERIFY-DATA.

      * XXX IF NO EDITS, DO WE KNOW IT WILL NUMERIC, DECIMAL, ETC?
      *    MOVE VDUNUM0 TO ST-GEOGRAPHIC-CODE.

           IF PCRQ-GEOGRAPHIC-CODE NOT NUMERIC
              MOVE "GEOGRAPHIC-CODE" TO ERR-FIELD
              PERFORM BAD-DATA
              GO TO EXIT-VERIFY-DATA
           ELSE
              MOVE PCRQ-GEOGRAPHIC-CODE TO ST-GEOGRAPHIC-CODE.
             
           IF PCRQ-MAX-LOAN-AVAIL NOT NUMERIC
              MOVE "MAX-LOAN-AVAIL" TO ERR-FIELD
              PERFORM BAD-DATA
              GO TO EXIT-VERIFY-DATA
           ELSE
              MOVE PCRQ-MAX-LOAN-AVAIL TO ST-MAX-LOAN-AVAIL.

           IF PCRQ-MAX-TAX-LOAN-AMT NOT NUMERIC
              MOVE "MAX-TAX-LOAN-AMT" TO ERR-FIELD
              PERFORM BAD-DATA
              GO TO EXIT-VERIFY-DATA
           ELSE
              MOVE PCRQ-MAX-TAX-LOAN-AMT TO ST-MAX-TAX-LOAN-AMT.

           IF PCRQ-NSF-FEE-AMT NOT NUMERIC
              MOVE "NSF-FEE-AMT" TO ERR-FIELD
              PERFORM BAD-DATA
              GO TO EXIT-VERIFY-DATA
           ELSE
              MOVE PCRQ-NSF-FEE-AMT TO ST-NSF-FEE-AMT.

       EXIT-VERIFY-DATA.
           EXIT.

      ***************************************
       WRITE-MODULE SECTION.
      ***************************************

           MOVE PCRQ-CODE TO ST-CODE.
           PERFORM READ-ST1-FILE.
           IF IO-FG NOT = 0
              GO TO WRITE-RECORD.

      * REWRITE

      * SAVE ORIGINAL
           MOVE ST-REC TO STOR-REC.

      * MOVES PASSED VALUES TO THE ST-REC
           PERFORM VERIFY-DATA.

           IF BAD-DATA-FLAG = "Y"
              GO TO EXIT-WRITE-MODULE.

      * DOES ORIGINAL ST-REC = NEW RECORD ? IF SO, EXIT WITHOUT SAVING
           IF STOR-REC = ST-REC
              GO TO EXIT-WRITE-MODULE.

           PERFORM REWRITE-ST1-FILE.
           IF ( IO-BAD )
              MOVE "Y" TO BAD-DATA-FLAG.

           PERFORM LOG-MAINTENANCE-CHGS.

           IF BAD-DATA-FLAG = "Y"
              MOVE "UNABLE TO REWRITE STMAIN RECORD" TO WO-ERR-DESC
              PERFORM WINOTIS-SEND-ERROR
           ELSE
              MOVE OTCS-OK TO WOS-MSG-ID
              MOVE 0 TO WOS-BUFFER-LEN
              PERFORM WINOTIS-SEND-MESSAGE.

           GO TO EXIT-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM CLEAR-ST-FILE.
           PERFORM VERIFY-DATA.
           IF BAD-DATA-FLAG = "Y"
              GO TO EXIT-WRITE-MODULE.

           PERFORM WRITE-ST1-FILE.
           IF ( IO-BAD )
              MOVE "Y" TO BAD-DATA-FLAG.

           IF BAD-DATA-FLAG = "Y"
              MOVE "UNABLE TO WRITE STMAIN RECORD" TO WO-ERR-DESC
              PERFORM WINOTIS-SEND-ERROR
           ELSE
              MOVE OTCS-OK TO WOS-MSG-ID
              MOVE 0 TO WOS-BUFFER-LEN
              PERFORM WINOTIS-SEND-MESSAGE.

       EXIT-WRITE-MODULE.
           EXIT.

      *******************************************
       DELETE-MODULE SECTION.
      *******************************************

           MOVE PCRQ-CODE TO ST-CODE.
           PERFORM READ-ST1-FILE.
           IF ( IO-BAD )
              MOVE "DELETE, RECORD NOT FOUND" TO WO-ERR-DESC
              PERFORM WINOTIS-SEND-ERROR
              GO TO EXIT-DELETE-MODULE.
           
          
           PERFORM DELETE-ST1-FILE.
           MOVE OTCS-OK TO WOS-MSG-ID.
           MOVE 0 TO WOS-BUFFER-LEN.
           PERFORM WINOTIS-SEND-MESSAGE.

      * XXXX
      * MAKE ENTRY IN ERRLOG REGARDING DELETE
      *    MOVE "C" TO ERRLOGW-ACTION.
      *    MOVE " " TO ERRLOGW-KEY.
      *    STRING ST1-KEY, " ", "DELETED" DELIMITED BY SIZE
      *                             INTO ERRLOGW-KEY.
      *    MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
      *    MOVE "CL/ERRLOG" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
      *    CANCEL FORM-PROGX.

       EXIT-DELETE-MODULE.
           EXIT.

      ***************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE " TO "                     TO LMC-TO-X.

           IF STOR-DESC NOT = STCH-DESC
             MOVE "01 "                   TO LMC-FIELD
             MOVE "STATE NAME    "        TO LMC-DESC
             MOVE STOR-DESC               TO LMC-ORG-TEXT
             MOVE STCH-DESC               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-REINSTATE-WINDOW NOT = STCH-REINSTATE-WINDOW
             MOVE "02 "                   TO LMC-FIELD
             MOVE "REINSTATE WIND"        TO LMC-DESC
             MOVE STOR-REINSTATE-WINDOW   TO LMC-ORG-TEXT
             MOVE STCH-REINSTATE-WINDOW   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-REPO-RENOTICE NOT = STCH-REPO-RENOTICE
             MOVE "03 "                   TO LMC-FIELD
             MOVE "REPO RENOTICE "        TO LMC-DESC
             MOVE STOR-REPO-RENOTICE      TO LMC-ORG-TEXT
             MOVE STCH-REPO-RENOTICE      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-TITLE-RELEASE-TYPE NOT = STCH-TITLE-RELEASE-TYPE
             MOVE "04 "                   TO LMC-FIELD
             MOVE "TITLE REL TYPE"        TO LMC-DESC
             MOVE STOR-TITLE-RELEASE-TYPE TO LMC-ORG-TEXT
             MOVE STCH-TITLE-RELEASE-TYPE TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-AUCTION-DATE-REQ NOT = STCH-AUCTION-DATE-REQ
             MOVE "05 "                   TO LMC-FIELD
             MOVE "AUC DATE REQ  "        TO LMC-DESC
             MOVE STOR-AUCTION-DATE-REQ   TO LMC-ORG-TEXT
             MOVE STCH-AUCTION-DATE-REQ   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-GEOGRAPHIC-CODE NOT = STCH-GEOGRAPHIC-CODE
             MOVE "06 "                   TO LMC-FIELD
             MOVE "GEOGRAPHIC CD "        TO LMC-DESC
             MOVE STOR-GEOGRAPHIC-CODE    TO LMC-ORG-TEXT
             MOVE STCH-GEOGRAPHIC-CODE    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-MAX-LOAN-AVAIL NOT = STCH-MAX-LOAN-AVAIL
             MOVE "07 "                   TO LMC-FIELD
             MOVE "MAX LOAN AVAIL"        TO LMC-DESC
             MOVE STOR-MAX-LOAN-AVAIL     TO LMC-ORG-AMT
             MOVE STCH-MAX-LOAN-AVAIL     TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-MAX-TAX-LOAN-AMT NOT = STCH-MAX-TAX-LOAN-AMT
             MOVE "08 "                   TO LMC-FIELD
             MOVE "MAX LOAN AMT  "        TO LMC-DESC
             MOVE STOR-MAX-TAX-LOAN-AMT     TO LMC-ORG-AMT
             MOVE STCH-MAX-TAX-LOAN-AMT     TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.
           IF STOR-NSF-FEE-AMT NOT = STCH-NSF-FEE-AMT
             MOVE "09 "                   TO LMC-FIELD
             MOVE "MAX LOAN AMT  "        TO LMC-DESC
             MOVE STOR-NSF-FEE-AMT     TO LMC-ORG-AMT
             MOVE STCH-NSF-FEE-AMT     TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

      ***************************************
       LOG-FIELD-CHANGE SECTION.
      ***************************************

      * XXX
      *    MOVE "F"           TO ERRLOGW-ACTION.
      *    MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
      *    MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

      *    MOVE "CL/ERRLOG" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
      *    CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

       COPY "LIBLP/LPSTCL.CPY".
       COPY "LIBGB/DATER.CPY".

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBUT/WOSRMSG.CPY".
       BAD-DATA.
           STRING "BAD/INVALID VALUE: " 
               DELIMITED BY SIZE
               ERR-FIELD DELIMITED BY SPACE
               INTO WO-ERR-DESC.
           PERFORM WINOTIS-SEND-ERROR.
           MOVE "Y" TO BAD-DATA-FLAG.

      ******************************************
      ***** SENDING DATA ("R" REQUEST)      ****               
      ******************************************
       SEND-STMAIN-DATA.
           MOVE OTCS-STMAIN-DATA TO WOS-MSG-ID.
            MOVE STMAIN-OUT-MESSAGE TO  
               WOS-BUFFER-VALUE.
            MOVE 53 TO WOS-BUFFER-LEN.
            PERFORM WINOTIS-SEND-MESSAGE.
                                
      ******************************************
      ***** SENDING OK (SUCCESFUL WRITE OR  ****               
      *****     DELETE)                     ****               
      ******************************************
       SEND-SUCCESS.
           MOVE OTCS-OK TO WOS-MSG-ID.
           MOVE SPACES TO  WOS-BUFFER-VALUE.
           MOVE 0 TO WOS-BUFFER-LEN.
           PERFORM WINOTIS-SEND-MESSAGE.


      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
      *COPY "LIBGB/DECLARE_SQL.CPY".
      * COPYMEMBER: LIBGB/DECLARE.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/DECLARE
      * DESCRIPTION   : SQL DECLARE ROUTINES
      *                 NEEDED WHEN PROGRAM HAS NO VISION FILES AND
      *                 CANNOT USE STANDARD DECLARE COPY MEMBERS USED
      *                 IN PROCEDURE DIVISION DECLARATIVES.
      *
      *=================================================================
      * REV:
      * JKC 2021-0513 ***NEW***
      * JKC 2022-0222 IN SQL-IO-VALIDATION ROUTINE ADDED A CHECK TO SEE
      *               IF THE SQL CONNECTION (EXT-ACUSQL-CONNECT-STAT)
      *               IS OPEN (GOOD) THEN `PERFORM CLOSE-FILES`;
      *               NEED TO ONLY CLOSE FILES/TABLES WHEN THERE IS
      *               AN SQL CONNECTION OTHERWISE YOU CAN GET INTO
      *               AN INIFITE LOOP DUE TO THE SQL-DISCONNECT IN
      *               SQL-ERROR WHICH THEN YOU CANNOT CLOSE CURSORS
      *               IF THE CONNECTION HAS BEEN CLOSED.
      *
      * JKC 2022-0622 MOVED THIS FROM LIBIO TO LIBGB; LIBIO WAS REMOVED
      *
      ******************************************************************
       OPEN-IT.
           MOVE "OPEN ERROR" TO E-MSG.
           MOVE SPACES TO E-KEYX.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       READ-IT.
           MOVE "READ ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       WRITE-IT.
           MOVE "WRITE ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       REWRITE-IT.
           MOVE "REWRITE ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       DELETE-IT.
           MOVE "DELETE ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       START-IT.
           MOVE "START ERROR" TO E-MSG.
           IF IO-FG NOT = 8 MOVE 0 TO LOCK-STATUS-99-RETRIES.
           MOVE 0 TO IO-FG IO-STATUS.
       CLOSE-IT.
           MOVE "CLOSE ERROR" TO E-MSG.
      *FORM-IT.
      *    MOVE "FORM ERROR" TO E-MSG.
      *    MOVE STAT TO E-STATUS-ERROR.
      *    MOVE SPACES TO E-KEYX.
      *SUSPEND.
      **    CALL SUSP.
      *    CALL "C$SLEEP" USING 1.
       ALARM.
      **    DISPLAY OMITTED WITH BEEP.
       FILE-ERRORS.
      *    MOVE E-FILE TO E-FILEX ERRLOGW-FILE.
      *    MOVE FORM-PATHNAME TO E-PROG ERRLOGW-PROGX.
      *    MOVE E-MSG TO ERRLOGW-TYPE.
      *    MOVE E-CODE TO ERRLOGW-CODE.
      *    MOVE E-SUBCODE TO ERRLOGW-SUBCODE.
      *    MOVE E-KEYX TO ERRLOGW-KEY.
      *    MOVE "E" TO ERRLOGW-ACTION.
      *    MOVE FORM-PATHNAME TO FORM-PATH.
      *    MOVE "CL/ERRLOG" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA EXIT-PATHNAME.
      *    CANCEL FORM-PROGX.
      *    MOVE FORM-PATHNAME TO FORM-PATH.
      *    IF PIGGY-BACK-FLAG = "PIGGY"
      *       MOVE "SNDERR" TO PIGGY-BACK-FLAG
      *OLD    GO TO FILE-ERRORS-TERMINATE     *> OLD VISION
      *       PERFORM FILE-ERRORS-TERMINATE   *> NEW SQL
      *       GO TO EXIT-PROG.                *> NEW SQL
      *    PERFORM ALARM.
      *    IF E-MSG4 = "OPEN" OR "WRIT" OR "REWR"
      *       MOVE "**** STOP ****"       TO ERRMSG-HEAD
      *       STRING "                    CALL HOME "
      *              "OFFICE OR SUPPORT             "
      *              DELIMITED BY SIZE INTO ERRMSG-MSG1
      *       STRING "AN ERROR HAS OCCURRED         "
      *              "                DO NOT PROCEED"
      *              DELIMITED BY SIZE INTO ERRMSG-MSG2
      *       MOVE "PRESS ENTER TO CONTINUE" TO ERRMSG-MSG3
      *       PERFORM CALL-ERRMSG.
      *    MOVE SPACES TO MESS.
      * THIS MUST STAY LIKE THIS, E-MSG HAS "OPEN ERROR" OR WHATEVER
      * IN IT, IT MUST GET MOVED AS BELOW
      *    MOVE ERROR-MSG-HEAD TO ERRMSG-HEAD.
      *    MOVE ERROR-MSG-STAT TO ERRMSG-MSG1.
      *    MOVE ERROR-MSG-PATH TO ERRMSG-MSG2.
      *    MOVE SPACES         TO ERRMSG-MSG3.
      *    MOVE ERROR-MSG-KEY  TO ERRMSG-MSG3.
      *    PERFORM CALL-ERRMSG.

      *    MOVE SPACES TO MESS.
       FILE-ERRORS-TERMINATE.
      *    A30, STARRED OFF, 7/26/12, BLM:
      *    IF WINDOWS-OPEN > ZERO
      *       CALL DELWIN
      *       SUBTRACT 1 FROM WINDOWS-OPEN
      *       GO TO FILE-ERRORS-TERMINATE.
           MOVE "E" TO ERRCD.
      *    IF FILE-STAT = "98"
      *       IF (FORM-PATHNAME(16:4) = "LONP" OR "XONP")
      *          MOVE "Z" TO ERRCD.
           PERFORM CLOSE-FILES.
      *OLD GO TO EXIT-PROG.     *> OLD VISION
      *SEND-MESS.
      *    PERFORM ALARM.
      *    DISPLAY MESSAGE BOX MESS.
      *COPY "LIBGB/TMMSG.CPY".
      *COPY "LIBGB/ERRMSG.CPY".

      *=================================================================
       SQL-IO-VALIDATION.

           IF ( E-MSG4  = "OPEN" ) AND   *> GOOD
              ( SQLCODE = 1      )
              EXIT PARAGRAPH
           ELSE
           IF ( SQLCODE = 0      )       *> GOOD
              EXIT PARAGRAPH.

           MOVE 9 TO IO-FG.

           IF ( E-MSG4  = "READ" ) AND   *> EOF OR ROW NOT FOUND
              ( SQLCODE = 100    )
              EXIT PARAGRAPH.

           IF ( E-MSG4   = "WRIT"  ) AND   *> DUPLICATE ROW
              ( SQLSTATE = "23000" )       *> LIKE VISION "22,00"
              MOVE "22"    TO FILE-STAT
              MOVE "00"    TO SECONDARY-FILE-STAT
              MOVE "22,00" TO FILE-STAT-DISP
                              E-STATUS-ERROR
              EXIT PARAGRAPH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE E-FILE        TO E-FILEX ERRLOGW-FILE.
      *    MOVE FORM-PATHNAME TO E-PROG ERRLOGW-PROGX.
      *    MOVE E-MSG         TO ERRLOGW-TYPE.
      *    MOVE E-CODE        TO ERRLOGW-CODE.
      *    MOVE E-SUBCODE     TO ERRLOGW-SUBCODE.
      *    MOVE E-KEYX        TO ERRLOGW-KEY.
      *    MOVE "E"           TO ERRLOGW-ACTION.
      *    MOVE FORM-PATHNAME TO FORM-PATH.
      *    MOVE "CL/ERRSQL"   TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA
      *                          SQLCA EXIT-PATHNAME.
      *    CANCEL FORM-PROGX.
      *    MOVE FORM-PATHNAME TO FORM-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "E" TO ERRCD.
      *    IF FILE-STAT = "98"
      *       IF (FORM-PATHNAME(16:4) = "LONP" OR "XONP")
      *          MOVE "Z" TO ERRCD.

      *    PERFORM SQL-ERROR.      *> DEFINED IN LIBGB/CONNECT_SQL.CPY

      *-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
      * ONLY CLOSE FILES/TABLES WHEN THERE IS AN SQL CONNECTION
      * OTHERWISE YOU CAN GET INTO AN INIFITE LOOP DUE TO THE
      * SQL-DISCONNECT IN SQL-ERROR WHICH THEN YOU CANNOT CLOSE CURSORS
      * IF THE CONNECTION HAS BEEN CLOSED.
      * NOTE: THIS WORKS FOR TABLES; NOT SURE WHAT HAPPENS WITH VISION
      *       WORK FILES, IMPORT & EXPORT EXTRACTION FILES; BELIEVE
      *       THEY SHOULD WORK FINE IF GOING RIGHT BACK INTO THE
      *       PROGRAM (THEORY IS THE CLOSING OF A PROGRAM WILL CLOSE
      *       THE FILES).                           BAH & JKC 2022-0214
      *-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
           IF ( EXT-ACUSQL-CONNECT-STAT-GOOD )
              PERFORM CLOSE-FILES.  *> CLOSE VISION FILES

           GO TO EXIT-PROG.        *> FORCE EXIT OF PROGRAM

      *=================================================================
       CLOSE-FILES.
           MOVE "CLOSE ERROR" TO E-MSG.
           PERFORM CLOSE-ST1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPST1IN.CPY".
      *COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
       EXIT-PROG.
      *    MOVE EXIT-PATH TO FORM-NAM.
      *    MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
      *    DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
      *    DESTROY STMAIN-SCREEN.
      *    DESTROY STMAIN-WINDOW-HANDLE.

           PERFORM CLOSE-FILES.
           EXIT PROGRAM.
