       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LEXBNK.
       DATE-WRITTEN.    10/19/2015.
      **********************************************************************
      *  DESC: CREATE ASCII OUTPUT FILES FOR LEXIS/NEXIS TO
      *         ANALYZE CURRENT PORTFOLIO FOR BANKRUPTCY ACCOUNTS.
      *     3 SEPARATE FILES BASED UPON "TYPE" REQUESTED ON SCREEN INPUT
      *         ASCII, PIPE DELIMITED
      * WORLD STILL USES 4/27/2023
      *        
      * TYPE "R" CREATES /USR/EXTRACT/WABKMMYY.SPR (REFRESH OF DATA)
      * TYPE "K" CREATES /USR/EXTRACT/WABKMMYY.KNO (KNOWN BANKRUPTS)
      * TYPE "I" CREATES /USR/EXTRACT/WABKMMYY.IN# (LOANS MADE)
      * TYPE "I" CREATES /USR/EXTRACT/WABKMMYY.DT# (PAID OUTS)
      *
      *
      * REV:
      *  151019 BAH NEW WORLD PR# 1111
      *  151111 BAH INCLUDE BANKRUPTS ON "R" REFRESH AND DO NOT END THE
      *             RECORD WITH A PIPE "|", WORLD PR# 1117
      *  BAH 160128 REMOVED LTFILE LPFILE, PD0003
      *  160503 BAH SUPPRESS QUOTES AND PIPES FROM THE FILE, WORLD PR#1155
      *  170314 BAH ADD I,S,O CONVERTED ACCOUNTS, WORLD PR# 1233
      *  20170929 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  20181214 BAH GLOBAL LNFILE
      *  20190808 BAH USE ENTDATE IF CONVERTED INSTEAD OF LOANDATE, PR#1385
      *  BAH 20191002 REMOVED ACCESS-CALL LNFILE
      *  MB  20230727 PERFORMANCE TUNING - ADDED STORED PROCEDURE 
      *               USES BRANCHLIST PROCESSING TO RETRIEVE DATA FOR GROUPS
      *               OF BRANCHES INSTEAD OF SINGLE.
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      ***********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * OUTPUT FILES
           SELECT WK-FILE ASSIGN OUTPUT WK-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
           SELECT WK2-FILE ASSIGN OUTPUT WK2-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-BYTE                PIC X OCCURS 250 TIMES.

       FD  WK2-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-BYTE               PIC X OCCURS 250 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/LEXBNK.CBL S35 02/22/24-07:35:30 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".

       01  WK-PATH.
           03  WK-PATH-FIXED        PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER               PIC X(4)  VALUE "WABK".
           03  WK-MM                PIC 99.
           03  WK-YY                PIC 99.
           03  FILLER               PIC X     VALUE ".".
           03  WK-EXT               PIC X(3).
           03  WK-EXT-X REDEFINES WK-EXT.
               05  FILLER           PIC XX.
               05  WK-EXT-NO        PIC 9.

       01  WK2-PATH.
           03  WK2-PATH-FIXED       PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER               PIC X(4)  VALUE "WABK".
           03  WK2-MM               PIC 99.
           03  WK2-YY               PIC 99.
           03  FILLER               PIC X     VALUE ".".
           03  WK2-EXT              PIC X(3).
           03  WK2-EXT-X REDEFINES WK2-EXT.
               05  FILLER           PIC XX.
               05  WK2-EXT-NO       PIC 9.

       01  WS-WK-REC.
           03  WK-CUSTOMER-ACCOUNT.
               05  WK-LN-OWNBR      PIC 9(4).
               05  WK-LN-ACCTNO     PIC 9(6).
           03  WK-FNAME             PIC X(18).
           03  WK-MNAME             PIC X(15).
           03  WK-LNAME             PIC X(18).
           03  WK-SUFFIX            PIC X.
           03  WK-SSNO              PIC 9(9).
           03  WK-ADDRESS           PIC X(34).
           03  WK-CITY              PIC X(17).
           03  WK-STATE             PIC X(2).
           03  WK-ZIPCODE           PIC X(12).
           03  WK-CLIENT            PIC XX.
      * YYYYMMDD
           03  WK-AGREEMENT-DATE    PIC 9(8).
           03  WK-PRODUCT-CODE      PIC XX.


      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-ENT-TYPE        PIC X.
       01  PASS-CONV-FLAG       PIC X.
       01  PASS-BEG-LOANDATE    PIC X(10).
       01  PASS-END-LOANDATE    PIC X(10).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW     PIC X     VALUE "N".
       01  MISC-SW              PIC X     VALUE "N".
       01  PROC-BRANCH-NO       PIC 9(4)        VALUE 0.
       01  LOOP-PROC-SUB        PIC 9(4)        VALUE 0.

       01  PIPE-1               PIC X VALUE "|".
       01  CR-1                 PIC X VALUE X"0D".
 
       01  IN-NAME.
           03  IN-NAME-BYTE    PIC X OCCURS 16 TIMES.
       01  OUT-NAME.
           03  OUT-NAME-BYTE   PIC X OCCURS 16 TIMES.


       01  SUB                 PIC 9(2)   VALUE 0.
       01  IN-SUB              PIC 9(2)   VALUE 0.
       01  OUT-SUB             PIC 9(2)   VALUE 0.
       01  JR-COUNT            PIC 9      VALUE 0.
       01  SR-COUNT            PIC 9      VALUE 0.
       01  III-COUNT           PIC 9      VALUE 0.
       01  LN-FEED             PIC 99     VALUE 0.
 
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  ENT-TYPE         PIC X.
           03  BEG-BR           PIC 9(4).
           03  END-BR           PIC 9(4).
           03  BEG-DATE         PIC 9(8).
           03  END-DATE         PIC 9(8).
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  CONVERTED-FLAG   PIC X.

       01  VERSION-CONTROL      PIC X VALUE "A".
 
       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(2).
           03  DISPLAY-ACCT     PIC Z(6).
           03  FILLER           PIC X(26) VALUE SPACES.
 

       01  TOTAL-WK         PIC 9(9)      VALUE 0.
       01  TOTAL-WK2        PIC 9(9)      VALUE 0.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LEXBNK_DEF.CPY".
       COPY "LIBSP/LEXBNK_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LEXBNK_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           CLOSE
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

       MAIN-ROUTINE.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LEXBNK" TO VDU-FORM-NAME.

      D    STOP MESS.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      *    IF GROUP-FLAG = "N"
      *       MOVE BEG-BR TO BR-NO
      *       PERFORM START-BR-FILE
      *       GO TO NEXT-BRANCH-NO.
      *
      *    MOVE ENT-GROUP TO GR-ID.
      *    PERFORM SEARCH-GR-INIT.
      *
      *NEXT-GROUP-LOC.
      *    IF GROUP-FLAG NOT = "Y"
      *       GO TO NEXT-BRANCH-NO.
      *
      *    PERFORM SEARCH-GR-NEXT.
      *    IF WGR-SRESULT = 0
      *       GO TO END-ROUTINE.
      *
      *    MOVE WGR-SRESULT TO BR-NO.
      *    PERFORM READ-BR-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.
      *
      *    GO TO MOVE-BRANCH-PATH-INFO.
      *
      *NEXT-BRANCH-NO.
      *    PERFORM READ-BR-FILE-NEXT.
      *    IF IO-FG = 9 OR BR-NO > END-BR
      *       GO TO END-ROUTINE.
      *

           MOVE "B" TO LN-PATH-OVERRIDE
                       BW-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.

      * LOAD BRANCHES TO PROCESS USING STORED PROCEDURE SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT
      *   INTO BGLS-BRANCH-TBL
           IF GROUP-FLAG = "Y"
               MOVE ENT-GROUP TO BGLS-PASS-BEG-BRGRP
               MOVE SPACES    TO BGLS-PASS-END-BRGRP
           ELSE
               MOVE BEG-BR                    TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-BEG-BRGRP
               MOVE END-BR                    TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-END-BRGRP.

           PERFORM LOAD-BGLS-BRANCH-LIST.

      * SET BRANCHLIST-PGM-PROC-MAX TO NUMBER OF BRANCHES TO PROCESS PER STORED PROCEDURE CURSOR
      * IF DATE RANGE WAS ENTERED, PROCESS 500 BRANCHES PER SP CALL ELSE 100 BRANCHES.
           IF BEG-DATE = 19500101
              MOVE 100 TO BRANCHLIST-PGM-PROC-MAX
           ELSE
              MOVE 500 TO BRANCHLIST-PGM-PROC-MAX.

      * LOOP THRU BGLS BRANCH TABLE (LOADED BY LOAD-BGLS-BRANCH-LIST)
      *  BGLS-BRANCHES-RETR-CTR = TOTAL BRANCHES LOADED IN LOADED BY LOAD-BGLS-BRANCH-LIST
      *  BRANCHLIST-PGM-PROC-MAX - MAX BRANCHES TO PROCESS AT ONE TIME
       PROC-LOOP-INIT.
           INITIALIZE BRANCHLIST-TBL BRANCHLIST-SUB
                      BRANCHLIST-PROC-BRANCH-CTR 
                      PASS-BRANCHLIST. 

       PROC-LOOP.
           ADD 1 TO LOOP-PROC-SUB.

           IF LOOP-PROC-SUB > BGLS-BRANCHES-RETR-CTR
              GO TO PROC-LOOP-CONT.

           ADD 1 TO BRANCHLIST-PROC-BRANCH-CTR
                    BRANCHLIST-SUB.

           MOVE BGLS-BRANCH-BRNO(LOOP-PROC-SUB) TO 
                            BRANCHLIST-BRNO(BRANCHLIST-SUB).
           IF BRANCHLIST-SUB > 1
              MOVE ","   TO BRANCHLIST-COMMA(BRANCHLIST-SUB).

           IF BRANCHLIST-PROC-BRANCH-CTR < BRANCHLIST-PGM-PROC-MAX 
               GO TO PROC-LOOP.
 
       PROC-LOOP-CONT.
           IF BRANCHLIST-SUB = 0
              GO TO END-ROUTINE.

            MOVE BRANCHLIST-TBL TO PASS-BRANCHLIST.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
      *     MOVE BR-MACHINE       TO FILPATH-MACHINE.
      *     MOVE BR-DATA-PATH     TO FILPATH-DATA.

      *     MOVE "B" TO LN-PATH-OVERRIDE
      *                 BW-PATH-OVERRIDE.

      *     PERFORM OPEN-LN3-FILE.
      *     PERFORM OPEN-BW1-FILE.

           MOVE ENT-TYPE       TO PASS-ENT-TYPE.
           MOVE CONVERTED-FLAG TO PASS-CONV-FLAG.

           MOVE BEG-DATE       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-BEG-LOANDATE.

           MOVE END-DATE       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-END-LOANDATE

           PERFORM START-LN3-FILE.   *> SSP LN BW BR
 
           PERFORM PROCESS-LOANS.

           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.

           GO TO PROC-LOOP-INIT.

      *     GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.
 
      *************************************************
       PROCESS-LOANS SECTION.
      *************************************************
      *     MOVE BR-NO          TO LN-OWNBR QLN3-WBEG-OWNBR.
      *     MOVE 0              TO LN-CLASS LN-ACCTNO.
 
       PROCESS-NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.   *> EMBEDDED LN BW BR
           IF IO-BAD                   *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-PROCESS-LOANS.

           IF LN-OWNBR NOT = PROC-BRANCH-NO
              MOVE BR-MACHINE   TO FILPATH-MACHINE
              MOVE BR-DATA-PATH TO FILPATH-DATA
              PERFORM CLOSE-BW1-FILE
              PERFORM OPEN-BW1-FILE
              MOVE LN-OWNBR TO PROC-BRANCH-NO
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE LN-OWNBR    TO DISPLAY-BRNO
                 MOVE ZEROS       TO DISPLAY-ACCT
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST   TO WR-LIST
                 PERFORM CALL-WRFORM.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF LN-OWNBR > END-BR
              GO TO EXIT-PROCESS-LOANS.

      * TEST CONVERTED LOANS
      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF CONVERTED-FLAG = "S"
              IF LN-CONVERCD NOT = " "
                 GO TO PROCESS-NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF CONVERTED-FLAG = "O"
              IF LN-CONVERCD = " "
                 GO TO PROCESS-NEXT-LN.

      * DELETED LOAN
      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF LN-POCD-NONLOAN
              GO TO PROCESS-NEXT-LN.

      * SKIP TRANSFERRED ACCOUNTS,  THE OPEN NEW ONE WILL GET REPORTED
      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF LN-POCD = "BT"
              GO TO PROCESS-NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF LN-CONVERCD = "Y"
              MOVE LN-ENTDATE TO NUM-DATE
           ELSE
              MOVE LN-LOANDATE TO NUM-DATE.
           IF NUM-DATE < BEG-DATE OR NUM-DATE > END-DATE
              GO TO PROCESS-NEXT-LN.

      * TEST BASED ON TYPE ENTERED:

      * "R" - ALL OPEN ACCOUNTS (ACCOUNT WITH A BALANCE) INCLUDING
      *       P&L & BANKRUPT ACCOUNTS, SKIP P&L INACTIVE
      * "K" - ONLY OPEN BANKRUPTS
      * "I" - NOTHING TO TEST HERE, IN# IS FOR LOANS MADE AND
      *       DT# IS FOR PAID OUTS WITHIN DATE RANGE ENTERED

      * KNOWN BANKRUPTS
      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF ENT-TYPE = "K"
              IF LN-BNKRPTDATE = 0
                 GO TO PROCESS-NEXT-LN
              ELSE
                 IF LN-POFFDATE NOT = 0
                    GO TO PROCESS-NEXT-LN.

      * REFRESH OF DATA
      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF ENT-TYPE = "R"
              IF (LN-PLCD = "I") OR (LN-POFFDATE NOT = 0)
                 GO TO PROCESS-NEXT-LN.

      * BOTH "I" FILES WILL SKIP BANKRUPT
      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF ENT-TYPE = "I"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO PROCESS-NEXT-LN.

      * CREATE A RECORD FOR EACH BORROWER ON THE LOAN
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              PERFORM BUILD-WK-REC
           END-PERFORM.

           GO TO PROCESS-NEXT-LN.
 
       EXIT-PROCESS-LOANS.
           EXIT.


      *************************************
       BUILD-WK-REC SECTION.
      *************************************

      * THIS ROUTINE IS PERFORMED WITHIN A PERFORM VARYING
      * TO INCREMENT SUB FOR READING BORROWER

           MOVE SPACES TO WS-WK-REC
                          WK-REC
                          WK2-REC.

           IF LN-SSNO(SUB) = 0
              GO TO EXIT-BUILD-WK-REC.

      * BW DATA RETURNED IN STORED PROCEDURE FOR LN-SSNO(1)
      * BW-SSNO = 0 NOT FOUND IN STORED PROCEDURE JOIN
           IF SUB = 1
              IF BW-SSNO = 0 
                 MOVE SPACES    TO BW-REC
                 MOVE 999999999 TO BW-SSNO
                 MOVE "INVALID" TO BW-LNAME
              END-IF
           ELSE 
              MOVE LN-SSNO(SUB) TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-FG = 9 
                 MOVE SPACES    TO BW-REC
                 MOVE 999999999 TO BW-SSNO
                 MOVE "INVALID" TO BW-LNAME.


      * 4 DIGIT BRANCH + 6 DIGIT ACCOUNT NUMBER
           MOVE LN-OWNBR          TO WK-LN-OWNBR.
           MOVE LN-ACCTNO         TO WK-LN-ACCTNO.

      * BORROWER FIRST NAME IN ALL CAPS. EXCLUDE HYPHEN
           IF BW-MNAME NOT = SPACES
              MOVE BW-FNAME TO WK-FNAME
           ELSE 
              PERFORM FIND-FIRST-NAME
              MOVE OUT-NAME TO WK-FNAME
           END-IF.
           CALL "C$TOUPPER" USING WK-FNAME, VALUE 18.

      * ALL CAPS. 
           IF BW-MNAME NOT = SPACES
              MOVE BW-MNAME TO WK-MNAME
           ELSE 
              PERFORM FIND-MIDDLE-NAME
              MOVE OUT-NAME TO WK-MNAME
           END-IF.
           CALL "C$TOUPPER" USING WK-MNAME, VALUE 15.

      * ALL CAPS. INCLUDE ALL DATA IN THIS FIELD EXCEPT EXCLUDE HYPHENS
      * , APOSTROPHES, JR, SR, III
           MOVE BW-LNAME TO WK-LNAME.
           CALL "C$TOUPPER" USING WK-LNAME, VALUE 18.

           MOVE 0 TO JR-COUNT SR-COUNT III-COUNT.
           INSPECT WK-LNAME TALLYING JR-COUNT FOR ALL "JR ".
           INSPECT WK-LNAME TALLYING SR-COUNT FOR ALL "SR ".
           INSPECT WK-LNAME TALLYING III-COUNT FOR ALL "III ".
           INSPECT WK-LNAME REPLACING ALL "JR " BY "   ".
           INSPECT WK-LNAME REPLACING ALL "SR " BY "   ".
           INSPECT WK-LNAME REPLACING ALL "III " BY "    ".
           

      * IF JR, SR OF III EXIST IN LAST NAME FIELD, SEND J,S,1,2 OR 3.
      * (JR=J, SR-S, III=3)
           IF JR-COUNT NOT = 0
              MOVE "J" TO WK-SUFFIX
           ELSE
              IF SR-COUNT NOT = 0
                 MOVE "S" TO WK-SUFFIX
              ELSE
                 IF III-COUNT NOT = 0
                    MOVE "3" TO WK-SUFFIX.

      * BORROWER SS IN THIS FORMAT XXXXXXXXX (NO HYPHENS)
           MOVE BW-SSNO TO WK-SSNO.

      * ALL CAPS. BORROWER ADDRESS 1 + ADDRESS 2 FIELDS
           STRING BW-ADR1," ",BW-ADR2 DELIMITED BY "  "
               INTO WK-ADDRESS.
           CALL "C$TOUPPER" USING WK-ADDRESS, VALUE 34.

      * ALL CAPS. BORROWER CITY. EXCLUDE PUNCTUATION.
           MOVE BW-CITY TO WK-CITY.
           CALL "C$TOUPPER" USING WK-CITY, VALUE 17.

      * ALL CAPS BORROWER STATE ABBREVIATION
           MOVE BW-STATE TO WK-STATE.
           CALL "C$TOUPPER" USING WK-STATE, VALUE 2.
            

      * ZIP IN THE FOLLOWING FORMAT XXXXXXXXXX (IF NO +4, DO
      * NOT INPUT ZEROS, JUST LEAVE BLANK)
           MOVE BW-ZIP TO WK-ZIPCODE.


      * MACHINE (R1 OR R2)
           MOVE EXT-FILPATH-MACHINE TO WK-CLIENT.
           CALL "C$TOUPPER" USING WK-CLIENT, VALUE 2.

      * LOAN DATE (CCYYMMDD)
           MOVE LN-LOANDATE TO WK-AGREEMENT-DATE.

      * OPTIONAL
           MOVE SPACES TO WK-PRODUCT-CODE.

           INSPECT WS-WK-REC REPLACING ALL "-" BY " ".
           INSPECT WS-WK-REC REPLACING ALL "," BY " ".
           INSPECT WS-WK-REC REPLACING ALL "'" BY " ".
           INSPECT WS-WK-REC REPLACING ALL "|" BY " ".
           INSPECT WS-WK-REC REPLACING ALL """" BY " ".

           STRING WK-CUSTOMER-ACCOUNT, PIPE-1,
                  WK-FNAME           , PIPE-1,
                  WK-MNAME           , PIPE-1,
                  WK-LNAME           , PIPE-1,
                  WK-SUFFIX          , PIPE-1,
                  WK-SSNO            , PIPE-1,
                  WK-ADDRESS         , PIPE-1,
                  WK-CITY            , PIPE-1,
                  WK-STATE           , PIPE-1,
                  WK-ZIPCODE         , PIPE-1,
                  WK-CLIENT          , PIPE-1,
                  WK-AGREEMENT-DATE  , PIPE-1,
                  WK-PRODUCT-CODE    , CR-1,
             DELIMITED BY "  " INTO WK-REC.

      * TYPE "I"
      *   WK-REC = IN# FOR LOANS MADE
      *   WK2-REC = DT# FOR LOANS PAID OUT AND P&L INACTIVE

      * IF TYPE "I" AND ACCOUNT IS NOT OPEN, DO NOT CREATE WK-REC
           IF ENT-TYPE = "I"
              IF (LN-POFFDATE NOT = 0) OR (LN-PLCD = "I")
                 MOVE WK-REC TO WK2-REC
                 PERFORM WRITE-WK2-FILE
                 IF IO-FG NOT = 0
                    GO TO IO-ERROR
                 ELSE
                    ADD 1 TO TOTAL-WK2
                    GO TO EXIT-BUILD-WK-REC.
                 
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO TOTAL-WK.

       EXIT-BUILD-WK-REC.
           EXIT.

      ***************************
       FIND-FIRST-NAME SECTION.
      ***************************
      * USE BYTES BEFORE FIRST SPACE IN BW-FNAME 
           MOVE SPACES   TO OUT-NAME.
           MOVE BW-FNAME TO IN-NAME.

           PERFORM VARYING IN-SUB FROM 1 BY 1 
            UNTIL IN-NAME-BYTE(IN-SUB) = " " OR IN-SUB > 16
              MOVE IN-NAME-BYTE(IN-SUB) TO OUT-NAME-BYTE(IN-SUB)
           END-PERFORM.

      ***************************
       FIND-MIDDLE-NAME SECTION.
      ***************************
      * USE BYTES AFTER FIRST SPACE IN BW-FNAME 
           MOVE SPACES   TO OUT-NAME.
           MOVE BW-FNAME TO IN-NAME.
           MOVE "N"      TO MISC-SW.
           MOVE 0        TO OUT-SUB.

           PERFORM VARYING IN-SUB FROM 1 BY 1 UNTIL IN-SUB > 16
              IF MISC-SW = "Y"
                 ADD 1 TO OUT-SUB
                 MOVE IN-NAME-BYTE(IN-SUB) TO OUT-NAME-BYTE(OUT-SUB)
              ELSE
                 IF IN-NAME-BYTE(IN-SUB) = SPACE
                    MOVE "Y" TO MISC-SW
                 END-IF
              END-IF
           END-PERFORM.


      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE "FILE CREATE IN PROCESS" TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE 0 TO TOTAL-WK TOTAL-WK2.
           
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           CALL "C$TOLOWER" USING WK-PATH-FIXED, VALUE 13.
           CALL "C$TOLOWER" USING WK2-PATH-FIXED, VALUE 13.
           MOVE DATE-MMDDYY-MM TO WK-MM WK2-MM.
           MOVE DATE-MMDDYY-YY TO WK-YY WK2-YY.
           IF ENT-TYPE = "R"
              MOVE "SPR" TO WK-EXT
           ELSE
              IF ENT-TYPE = "K"
                 MOVE "KNO" TO WK-EXT
              ELSE
                 MOVE "IN" TO WK-EXT
                 MOVE "DT" TO WK2-EXT
                 MOVE EXT-FILPATH-MACHINE TO WK-EXT-NO WK2-EXT-NO
                 PERFORM OPEN-WK2-FILE-OUTPUT.

           PERFORM OPEN-WK-FILE-OUTPUT.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO ENTER-CONVERTED
              ELSE
                 GO TO BEGDATE-01.

       ENTER-TYPE.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-TYPE.
           MOVE VDUBUF TO ENT-TYPE.
           IF NOT (ENT-TYPE = "R" OR "K" OR "I")
              MOVE "ENTER: 'R', 'K', OR 'I' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TYPE.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO BEGDATE-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO BEGDATE-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-CONVERTED.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-CONVERTED.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              IF END-DATE < BEG-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO ENTER-CONVERTED.

      *----------------------------------------------------------
       ALL-DATE.
           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       ENTER-CONVERTED.
           MOVE "E  A010CONV." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-CONVERTED.
           MOVE VDUBUF TO CONVERTED-FLAG.
           IF NOT (CONVERTED-FLAG = "I" OR "S" OR "O")
              MOVE "ENTER: 'I', 'S', OR 'O' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CONVERTED.


       EXIT-PARM.
           EXIT.
 
      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A010TYPE." TO VDU.
           MOVE ENT-TYPE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010CONV." TO VDU.
           MOVE CONVERTED-FLAG TO VDUBUF.
           PERFORM SCREENIO.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO LEXBNK-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LEXBNK-SYSDATE.
           DISPLAY LEXBNK-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LEXBNK-FORM-FIELDS TO WR-BUF.
           MOVE LEXBNK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LEXBNK-HELP--FILE TO HELP-LINK-FILE.
           MOVE LEXBNK-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LEXBNK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GRBRLS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

      *COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.    *> SSP LN BW BR
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_LEXBNK_LNFILE_SELECT
                  (:PASS-BRANCHLIST,:PASS-ENT-TYPE,:PASS-CONV-FLAG,
                   :PASS-BEG-LOANDATE, :PASS-END-LOANDATE) 
                END-EXEC.

      *     SELECT BRFILE.BR_NO
      *            BRFILE.BR_DATA_PATH, 
      *            BRFILE.BR_MACHINE, 
      *            LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_CONVERCD,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_PLCD,
      *            CAST(LNFILE.LN_ENTDATE    AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LOANDATE   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_POFFDATE   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)),
      *            ISNULL(BWFILE.BW_OWNBR,0),
      *            ISNULL(BWFILE.BW_SSNO,0),
      *            ISNULL(BWFILE.BW_FNAME,''''),
      *            ISNULL(BWFILE.BW_MNAME,''''),
      *            ISNULL(BWFILE.BW_LNAME,''''), 
      *            ISNULL(BWFILE.BW_ADR1,''''),
      *            ISNULL(BWFILE.BW_ADR2,''''),
      *            ISNULL(BWFILE.BW_CITY,''''),
      *            ISNULL(BWFILE.BW_STATE,''''),
      *            ISNULL(BWFILE.BW_ZIP,'''')
      *    FROM #TEMP1_BRANCH_GROUP_TBL T1
      *    INNER JOIN DBO.BRFILE BRFILE
      *     ON BRFILE.BR_NO = T1.BRNO
      *    INNER JOIN DBO.LNFILE LNFILE
      *     ON LNFILE.LN_OWNBR = BRFILE.BR_NO
      *    LEFT OUTER JOIN DBO.BWFILE BWFILE
      *     ON BWFILE.BW_OWNBR = LNFILE.LN_OWNBR 
      *     AND BWFILE.BW_SSNO = LNFILE.LN_SSNO1
      *    WHERE LNFILE.LN_POCD NOT IN ('**', '*I', '*R', '*U', 'BT') 
      *    AND ( (LNFILE.LN_CONVERCD  = 'Y' AND LNFILE.LN_ENTDATE  BETWEEN @BEG_LNDATE AND @END_LNDATE)
      *     OR   (LNFILE.LN_CONVERCD != 'Y' AND LNFILE.LN_LOANDATE BETWEEN @BEG_LNDATE AND @END_LNDATE) )
      *  IF @ENT_TYPE = 'R'
      *    SELECT @SQL += ' AND (LNFILE.LN_PLCD != 'I' AND LNFILE.LN_POFFDATE = '1900-01-01' )  
      *  IF @ENT_TYPE = 'K'
      *    SELECT @SQL += ' AND (LNFILE.LN_BNKRPTDATE > '1900-01-01')  
      *  IF @ENT_TYPE = 'I'
      *    SELECT @SQL += ' AND (LNFILE.LN_BNKRPTDATE = '1900-01-01')  
      *  IF @CONV_FLAG = 'S'
      *    SELECT @SQL += ' AND (LNFILE.LN_CONVERCD = ' ')  
      *  IF @CONV_FLAG = 'O'
      *    SELECT @SQL += ' AND (LNFILE.LN_CONVERCD != ' ')  
      *  ORDER BY T1.SEQ, LNFILE.LN_CLASS, LNFILE.LN_ACCTNO

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.   *> EMBEDDED LN BW BR
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC QBW-REC QBR-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LN3_CURSOR INTO
                   :QBR-NO,
                   :QBR-DATA-PATH, 
                   :QBR-MACHINE, 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-CONVERCD,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-ENTDATE,
                   :QLN-LOANDATE,
                   :QLN-POFFDATE,
                   :QLN-BNKRPTDATE,
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-FNAME,
                   :QBW-MNAME,
                   :QBW-LNAME,
                   :QBW-ADR1,
                   :QBW-ADR2,
                   :QBW-CITY,
                   :QBW-STATE,
                   :QBW-ZIP
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.
              
      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN3-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       OPEN-WK2-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK2-PATH TO E-FILE.
           OPEN OUTPUT WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE-OUTPUT.
       WRITE-WK2-FILE.
           PERFORM WRITE-IT.
           MOVE WK2-PATH TO E-FILE.
           WRITE WK2-REC.
           IF IO-FG = 8
             GO TO WRITE-WK2-FILE.
       CLOSE-WK2-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK2-FILE.

       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE WK-PATH TO E-FILE.
           WRITE WK-REC.
           IF IO-FG = 8
             GO TO WRITE-WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ***********************************************
       END-PROGRAM SECTION.
      ***********************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LEXBNK-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
