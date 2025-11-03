      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHMEMO.
       DATE-WRITTEN.    12-14-1989.
      ******************************************************************
      *
      *  IDENTIFICATION:  CL/CHMEMO.C
      *
      *  DESCRIPTION   :  LOAN PROCESSING
      *                    WRITE A MEMO RECORD FOR THE CHANGE MADE TO
      *                    THE LOAN RECORD
      *
      *-----------------------------------------------------------------
      * REV:
      * GLF 1998.1215 MADE MEMO DATE 2000 COMPATIBLE
      * MJD 1999.0203 ADDED LOGIC FOR FROZEN ACCOUNT MEMO
      * CS  1999.0203 ADDED MEMO FOR CHANGE TO LN-CHARGE-INTEREST-FG,
      *                LN-CHARGE-LATECHG-FG, LN-CHARGE-MEMBERSHIP-FG
      * CS  1999.0215 ADDED MEMO FOR LN-INDEXADJUST-FACTOR(1) AND
      *                LN-INDEX-ADJUST-FACTOR(2)
      * CS  1999.0220 ADDED MEMO FOR REVOLVING INSURANCE CODE CHANGES
      * KEC 1999.0526 ADDED/REVISED MEMO ROUTINES FOR CREDIT BUREAU
      *                REPORTING.                       (TFC) PR#: 00023
      *  MG  990927 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLV  000218 ADDED MEMO FOR LN-WCBC-CREDITLIM, "W", WORLD PR# 225
      * MJD  000918 ADDED "Y" FOR CHANGES TO ALLOTCD/LBOX. (MFC PR#227)
      * MJD 001004 ADDED "R" FOR REVERSALS (WORLD PR#???)
      * BAH 020626 ADDED "P" FOR CHARGE OFF REASON CODE REGACC PR# 1775
      * CS  020906 ADDED "N" FOR LOAN-ORIGIN CHANGE   [MODERN PR# 1876]
      * BAH 050928 ADDED CH-TYPE "6" FROM CHTRWF FOR COMPLIANCE CODE
      *            LIBERAL PR# 2591
      * BLM 100225 ADDED CH-TYPE "O", FROM CHESCR, CHANGE ESCROW FLAG
      *            TO INACTIVE, REGWAN PR # 3506
      *  CS 110314 ADDED 'CEASE & DESIST' TO TYPE 'C' WHICH SHARES WITH
      *            CONFIDENTIAL   MODERN PR#3720
      *  CS 130425 ADDED CH-TYPE "2", TDR1 CHANGES REGACC PR#338
      * BAH 150818 ADDED CH-TYPE "1" FROM PL# 776, CREATE A MEMO
      *            WHEN GROSS INCOME CHANGES, WORLD PR# 1100
      * BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      * BAH 160415 CHANGED BANKRUPT MEMOS TO WORK WITH DIFFERENT FIELD
      *            LENGTHS, WAS ONLY WORKING WITH A DATE.  WORLD 1150
      * BAH 170323 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 170328 ADDED CH-TYPE = "3" CHANGE LOAN SOURCE CODES WORLD PR#1238
      * BAH 180328 ADDED CH-TYPE = "C" FOR DUAL LOAN OVERRIDE WORLD #1308
      * BAH 181127 GLOBAL LMFILE
      * BAH 2022.0114 ADDED CH-TYPE = "4" CHANGE COLLATERAL CODE #1536
      * BAH 2022.0426 ADDED CH-TYPE = "5" CHANGE COLLATERAL CODE #1551
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/CHMEMO.CBL A32 05/14/19-14:17:42 >".

       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       01  ERRCD                       PIC X     VALUE SPACES.

       01  TODAYS-DATE                 PIC 9(8).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MEMO-C-FROM                 PIC Z(05)9.
       01  FILLER                      REDEFINES MEMO-C-FROM.
           03  MEMO-C-FROM-CHAR        PIC X(01)     OCCURS 6.

       01  MEMO-C-TO                   PIC Z(05)9.
       01  FILLER                      REDEFINES MEMO-C-TO.
           03  MEMO-C-TO-CHAR          PIC X(01)     OCCURS 6.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MEMO-D-FROM                 PIC 99/99/99.
       01  MEMO-D-TO                   PIC 99/99/99.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MEMO-DATE-FIX               PIC X(18)     VALUE
                                                 "DATE CHANGE, FROM ".

       01  MEMO-FIX                    PIC X(20)     VALUE
                                                 "STATUS CHANGE, FROM ".

       78  WS-QUOTE VALUE  X'22'.
       01  RV-MESG.
           03  FILLER                  PIC X(19) VALUE
               "REVERSAL POSTED BY ".
           03  D-RV-USERID             PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(04) VALUE " AT ".
           03  D-RV-HOUR               PIC 9(02).
           03  FILLER                  PIC X(01) VALUE ":".
           03  D-RV-MIN                PIC 9(02).
           03  FILLER                  PIC X(01) VALUE ":".
           03  D-RV-SEC                PIC 9(02).
           03  D-RV-TZ                 PIC X(03) VALUE SPACES.
           03  FILLER                  PIC X(14) VALUE SPACES.


       01  ALLOT-MESG.
           03  FILLER                  PIC X(20) VALUE
               "ALLOTMENT PLAN FROM ".
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  D-OLD-ALLOTCD           PIC X(01) VALUE SPACE.
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  FILLER                  PIC X(04) VALUE " TO ".
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  D-NEW-ALLOTCD           PIC X(01) VALUE SPACE.
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  FILLER                  PIC X(20) VALUE
           ";LOCK BOX PLAN FROM ".
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  D-OLD-LBOX              PIC X(01) VALUE SPACE.
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  FILLER                  PIC X(04) VALUE " TO ".
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  D-NEW-LBOX              PIC X(01) VALUE SPACE.
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MEMO-FIX2                   PIC X(04)     VALUE " TO ".
       01  MEMO-FROM-FIX               PIC X(05)     VALUE "FROM ".
       01  MEMO-FROZEN-FROM            PIC X(01).
       01  MEMO-FROZEN-TO              PIC X(01).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MEMO-LN-ORIGIN-FROM         PIC X(4).
       01  MEMO-LN-ORIGIN-TO           PIC X(4).
       01  MEMO-LN-SRCD-FROM           PIC X(2).
       01  MEMO-LN-SRCD-TO             PIC X(2).

       01  BANKRUPT-MEMO.
           03  BANKRUPT-USERID         PIC X(8).
           03  FILLER                  PIC X.
           03  BANKRUPT-DESCRIPT       PIC X(20).
           03  FILLER                  PIC X.
           03  BANKRUPT-ORIG           PIC X(10).
           03  FILLER                  PIC X.
           03  BANKRUPT-TO             PIC X(4).
           03  FILLER                  PIC X.
           03  BANKRUPT-NEW            PIC X(10).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MEMO-TYPE                   PIC X(20)     VALUE SPACES.
       01  MEMO-USERID                 PIC X(10)     VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MEMO-LEFT-JUST-1            PIC X(06).
       01  FILLER                      REDEFINES MEMO-LEFT-JUST-1.
           03  MEMO-LEFT-1-CHAR        PIC X(01)     OCCURS 6.

       01  MEMO-LEFT-JUST-2            PIC X(06).
       01  FILLER                      REDEFINES MEMO-LEFT-JUST-2.
           03  MEMO-LEFT-2-CHAR        PIC X(01)     OCCURS 6.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  SAVE-LM1-KEY                 VALUE ZEROES.
           03  SAVE-LM-BRNO            PIC 9(4).
           03  SAVE-LM-ACCTNO          PIC 9(8).
           03  SAVE-LM-DATE            PIC 9(8).
           03  SAVE-LM-SEQ             PIC 9(3).


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01   ESCROW-MEMO.
            03  FILLER             PIC X(25) VALUE
                "ESCROW FLAG CHANGED FROM ".
            03  ESCROW-MEMO-FROM   PIC X.
            03  FILLER             PIC X(4)  VALUE " TO ".
            03  ESCROW-MEMO-TO     PIC X.
            03  FILLER             PIC X(2)  VALUE ", ".
            03  ESCROW-MEMO-USERID PIC X(8).
            03  FILLER             PIC X(1).
            03  ESCROW-MEMO-DATE   PIC 99/99/99.
            03  FILLER             PIC X(1).
            03  ESCROW-MEMO-TIME   PIC X(8).
            03  FILLER             PIC X(1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  TDR-MEMO.
           03  D-TDR-USERID            PIC X(8).
           03  FILLER                  PIC X   VALUE SPACES.
           03  FILLER                  PIC X(03) VALUE
               "TDR".
           03  D-TDR-1-2               PIC X(01).
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  D-OLD-TDR               PIC X(01) VALUE SPACE.
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  FILLER                  PIC X(04) VALUE " TO ".
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  D-NEW-TDR               PIC X(01).
           03  FILLER                  PIC X(01) VALUE WS-QUOTE.
           03  FILLER                  PIC X(01).
           03  D-OLD-TDR-DATE          PIC 99/99/99.
           03  FILLER                  PIC X(4) VALUE " TO ".
           03  D-NEW-TDR-DATE          PIC 99/99/99.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  SUBSCRIPTS-WORKERS          VALUE ZEROES.
           03  SUB                     PIC 9(01).
           03  SUB-FROM                PIC 9(01).
           03  SUB-TO                  PIC 9(01).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      *=================================================================
      *     L I N K A G E - S E C T I O N
      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01CH.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME CH-REC EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.
           PERFORM PRIMARY-DRIVER-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM GETDAT.
           MOVE SYS-DATE TO TODAYS-DATE.

           MOVE CH-BRNO       TO SAVE-LM-BRNO.
           MOVE CH-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - TODAYS-DATE.

           MOVE 1 TO SAVE-LM-SEQ.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       PRIMARY-DRIVER-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-LM1-FILE.

       CREATE-MEMO-LOOP.

           MOVE SPACES       TO LM-REC.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE CH-USERID    TO MEMO-USERID.

      *- - - - - - - - - - - - - - - - - - - - - -{ CREATE MEMO RECORD }
      *    IF ( CH-CODE = "A" )  DEALER NUMBER
           IF ( CH-CODE = "B" )  PERFORM CREATE-MEMO-B.
           IF ( CH-CODE = "C" )  PERFORM CREATE-MEMO-C.
           IF ( CH-CODE = "D" )  PERFORM CREATE-MEMO-D.
           IF ( CH-CODE = "E" )  PERFORM CREATE-MEMO-E.
           IF ( CH-CODE = "F" )  PERFORM CREATE-MEMO-F.
           IF ( CH-CODE = "H" )  PERFORM CREATE-MEMO-H.
           IF ( CH-CODE = "K" )  PERFORM CREATE-MEMO-K.
           IF ( CH-CODE = "L" )  PERFORM CREATE-MEMO-L.
      *    IF ( CH-CODE = "M" )  MAINTENANCE CHANGES
           IF ( CH-CODE = "N" )  PERFORM CREATE-MEMO-N-5.
           IF ( CH-CODE = "O" )  PERFORM CREATE-MEMO-O.
           IF ( CH-CODE = "P" )  PERFORM CREATE-MEMO-P.
           IF ( CH-CODE = "R" )  PERFORM CREATE-MEMO-R.
      *    IF ( CH-CODE = "T" )  COLLATERAL TRAILERS
           IF ( CH-CODE = "W" )  PERFORM CREATE-MEMO-W.
           IF ( CH-CODE = "X" )  PERFORM CREATE-MEMO-X.
           IF ( CH-CODE = "Y" )  PERFORM CREATE-MEMO-Y.
           IF ( CH-CODE = "1" )  PERFORM CREATE-MEMO-1.
           IF ( CH-CODE = "2" )  PERFORM CREATE-MEMO-2.
           IF ( CH-CODE = "3" )  PERFORM CREATE-MEMO-3-4.
           IF ( CH-CODE = "4" )  PERFORM CREATE-MEMO-3-4.
           IF ( CH-CODE = "5" )  PERFORM CREATE-MEMO-N-5.

      * UPDATE 'LM' FILE 

           PERFORM WRITE-LM1-FILE.
           IF ( IO-FG NOT = 0 )
              IF ( SAVE-LM-SEQ < 999 )
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO CREATE-MEMO-LOOP.

           PERFORM CLOSE-LM1-FILE.

       PRIMARY-DRIVER-MODULE-EXIT.
           EXIT.

      ***********************
       CREATE-MEMO-B SECTION.
      ***********************

      * CHBANK AND CHBNK2 DO NOT USE THE "B" BANKRUPT LAYOUT IN LP01CH,
      * THEY USE THE REDEFINES FOR ALL THE MISC TYPES OF FIELDS, BUT 
      * IT STILL USES CH-CREDIT-ACCTNO BECAUSE THIS PROGRAM NEEDED IT.
      * HAD TO BUILD A WORKING STORAGE MEMO SO I COULD FIT ALL THE
      * FIELDS INTO THE 60 BYTE MEMO, THE STRING COMMAND WAS NOT
      * WORKING WITH THE GENERIC X(24) CH-ORG-MAINT AND NEW-MAINT FIELDS.

           MOVE SPACES TO BANKRUPT-MEMO.

           MOVE CH-USERID     TO BANKRUPT-USERID.
           MOVE CH-DESCRIPT   TO BANKRUPT-DESCRIPT.
           MOVE CH-ORG-CHAR   TO BANKRUPT-ORIG.
           MOVE MEMO-FIX2     TO BANKRUPT-TO.
           MOVE CH-NEW-CHAR   TO BANKRUPT-NEW.

           MOVE BANKRUPT-MEMO TO LM-MEMO.
           MOVE CH-CODE       TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-C SECTION.
      ***********************
           IF CH-DESCRIPT = "DUAL LOAN OVERRIDE" OR "CEASE & DESIST"
              MOVE CH-DESCRIPT TO MEMO-TYPE
           ELSE
              MOVE "CONFIDENTIAL        " TO MEMO-TYPE.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-FIX,
                  CH-ORG-CHAR,
                  MEMO-FIX2,
                  CH-NEW-CHAR
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9 " TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-D SECTION.
      ***********************

           MOVE "DEBT-AID            " TO MEMO-TYPE.

           MOVE CH-ORG-NODEC TO MEMO-D-FROM.
           MOVE CH-NEW-NODEC TO MEMO-D-TO.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-DATE-FIX,
                  MEMO-D-FROM,
                  MEMO-FIX2,
                  MEMO-D-TO
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE CH-CODE TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-E SECTION.
      ***********************

           MOVE "FREEZE ACCOUNT      " TO MEMO-TYPE.

           MOVE CH-ORG-CHAR TO MEMO-FROZEN-FROM.
           MOVE CH-NEW-CHAR TO MEMO-FROZEN-TO.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-FROM-FIX,
                  MEMO-FROZEN-FROM,
                  MEMO-FIX2,
                  MEMO-FROZEN-TO
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9 " TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-F SECTION.
      ***********************

           MOVE "CONFIRMATION        " TO MEMO-TYPE.

           MOVE CH-ORG-NODEC TO MEMO-D-FROM.
           MOVE CH-NEW-NODEC TO MEMO-D-TO.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-DATE-FIX,
                  MEMO-D-FROM,
                  MEMO-FIX2,
                  MEMO-D-TO
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9 " TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-H SECTION.
      ***********************

           MOVE "CREDIT LIMIT        " TO MEMO-TYPE.

           MOVE CH-ORG-NODEC TO MEMO-C-FROM.
           MOVE CH-NEW-NODEC TO MEMO-C-TO.
           MOVE 1             TO SUB-FROM SUB-TO.
           MOVE SPACES        TO MEMO-LEFT-JUST-1
                                 MEMO-LEFT-JUST-2.

           PERFORM LEFT-JUST VARYING SUB FROM 1 BY 1 UNTIL SUB > 6.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-FIX,
                  MEMO-LEFT-JUST-1,
                  MEMO-FIX2,
                  MEMO-LEFT-JUST-2
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.



      ***********************
       CREATE-MEMO-K SECTION.
      ***********************

           IF ( CH-TYPE-K-TRW-SKIP )
              MOVE "SKIP TRW            " TO MEMO-TYPE
           ELSE
           IF ( CH-TYPE-K-TRW-STATUS )
              MOVE "TRW STATUS          " TO MEMO-TYPE
           ELSE
           IF ( CH-TYPE-K-TRW-SPECIAL-CODE )
              MOVE "TRW SPECIAL CODE    " TO MEMO-TYPE
           ELSE
           IF ( CH-TYPE-K-TRW-B-CONSUMER )
              MOVE "TRW MAKER CONSUMER CODE" TO MEMO-TYPE
           ELSE
           IF ( CH-TYPE-K-TRW-J-CONSUMER )
              MOVE "TRW COMAKER CONSUMER CODE" TO MEMO-TYPE
           ELSE
           IF ( CH-TYPE-K-TRW-COMPLIANCE )
              MOVE "TRW COMPLIANCE CODE" TO MEMO-TYPE
           ELSE
              MOVE "TRW                 " TO MEMO-TYPE.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-FIX,
                  CH-ORG-CHAR,
                  MEMO-FIX2,
                  CH-NEW-CHAR
                  DELIMITED BY "   " INTO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-L SECTION.
      ***********************

           MOVE "LEGAL               " TO MEMO-TYPE.

           MOVE CH-ORG-NODEC TO MEMO-D-FROM.
           MOVE CH-NEW-NODEC TO MEMO-D-TO.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-DATE-FIX,
                  MEMO-D-FROM,
                  MEMO-FIX2,
                  MEMO-D-TO
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE CH-CODE TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-N-5 SECTION.
      ***********************
           IF ( CH-CODE = "5" )
              MOVE "SECURITIZATION POOL " TO MEMO-TYPE
           ELSE
              MOVE "LOAN ORIGIN         " TO MEMO-TYPE.

           MOVE CH-ORG-CHAR  TO MEMO-LN-ORIGIN-FROM.
           MOVE CH-NEW-CHAR  TO MEMO-LN-ORIGIN-TO.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-FROM-FIX,
                  MEMO-LN-ORIGIN-FROM,
                  MEMO-FIX2,
                  MEMO-LN-ORIGIN-TO
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-O SECTION.
      ***********************
           MOVE CH-ORG-CHAR      TO ESCROW-MEMO-FROM.
           MOVE CH-NEW-CHAR      TO ESCROW-MEMO-TO.
           MOVE CH-USERID        TO ESCROW-MEMO-USERID.
           MOVE TODAYS-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO ESCROW-MEMO-DATE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT        TO ESCROW-MEMO-TIME.
           MOVE ESCROW-MEMO      TO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-P SECTION.
      ***********************

           MOVE "CHARGE OFF REASON      " TO MEMO-TYPE.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-FIX,
                  CH-ORG-CHAR,
                  MEMO-FIX2,
                  CH-NEW-CHAR
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-R SECTION.
      ***********************

           MOVE CH-USERID      TO D-RV-USERID.
           MOVE CH-HR          TO D-RV-HOUR.
           MOVE CH-MIN         TO D-RV-MIN.
           MOVE CH-SEC         TO D-RV-SEC.
           ACCEPT D-RV-TZ      FROM ENVIRONMENT "TZ".

           CALL "C$TOLOWER" USING D-RV-TZ, VALUE 3.
           ACCEPT D-RV-USERID FROM ENVIRONMENT "LOGNAME".

      *    IF CH-DESCRIPT = "BACKDATE"
      *       MOVE BACKDATE-MESG        TO LM-MEMO
      *       MOVE "P" TO LM-MEMO-TYPE
      *    ELSE
              MOVE RV-MESG        TO LM-MEMO
              MOVE CH-CODE TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-X SECTION.
      ***********************

           MOVE "REPOSSESS           " TO MEMO-TYPE.

           MOVE CH-ORG-NODEC TO MEMO-D-FROM.
           MOVE CH-NEW-NODEC TO MEMO-D-TO.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-DATE-FIX,
                  MEMO-D-FROM,
                  MEMO-FIX2,
                  MEMO-D-TO
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE CH-CODE TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-W SECTION.
      ***********************

           MOVE "WCBC CREDIT LIMIT   " TO MEMO-TYPE.

           MOVE CH-ORG-NODEC TO MEMO-C-FROM.
           MOVE CH-NEW-NODEC TO MEMO-C-TO.
           MOVE 1             TO SUB-FROM SUB-TO.
           MOVE SPACES        TO MEMO-LEFT-JUST-1
                                 MEMO-LEFT-JUST-2.

           PERFORM LEFT-JUST VARYING SUB FROM 1 BY 1 UNTIL SUB > 6.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-FIX,
                  MEMO-LEFT-JUST-1,
                  MEMO-FIX2,
                  MEMO-LEFT-JUST-2
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-Y SECTION.
      ***********************

           MOVE "ALLOTMENT/LBOX      " TO MEMO-TYPE.

           MOVE CH-ORG-ALLOTCD TO D-OLD-ALLOTCD.
           MOVE CH-NEW-ALLOTCD TO D-NEW-ALLOTCD.
           MOVE CH-ORG-LBOX    TO D-OLD-LBOX.
           MOVE CH-NEW-LBOX    TO D-NEW-LBOX.
           MOVE ALLOT-MESG     TO LM-MEMO.

           MOVE CH-CODE TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-1 SECTION.
      ***********************

           MOVE "GROSS INCOME CHANGE, " TO MEMO-TYPE.

           MOVE CH-ORG-NODEC TO MEMO-C-FROM.
           MOVE CH-NEW-NODEC TO MEMO-C-TO.
           MOVE 1             TO SUB-FROM SUB-TO.
           MOVE SPACES        TO MEMO-LEFT-JUST-1
                                 MEMO-LEFT-JUST-2.

           PERFORM LEFT-JUST VARYING SUB FROM 1 BY 1 UNTIL SUB > 6.

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  "FROM ",
                  MEMO-LEFT-JUST-1,
                  MEMO-FIX2,
                  MEMO-LEFT-JUST-2
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-2 SECTION.
      ***********************
           MOVE CH-USERID       TO D-TDR-USERID.
           MOVE CH-TDR-1-2      TO D-TDR-1-2.
           MOVE CH-ORG-TDR      TO D-OLD-TDR.
           MOVE CH-ORG-TDR-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-OLD-TDR-DATE.
           MOVE CH-NEW-TDR      TO D-NEW-TDR.
           MOVE CH-NEW-TDR-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-NEW-TDR-DATE.
           MOVE TDR-MEMO        TO LM-MEMO.
           MOVE CH-CODE         TO LM-MEMO-TYPE.

      ***********************
       CREATE-MEMO-3-4 SECTION.
      ***********************
           IF CH-CODE = "3"
              MOVE "LOAN SRCD CODE      " TO MEMO-TYPE
           ELSE
              MOVE "COLLATERAL CODE     " TO MEMO-TYPE.

           MOVE CH-ORG-CHAR       TO MEMO-LN-SRCD-FROM.
           MOVE CH-NEW-CHAR       TO MEMO-LN-SRCD-TO.   

           STRING MEMO-USERID," ",
                  MEMO-TYPE," ",
                  MEMO-FROM-FIX,
                  MEMO-LN-SRCD-FROM,
                  MEMO-FIX2,
                  MEMO-LN-SRCD-TO
                  DELIMITED BY "  " INTO LM-MEMO.

           MOVE "9" TO LM-MEMO-TYPE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ***********************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ***********************************
       VDU-CONVERT-FIELD.
       SETUP-LINK-INFO.
       FORM-RESET.

       LEFT-JUST.

           IF ( MEMO-C-FROM-CHAR(SUB) NOT = SPACES )
              MOVE MEMO-C-FROM-CHAR(SUB) TO MEMO-LEFT-1-CHAR(SUB-FROM)
              ADD 1 TO SUB-FROM.

           IF ( MEMO-C-TO-CHAR(SUB) NOT = SPACES )
              MOVE MEMO-C-TO-CHAR(SUB) TO MEMO-LEFT-2-CHAR(SUB-TO)
              ADD 1 TO SUB-TO.

      *********************
       IO-ROUTINES SECTION.
      *********************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LM1-FILE.
       COPY "LIBLP/LPLMGS_SQL.CPY".
      *********************
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-PROGRAM.

           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
